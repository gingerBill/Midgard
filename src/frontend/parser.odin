package wasm_frontend

import "core:mem"
import "core:fmt"
import "intrinsics"

Parser :: struct {
	file: ^File,
	tok: Tokenizer,

	err: Error_Handler,

	prev_tok: Token,
	curr_tok: Token,

	expr_level: int,

	sync_pos:   Pos,
	sync_count: int,

	error_count: int,

	node_allocator: mem.Allocator,
}

MAX_SYNC_COUNT :: 10;

Parse_Spec_Proc :: #type proc(p: ^Parser, keyword: Token, index: int, check_semicolon: bool) -> ^Ast_Spec;

parser_error :: proc(p: ^Parser, pos: Pos, msg: string, args: ..any) {
	if p.err != nil {
		p.err(pos, msg, ..args);
	}
	p.file.syntax_error_count += 1;
	p.error_count += 1;
}

new_node :: proc(p: ^Parser, $T: typeid) -> ^T {
	if p.node_allocator.procedure == nil {
		p.node_allocator = context.allocator;
	}
	n := new(T, p.node_allocator);
	n.variant = n;
	return n;
}

unparen :: proc(expr: ^Ast_Expr) -> ^Ast_Expr {
	x := expr;
	for {
		if p, is_paren := x.variant.(^Ast_Paren_Expr); is_paren {
			x = p.expr;
		} else {
			break;
		}
	}
	return x;
}


end_pos :: proc(tok: Token) -> Pos {
	pos := tok.pos;
	pos.offset += len(tok.text);

	if tok.kind == .Comment {
		if tok.text[:2] != "/*" {
			pos.column += u32(len(tok.text));
		} else {
			for i := 0; i < len(tok.text); i += 1 {
				c := tok.text[i];
				if c == '\n' {
					pos.line += 1;
					pos.column = 1;
				} else {
					pos.column += 1;
				}
			}
		}
	} else {
		pos.column += u32(len(tok.text));
	}
	return pos;
}
safe_pos :: proc(p: ^Parser, pos: Pos) -> Pos {
	if 0 <= pos.offset && pos.offset < len(p.file.src) {
		return pos;
	}
	return Pos{
		file = p.file,
		offset = len(p.file.src),
		line = u32(p.tok.line_count),
		column = 1,
	};
}


next_token0 :: proc(p: ^Parser) -> bool {
	p.curr_tok = scan(&p.tok);
	if p.curr_tok.kind == .EOF {
		return false;
	}
	return true;
}

consume_comment :: proc(p: ^Parser) -> (tok: Token, end_line: int) {
	tok = p.curr_tok;
	assert(tok.kind == .Comment);
	end_line = int(tok.pos.line);

	if tok.text[1] == '*' {
		for c in tok.text {
			if c == '\n' {
				end_line += 1;
			}
		}
	}

	_ = next_token0(p);
	if p.curr_tok.pos.line > tok.pos.line {
		end_line += 1;
	}

	return;
}


consume_comment_group :: proc(p: ^Parser, n: int) -> (end_line: int) {
	end_line = int(p.curr_tok.pos.line);
	for p.curr_tok.kind == .Comment &&
	    int(p.curr_tok.pos.line) <= end_line+n {
	    comment: Token;
		comment, end_line = consume_comment(p);
	}
	return;
}

consume_comment_groups :: proc(p: ^Parser, prev: Token) {
	if p.curr_tok.kind == .Comment {
		end_line := 0;

		if p.curr_tok.pos.line == prev.pos.line {
			end_line = consume_comment_group(p, 0);
		}

		end_line = -1;
		for p.curr_tok.kind == .Comment {
			end_line = consume_comment_group(p, 1);
		}
		assert(p.curr_tok.kind != .Comment);
	}
}


advance_token :: proc(p: ^Parser) -> Token {
	p.prev_tok = p.curr_tok;
	prev := p.prev_tok;
	if next_token0(p) {
		consume_comment_groups(p, prev);
	}
	return prev;
}

peek_token :: proc(p: ^Parser, amount: uint) -> Token_Kind {
	prev_state := p.tok.state;
	prev_curr_tok := p.curr_tok;
	tok := prev_curr_tok;
	defer {
		p.tok.state = prev_state;
		p.curr_tok = prev_curr_tok;
	}

	for i in 0..<amount {
		prev := p.curr_tok;
		if !next_token0(p) {
			break;
		}
		consume_comment_groups(p, prev);
		tok = p.curr_tok;
	}

	return tok.kind;
}

parser_expect_error :: proc(p: ^Parser, pos: Pos, msg: string) {
	g := to_string(p.curr_tok.kind);
	if p.curr_tok.text == "\n" {
		g = "newline";
	}
	parser_error(p, pos, "expected {}, got {}", msg, g);
}

expect_token :: proc(p: ^Parser, kind: Token_Kind) -> Token {
	prev := p.curr_tok;
	if prev.kind != kind {
		e := to_string(kind);
		g := to_string(prev.kind);
		if prev.text == "\n" {
			g = "newline";
		}
		parser_error(p, prev.pos, "expected '{}', got '{}'", e, g);
	}
	advance_token(p);
	return prev;
}

expect_token_after :: proc(p: ^Parser, kind: Token_Kind, msg: string) -> Token {
	prev := p.curr_tok;
	if prev.kind != kind {
		e := to_string(kind);
		g := to_string(prev.kind);
		if prev.text == "\n" {
			g = "newline";
		}
		parser_error(p, prev.pos, "expected '{}' after {}, got '{}'", e, msg, g);
	}
	advance_token(p);
	return prev;
}

expect_closing :: proc(p: ^Parser, kind: Token_Kind, ctx: string) -> Token {
	prev := p.curr_tok;
	if prev.kind != kind && prev.kind == .Semicolon && prev.text == "\n" {
		parser_error(p, prev.pos, "missing ',' before newline in {}", ctx);
		advance_token(p);
	}
	return expect_token(p, kind);
}


expect_semicolon :: proc(p: ^Parser) {
	if p.curr_tok.kind != .Close_Brace || p.curr_tok.kind != .Close_Paren {
		#partial switch p.curr_tok.kind {
		case .Semicolon:
			advance_token(p);
		case .Comma:
			parser_error(p, p.curr_tok.pos, "expected ';'");
			advance_token(p);
		case:
			parser_error(p, p.curr_tok.pos, "expected ';'");
			parser_fix_sync(p, &_stmt_start_map);
		}
	}
}

allow_token :: proc(p: ^Parser, kind: Token_Kind) -> bool {
	if p.curr_tok.kind == kind {
		advance_token(p);
		return true;
	}
	return false;
}


is_blank_ident :: proc{
	is_blank_ident_string,
	is_blank_ident_token,
	is_blank_ident_node,
};
is_blank_ident_string :: inline proc(str: string) -> bool {
	return str == "_";
}
is_blank_ident_token :: inline proc(tok: Token) -> bool {
	if tok.kind == .Ident {
		return is_blank_ident_string(tok.text);
	}
	return false;
}
is_blank_ident_node :: inline proc(node: ^Ast_Expr) -> bool {
	if ident, ok := node.variant.(^Ast_Ident); ok {
		return is_blank_ident(ident.name);
	}
	return true;
}







parse_file :: proc(p: ^Parser, file: ^File) -> bool {
	assert(file != nil);

	{
		p.prev_tok = {};
		p.curr_tok = {};
		p.expr_level = 0;
	}

	p.file = file;
	tokenizer_init(&p.tok, file);
	if p.tok.ch <= 0 {
		return true;
	}

	advance_token(p);
	consume_comment_groups(p, p.prev_tok);

	if p.file.syntax_error_count > 0 {
		return false;
	}

	assert(p.file.decls == nil);
	p.file.decls = make([dynamic]^Ast_Decl);

	for p.curr_tok.kind != .EOF {
		decl := parse_decl(p, &_decl_start_map);
		append(&p.file.decls, decl);
	}
	return true;
}


parse_decl :: proc(p: ^Parser, sync: ^[Token_Kind]bool) -> ^Ast_Decl {
	f: Parse_Spec_Proc;
	#partial switch p.curr_tok.kind {
	case .Import:
		f = parse_import_spec;
	case .Const, .Var:
		f = parse_value_spec;
	case .Type:
		f = parse_type_spec;
	case .Proc:
		return parse_proc_decl(p);
	case:
		tok := p.curr_tok;
		parser_expect_error(p, tok.pos, "declaration");
		parser_fix_sync(p, sync);
		bad := new_node(p, Ast_Bad_Decl);
		bad.pos = tok.pos;
		bad.end = safe_pos(p, p.curr_tok.pos);
		return bad;
	}
	assert(f != nil);
	return parse_gen_decl(p, p.curr_tok.kind, f);
}


parse_gen_decl :: proc(p: ^Parser, keyword: Token_Kind, f: Parse_Spec_Proc, allow_grouping := true) -> ^Ast_Gen_Decl {
	tok := expect_token(p, keyword);
	open, close: Token;
	specs := make([dynamic]^Ast_Spec, 0, 0);

	if allow_grouping && p.curr_tok.kind == .Open_Paren {
		open = expect_token(p, .Open_Paren);
		for i := 0; p.curr_tok.kind != .Close_Paren && p.curr_tok.kind != .EOF; i += 1 {
			append(&specs, f(p, tok, i, true));
		}
		close = expect_token(p, .Close_Paren);
		expect_semicolon(p);
	} else {
		reserve(&specs, 1);
		append(&specs, f(p, tok, 0, allow_grouping));
	}

	decl := new_node(p, Ast_Gen_Decl);
	decl.pos = tok.pos;
	decl.end = end_pos(close) if close.pos.line > 0 else specs[len(specs)-1].end;

	decl.tok = tok;
	decl.open_paren = open.pos;
	decl.specs = specs[:];
	decl.close_paren = close.pos;

	return decl;
}


parse_import_spec :: proc(p: ^Parser, keyword: Token, _: int, _: bool) -> ^Ast_Spec {
	name: ^Ast_Ident;
	if p.curr_tok.kind == .Ident {
		name = parse_ident(p);
	}


	path_tok := p.curr_tok;
	path: string;
	if path_tok.kind == .String {
		path = path_tok.text;
		advance_token(p);
	} else {
		expect_token(p, .String);
	}
	expect_semicolon(p);

	path_node := new_node(p, Ast_Basic_Lit);
	path_node.pos = path_tok.pos;
	path_node.end = end_pos(path_tok);
	path_node.tok = path_tok;

	spec := new_node(p, Ast_Import_Spec);
	spec.pos = keyword.pos;
	spec.end = end_pos(path_tok);
	spec.name = name;
	spec.path = path_node;

	return spec;
}

parse_value_spec :: proc(p: ^Parser, keyword: Token, index: int, check_semicolon := true) -> ^Ast_Spec {
	start := p.curr_tok;
	pos := p.curr_tok.pos;
	names := parse_ident_list(p);
	type := parse_try_type(p);
	values: []^Ast_Expr;

	if allow_token(p, .Assign) {
		values = parse_rhs_list(p);
	}
	if check_semicolon {
		expect_semicolon(p);
	}

	#partial switch keyword.kind {
	case .Var:
		if type == nil && values == nil {
			parser_error(p, pos, "missing variable type or initialization");
		}
	case .Const:
		if values == nil && type != nil {
			parser_error(p, pos, "missing constant value");
		}
	}

	spec := new_node(p, Ast_Value_Spec);
	spec.pos = keyword.pos;
	spec.end = end_pos(start);
	if values != nil {
		spec.end = values[len(values)-1].end;
	} else if type != nil {
		spec.end = type.end;
	} else if names != nil {
		spec.end = names[len(names)-1].end;
	}

	spec.keyword = keyword;
	spec.names = names;
	spec.type = type;
	spec.values = values;

	return spec;
}

parse_type_spec :: proc(p: ^Parser, keyword: Token, index: int, _: bool) -> ^Ast_Spec {
	start := p.curr_tok;
	pos := p.curr_tok.pos;
	
	spec := new_node(p, Ast_Type_Spec);
	spec.pos = pos;
	spec.name = parse_ident(p);

	if p.curr_tok.kind == .Assign {
		spec.assign = advance_token(p).pos;
	}
	spec.type = parse_type(p);
	spec.end = spec.type.end;

	expect_semicolon(p);

	return spec;
}

parse_proc_decl :: proc(p: ^Parser) -> ^Ast_Proc_Decl {
	tok := expect_token(p, .Proc);
	pos := tok.pos;

	name := parse_ident(p);

	params, results := parse_signature(p);

	body: ^Ast_Block_Stmt;
	if p.curr_tok.kind == .Open_Brace {
		body = parse_body(p);
		expect_semicolon(p);
	} else if p.curr_tok.kind == .Semicolon {
		expect_semicolon(p);
		if p.curr_tok.kind == .Open_Brace {
			parser_error(p, p.curr_tok.pos, "unexpected semicolon or newline before {");
			body = parse_body(p);
			expect_semicolon(p);
		}
	} else {
		expect_semicolon(p);
	}

	type := new_node(p, Ast_Proc_Type);
	type.pos = tok.pos;
	type.end = results.end if results != nil else params.end;
	type.tok = tok;
	type.params = params;
	type.results = results;

	decl := new_node(p, Ast_Proc_Decl);
	decl.pos = pos;
	decl.end = body.end if body != nil else type.end;
	decl.name = name;
	decl.type = type;
	decl.body = body;

	return decl;
}


parse_stmt_list :: proc(p: ^Parser) -> []^Ast_Stmt {
	list: [dynamic]^Ast_Stmt;
	for p.curr_tok.kind != .Close_Brace && p.curr_tok.kind != .Case && p.curr_tok.kind != .EOF {
		if stmt := parse_stmt(p); stmt != nil {
			append(&list, stmt);
		}
	}
	return list[:];
}


parse_stmt :: proc(p: ^Parser) -> ^Ast_Stmt {
	#partial switch p.curr_tok.kind {
	case .Proc:
		if peek_token(p, 1) == .Ident {
			decl := parse_proc_decl(p);
			s := new_node(p, Ast_Decl_Stmt);
			s.pos, s.end = decl.pos, s.end;
			s.decl = decl;
			return s;
		}
		fallthrough;

	case .Ident, .Integer, .Float, .Rune, .String, .Open_Paren,
	     .Open_Bracket, .Struct, .Pointer,
	     .Add, .Sub, .And, .Xor, .Not:
		s := parser_simple_stmt(p, .Label_Ok);
		if _, ok := s.variant.(^Ast_Labeled_Stmt); !ok {
			expect_semicolon(p);
		}
		return s;

	case .Var, .Const, .Type:
		decl := parse_decl(p, &_stmt_start_map);
		s := new_node(p, Ast_Decl_Stmt);
		s.pos, s.end = decl.pos, s.end;
		s.decl = decl;
		return s;


	case .Open_Brace:
		s := parse_block_stmt(p);
		expect_semicolon(p);
		return s;

	case .Break, .Continue, .Fallthrough:
		tok := advance_token(p);
		label: ^Ast_Ident;
		if tok.kind != .Fallthrough && p.curr_tok.kind == .Ident {
			label = parse_ident(p);
		}
		expect_semicolon(p);

		stmt := new_node(p, Ast_Branch_Stmt);
		stmt.pos = tok.pos;
		stmt.end = label.end if label != nil else end_pos(tok);
		stmt.tok = tok;
		stmt.label = label;
		return stmt;

	case .Defer:
		tok := advance_token(p);
		stmt := parse_stmt(p);
		if inner, ok := stmt.variant.(^Ast_Defer_Stmt); ok {
			parser_error(p, inner.pos, "illegal nested defer statement");
			stmt = inner.stmt;
		}
		s := new_node(p, Ast_Defer_Stmt);
		s.pos = tok.pos;
		s.end = stmt.end;
		s.tok = tok;
		s.stmt = stmt;
		return s;

	case .If:
		return parse_if_stmt(p);

	case .Switch:
		return parse_switch_stmt(p);
		
	case .For:
		return parse_for_stmt(p);
		
	case .Return:
		tok := expect_token(p, .Return);
		results: []^Ast_Expr;
		if p.curr_tok.kind != .Semicolon && p.curr_tok.kind != .Close_Brace {
			results = parse_rhs_list(p);
		}
		expect_semicolon(p);
		s := new_node(p, Ast_Return_Stmt);
		s.pos = tok.pos;
		s.end = results[len(results)-1].end if len(results) != 0 else end_pos(tok);
		s.tok_pos = tok.pos;
		s.results = results;
		return s;

	case .Semicolon: // Handle implicit semicolons regardless
		s := new_node(p, Ast_Empty_Stmt);
		s.pos, s.end = p.curr_tok.pos, end_pos(p.curr_tok);
		s.semicolon = p.curr_tok.pos;
		s.implicit = p.curr_tok.text == "\n";
		advance_token(p);
		return s;

	case .Close_Brace: // Semicolon can be ommited before a closing '}'
		s := new_node(p, Ast_Empty_Stmt);
		s.pos, s.end = p.curr_tok.pos, end_pos(p.curr_tok);
		s.semicolon = p.curr_tok.pos;
		s.implicit = true;
		return s;

	case:
		pos := p.curr_tok.pos;
		parser_error(p, pos, "expected statement");
		parser_fix_sync(p, &_stmt_start_map);
		s := new_node(p, Ast_Bad_Stmt);
		s.pos = pos;
		s.end = p.curr_tok.pos;
		return s;
	}
	return nil;
}


parse_if_stmt :: proc(p: ^Parser) -> ^Ast_If_Stmt {
	tok := expect_token(p, .If);

	init: ^Ast_Stmt = nil;
	cond: ^Ast_Expr = nil;
	header: {
		if p.curr_tok.kind == .Open_Brace {
			parser_error(p, p.curr_tok.pos, "missing condition in if statement");
			break header;
		}

		prev_level := p.expr_level;
		defer p.expr_level = prev_level;
		p.expr_level = -1;

		if p.curr_tok.kind != .Semicolon {
			init = parser_simple_stmt(p, .Normal);
		}

		cond_stmt: ^Ast_Stmt;
		semi: Token;

		if p.curr_tok.kind != .Open_Brace {
			if p.curr_tok.kind == .Semicolon {
				semi = p.curr_tok;
				advance_token(p);
			} else {
				expect_semicolon(p);
			}
			if p.curr_tok.kind != .Open_Brace {
				cond_stmt = parser_simple_stmt(p, .Normal);
			}
		} else {
			cond_stmt = init;
			init = nil;
		}

		if cond_stmt != nil {
			cond = parser_conv_to_expr(p, cond_stmt, "boolean expression");
		} else if semi.pos.line > 0 {
			if semi.text == "\n" {
				parser_error(p, semi.pos, "unexpected newline, expected '{' after if clause");
			} else {
				parser_error(p, semi.pos, "missing condition in if statement");
			}
		}

		if cond == nil {
			be := new_node(p, Ast_Bad_Expr);
			be.pos = p.curr_tok.pos;
			be.end = end_pos(p.curr_tok);
			cond = be;
		}
	}

	body := parse_block_stmt(p);

	else_stmt: ^Ast_Stmt;
	if allow_token(p, .Else) {
		#partial switch p.curr_tok.kind {
		case .If:
			else_stmt = parse_if_stmt(p);
		case .Open_Brace:
			else_stmt = parse_block_stmt(p);
			expect_semicolon(p);
		case:
			parser_error(p, p.curr_tok.pos, "execpted if statement of block");
			bs := new_node(p, Ast_Bad_Stmt);
			bs.pos = p.curr_tok.pos;
			bs.end = end_pos(p.curr_tok);
			else_stmt = bs;
		}
	} else {
		expect_semicolon(p);
	}

	s := new_node(p, Ast_If_Stmt);
	s.pos = tok.pos;
	s.end = else_stmt.end if else_stmt != nil else body.end;
	s.if_pos = tok.pos;
	s.init = nil;
	s.cond = nil;
	s.body = body;
	s.else_stmt = else_stmt;

	return s;
}

parse_for_stmt :: proc(p: ^Parser) -> ^Ast_For_Stmt {
	tok := expect_token(p, .For);
	stmt1, stmt2, stmt3: ^Ast_Stmt;
	if p.curr_tok.kind != .Open_Brace {
		prev_level := p.expr_level;
		defer p.expr_level = prev_level;
		p.expr_level = -1;

		if p.curr_tok.kind != .Semicolon {
			stmt2 = parser_simple_stmt(p, .Normal);
		}
		if p.curr_tok.kind == .Semicolon {
			advance_token(p);
			stmt1 = stmt2;
			stmt2 = nil;
			if p.curr_tok.kind != .Semicolon {
				stmt2 = parser_simple_stmt(p, .Normal);
			}
			expect_semicolon(p);
			if p.curr_tok.kind != .Open_Brace {
				stmt3 = parser_simple_stmt(p, .Normal);
			}
		}
	}

	body := parse_block_stmt(p);
	expect_semicolon(p);

	s := new_node(p, Ast_For_Stmt);
	s.pos = tok.pos;
	s.end = body.end;
	s.init = stmt1;
	s.cond = parser_conv_to_expr(p, stmt2, "boolean expression");
	s.post = stmt3;
	s.body = body;
	return s;
}

parse_case_clause :: proc(p: ^Parser) -> ^Ast_Case_Clause {
	tok := expect_token(p, .Case);
	clauses: []^Ast_Expr;
	if p.curr_tok.kind != .Colon {
		clauses = parse_rhs_list(p);
	}
	colon := expect_token(p, .Colon);
	body := parse_stmt_list(p);

	c := new_node(p, Ast_Case_Clause);
	c.pos = tok.pos;
	c.end = body[len(body)-1].end if len(body) != 0 else end_pos(colon);
	c.tok = tok;
	c.clauses = clauses;
	c.colon = colon.pos;
	c.body = body;

	return c;
}

parse_switch_stmt :: proc(p: ^Parser) -> ^Ast_Switch_Stmt {
	tok := expect_token(p, .Switch);
	stmt1, stmt2: ^Ast_Stmt;

	if p.curr_tok.kind != .Open_Brace {
		prev_level := p.expr_level;
		defer p.expr_level = prev_level;
		p.expr_level = -1;

		if p.curr_tok.kind != .Semicolon {
			stmt2 = parser_simple_stmt(p, .Normal);
		}

		if p.curr_tok.kind == .Semicolon {
			advance_token(p);
			stmt1 = stmt2;
			stmt2 = nil;
			if p.curr_tok.kind != .Open_Brace {
				stmt2 = parser_simple_stmt(p, .Normal);
			}
		}
	}

	open := expect_token(p, .Open_Brace);
	clauses: [dynamic]^Ast_Stmt;

	for p.curr_tok.kind == .Case {
		append(&clauses, parse_case_clause(p));
	}

	close := expect_token(p, .Close_Brace);
	expect_semicolon(p);

	body := new_node(p, Ast_Block_Stmt);
	body.pos = open.pos;
	body.end = end_pos(close);
	body.open = open.pos;
	body.stmts = clauses[:];
	body.close = close.pos;

	s := new_node(p, Ast_Switch_Stmt);
	s.pos = tok.pos;
	s.end = body.end;
	s.init = stmt1;
	s.cond = parser_conv_to_expr(p, stmt2, "switch expression");
	s.body = body;

	return s;

}



Simple_Stmt_Mode :: enum {
	Normal,
	Label_Ok,
}

parser_simple_stmt :: proc(p: ^Parser, mode: Simple_Stmt_Mode) -> ^Ast_Stmt {
	#partial switch p.curr_tok.kind {
	case .Var, .Const:
		decl := parse_gen_decl(p, p.curr_tok.kind, parse_value_spec, false);
		s := new_node(p, Ast_Decl_Stmt);
		s.pos, s.end = decl.pos, s.end;
		s.decl = decl;
		return s;
	}	

	lhs := parse_lhs_list(p);

	#partial switch p.curr_tok.kind {
	case .Assign, 
	     .Add_Assign, .Sub_Assign, .Mul_Assign, .Quo_Assign, .Mod_Assign,
	     .And_Assign, .Or_Assign, .Xor_Assign, .Shl_Assign, .Shr_Assign, .And_Not_Assign:
		op := advance_token(p);
		rhs := parse_rhs_list(p);

		s := new_node(p, Ast_Assign_Stmt);
		s.pos = lhs[0].pos;
		s.end = rhs[len(rhs)-1].end;
		s.lhs = lhs;
		s.op  = op;
		s.rhs = rhs;
		return s;
	}

	if len(lhs) > 1 {
		parser_error(p, lhs[0].pos, "expected 1 expression, got {}", len(lhs));
	}

	if p.curr_tok.kind == .Colon {
		colon := advance_token(p);
		if label, ok := lhs[0].variant.(^Ast_Ident); mode == .Label_Ok && ok {
			stmt := parse_stmt(p);
			if inner, is_label := stmt.variant.(^Ast_Labeled_Stmt); is_label {
				parser_error(p, inner.pos, "illegal nested label declaration");
				stmt = inner.stmt;
			}

			s := new_node(p, Ast_Labeled_Stmt);
			s.pos = lhs[0].pos;
			s.end = stmt.end;
			s.colon = colon.pos;
			s.stmt = stmt;
			return s;
		}
		parser_error(p, colon.pos, "illegal label declaration");

		bs := new_node(p, Ast_Bad_Stmt);
		bs.pos = lhs[0].pos;
		bs.end = end_pos(colon);
		return bs;
	}

	es := new_node(p, Ast_Expr_Stmt);
	es.pos = lhs[0].pos;
	es.end = lhs[0].end;
	es.expr = lhs[0];

	return es;
}

parser_conv_to_expr :: proc(p: ^Parser, s: ^Ast_Stmt, want: string) -> ^Ast_Expr {
	if s == nil {
		return nil;
	}

	if es, ok := s.variant.(^Ast_Expr_Stmt); ok {
		return parser_check_expr(p, es.expr);
	}

	found := "simple statement";
	if _, ok := s.variant.(^Ast_Assign_Stmt); ok {
		found = "assignment";
	}
	parser_error(p, s.pos, "expected {}, found {}", want, found);
	be := new_node(p, Ast_Bad_Expr);
	be.pos = s.pos;
	be.end = s.end;
	return be;
}


parse_block_stmt :: proc(p: ^Parser) -> ^Ast_Block_Stmt {
	open := expect_token(p, .Open_Brace);
	stmts := parse_stmt_list(p);
	close := expect_token(p, .Close_Brace);

	body := new_node(p, Ast_Block_Stmt);
	body.pos = open.pos;
	body.end = end_pos(close);
	body.open = open.pos;
	body.close = close.pos;
	body.stmts = stmts;

	return body;
}

parse_body :: proc(p: ^Parser) -> ^Ast_Block_Stmt {
	return parse_block_stmt(p);
}

parse_signature :: proc(p: ^Parser) -> (params, results: ^Ast_Field_List) {
	params = parse_parameters(p);
	results = parse_result(p);
	return;
}


parse_parameters_list :: proc(p: ^Parser) -> []^Ast_Field {
	return nil;
}

parse_parameters :: proc(p: ^Parser) -> ^Ast_Field_List {
	list: []^Ast_Field;
	open := expect_token(p, .Open_Paren);

	if p.curr_tok.kind != .Close_Paren {
		list = parse_parameters_list(p);
	}

	close := expect_token(p, .Close_Paren);


	fields := new_node(p, Ast_Field_List);
	fields.pos = open.pos;
	fields.end = end_pos(close);
	fields.open = open.pos;
	fields.list = list;
	fields.close = close.pos;
	return fields;
}

parse_result :: proc(p: ^Parser) -> ^Ast_Field_List {
	if p.curr_tok.kind == .Open_Paren {
		return parse_parameters(p);	
	}

	type := parse_try_type(p);
	if type != nil {
		list := make([]^Ast_Field, 1);
		list[0] = new_node(p, Ast_Field);
		list[0].pos = type.pos;
		list[0].end = type.end;

		fields := new_node(p, Ast_Field_List);
		fields.pos = list[0].pos;
		fields.end = list[0].end;
		fields.list = list;
		return fields;
	}

	return nil;
}




parse_ident :: proc(p: ^Parser) -> ^Ast_Ident {
	tok := expect_token(p, .Ident);
	node := new_node(p, Ast_Ident);
	node.pos = tok.pos;
	node.end = end_pos(tok);
	node.name = tok.text;
	return node;
}


parse_ident_list :: proc(p: ^Parser) -> []^Ast_Ident {
	list := make([dynamic]^Ast_Ident, 0, 1);
	append(&list, parse_ident(p));
	for allow_token(p, .Comma) {
		append(&list, parse_ident(p));
	}
	return list[:];
}

parse_rhs :: proc(p: ^Parser) -> ^Ast_Expr {
	return parse_expr(p, false);
}

parse_lhs_list :: proc(p: ^Parser) -> []^Ast_Expr {
	return parse_expr_list(p, true);
}

parse_rhs_list :: proc(p: ^Parser) -> []^Ast_Expr {
	return parse_expr_list(p, false);
}

parse_expr_list :: proc(p: ^Parser, is_lhs: bool) -> []^Ast_Expr {
	list := make([dynamic]^Ast_Expr, 0, 1);
	append(&list, parser_check_expr(p, parse_expr(p, is_lhs)));
	for allow_token(p, .Comma) {
		append(&list, parser_check_expr(p, parse_expr(p, is_lhs)));
	}
	return list[:];
}

parse_expr :: proc(p: ^Parser, is_lhs: bool) -> ^Ast_Expr {
	return parse_binary_expr(p, is_lhs, 0+1);
}

parser_check_expr :: proc(p: ^Parser, expr: ^Ast_Expr) -> ^Ast_Expr {
	x := unparen(expr);
	#partial switch _ in x.variant {
	case ^Ast_Paren_Expr:
		unreachable();
	case ^Ast_Bad_Expr:
	case ^Ast_Ident:
	case ^Ast_Basic_Lit:
	case ^Ast_Comp_Lit:
	case ^Ast_Proc_Lit:
	case ^Ast_Selector_Expr:
	case ^Ast_Index_Expr:
	case ^Ast_Slice_Expr:
	case ^Ast_Call_Expr:
	case ^Ast_Unary_Expr:
	case ^Ast_Binary_Expr:
	case:
		parser_error(p, x.pos, "expected expression");
		bad := new_node(p, Ast_Bad_Expr);
		bad.pos = x.pos;
		bad.end = safe_pos(p, x.end);
		return bad;
	}
	return expr;
}

parser_check_expr_or_type :: proc(p: ^Parser, expr: ^Ast_Expr) -> ^Ast_Expr {
	x := unparen(expr);
	#partial switch _ in x.variant {
	case ^Ast_Paren_Expr:
		unreachable();
	}

	return expr;
}


parse_binary_expr :: proc(p: ^Parser, is_lhs: bool, prec_init: int) -> ^Ast_Expr {
	x := parse_unary_expr(p, is_lhs);
	for {
		op_prec := token_precedence(p.curr_tok.kind);
		if op_prec < prec_init {
			return x;
		}
		op := advance_token(p);

		y := parse_binary_expr(p, false, op_prec+1);
		be := new_node(p, Ast_Binary_Expr);
		be.left = parser_check_expr(p, x);
		be.op = op;
		be.right = parser_check_expr(p, y);
		x = be;
	}
	return x;
}

parse_unary_expr :: proc(p: ^Parser, is_lhs: bool) -> ^Ast_Expr {
	#partial switch p.curr_tok.kind {
	case .Add, .Sub, .Not, .Xor, .And:
		op := advance_token(p);
		x := parse_unary_expr(p, false);
		ue := new_node(p, Ast_Unary_Expr);
		ue.pos = op.pos;
		ue.end = x.end;
		ue.op = op;
		ue.expr = x;
		return ue;
	}
	return parse_atom_expr(p, is_lhs);
}

parser_at_comma :: proc(p: ^Parser, comment: string, follow: Token_Kind) -> bool {
	if p.curr_tok.kind == .Comma {
		return true;
	}
	if p.curr_tok.kind != follow {
		if p.curr_tok.kind == .Semicolon && p.curr_tok.text == "\n" {
			parser_error(p, p.curr_tok.pos, "missing ',' before newline");
		} else {
			parser_error(p, p.curr_tok.pos, "missing ','");
		}
		return true;
	}
	return false;

}

parse_atom_expr :: proc(p: ^Parser, is_lhs: bool) -> ^Ast_Expr {
	lhs := is_lhs;
	x := parse_operand(p, lhs);
	loop: for {
		#partial switch p.curr_tok.kind {
		case .Period:
			advance_token(p);
			#partial switch p.curr_tok.kind {
			case .Ident:
				sel := parse_ident(p);
				se := new_node(p, Ast_Selector_Expr);
				x = parser_check_expr(p, x);
				se.pos = x.pos;
				se.end = sel.end;
				se.x = x;
				se.sel = sel;
				x = se;

			case:
				tok := p.curr_tok;
				parser_error(p, tok.pos, "expected selector");
				advance_token(p);
				name := tok;
				name.kind = .Ident;
				name.text = "_";
				sel := new_node(p, Ast_Ident);
				sel.pos = name.pos;
				sel.end = end_pos(name);
				sel.name = name.text;

				se := new_node(p, Ast_Selector_Expr);
				se.pos = x.pos;
				se.end = sel.end;
				se.x = x;
				se.sel = sel;
				x = se;
			}

		case .Pointer:
			x = parser_check_expr(p, x);

			op := advance_token(p);
			de := new_node(p, Ast_Deref_Expr);
			de.pos = x.pos;
			de.end = end_pos(op);
			de.expr = x;
			de.op = op.pos;

			x = de;

		case .Open_Paren:
			x = parser_check_expr(p, x);

			open := expect_token(p, .Open_Paren);
			args := make([dynamic]^Ast_Expr, 0, 0);
			for p.curr_tok.kind != .Close_Paren && p.curr_tok.kind != .EOF {
				append(&args, parse_rhs_or_type(p));
				if !parser_at_comma(p, "argument list", .Close_Paren) {
					break;
				}
				advance_token(p);
			}
			close := expect_token(p, .Close_Paren);

			ce := new_node(p, Ast_Call_Expr);
			ce.pos = x.pos;
			ce.end = end_pos(close);
			ce.expr = x;
			ce.open = open.pos;
			ce.args = args[:];
			ce.close = close.pos;

			x = ce;

		case .Open_Bracket:
			x = parser_check_expr(p, x);

			N :: 2;
			open := expect_token(p, .Open_Bracket);
			p.expr_level += 1;
			index: [2]^Ast_Expr;
			colon: Token;
			colon_count := 0;

			if p.curr_tok.kind != .Colon {
				index[0] = parse_rhs(p);
			}
			if p.curr_tok.kind == .Colon {
				colon = advance_token(p);
				colon_count += 1;
				if p.curr_tok.kind != .Close_Bracket && p.curr_tok.kind != .EOF {
					index[colon_count] = parse_rhs(p);
				}
			}

			p.expr_level -= 1;
			close := expect_token(p, .Close_Bracket);

			if colon_count > 0 {
				ie := new_node(p, Ast_Slice_Expr);
				ie.pos = x.pos;
				ie.end = end_pos(close);
				ie.x = x;
				ie.open = open.pos;
				ie.low  = index[0];
				ie.high = index[1];
				ie.close = close.pos;

				x = ie;
			} else {
				ie := new_node(p, Ast_Index_Expr);
				ie.pos = x.pos;
				ie.end = end_pos(close);
				ie.x = x;
				ie.open = open.pos;
				ie.index = index[0];
				ie.close = close.pos;

				x = ie;
			}

		case .Open_Brace:
			if is_literal_type(x) && (p.expr_level >= 0) {
				x = parse_literal_value(p, x);
			} else {
				break loop;
			}
		case: 
			break loop;
		}
		lhs = false;
	}

	return x;
}

parse_operand :: proc(p: ^Parser, is_lhs: bool) -> ^Ast_Expr {
	#partial switch p.curr_tok.kind {
	case .Ident:
		return parse_ident(p);
	case .Integer, .Float, .Rune, .String:
		tok := p.curr_tok;
		advance_token(p);

		x := new_node(p, Ast_Basic_Lit);
		x.pos = tok.pos;
		x.end = end_pos(tok);
		x.tok = tok;

		return x;
	case .Open_Paren:
		open := expect_token(p, .Open_Paren);
		p.expr_level += 1;
		expr := parse_rhs_or_type(p);
		p.expr_level -= 1;
		close := expect_token(p, .Close_Paren);

		x := new_node(p, Ast_Paren_Expr);
		x.pos = open.pos;
		x.end = end_pos(close);
		x.open = open.pos;
		x.expr = expr;
		x.close = close.pos;
		return x;

	case .Proc:
		return parse_proc_type_or_lit(p);
	}

	if type := parse_try_type(p); type != nil {
		_, is_ident := type.variant.(^Ast_Ident);
		assert(!is_ident, "type cannot be identifier");
		return type;
	}

	// Error
	tok := p.curr_tok;
	parser_error(p, tok.pos, "expected operand");
	parser_fix_sync(p, &_stmt_start_map);
	bad := new_node(p, Ast_Bad_Expr);
	bad.pos = tok.pos;
	bad.end = end_pos(p.curr_tok);
	return bad;
}


parse_proc_type :: proc(p: ^Parser) -> ^Ast_Proc_Type {
	tok := expect_token(p, .Proc);
	params, results := parse_signature(p);

	type := new_node(p, Ast_Proc_Type);
	type.pos = tok.pos;
	type.end = results.end if results != nil else params.end;
	type.params = params;
	type.results = results;
	return type;
}

parse_proc_type_or_lit :: proc(p: ^Parser) -> ^Ast_Expr {
	type := parse_proc_type(p);
	if p.curr_tok.kind != .Open_Brace {
		return type;
	}

	p.expr_level += 1;
	body := parse_body(p);
	p.expr_level -= 1;

	lit := new_node(p, Ast_Proc_Lit);
	lit.pos = type.pos;
	lit.end = body.end;
	lit.type = type;
	lit.body = body;
	return lit;
}

parse_value :: proc(p: ^Parser, allow_key: bool) -> ^Ast_Expr {
	if p.curr_tok.kind == .Open_Brace {
		return parse_literal_value(p, nil);
	}

	return parser_check_expr(p, parse_expr(p, allow_key));
}


parse_element :: proc(p: ^Parser) -> ^Ast_Expr {
	x := parse_value(p, true);
	if p.curr_tok.kind == .Assign {
		assign := expect_token(p, .Assign);
		key := x;
		value := parse_value(p, false);
		kv := new_node(p, Ast_Key_Value_Expr);
		kv.pos = key.pos;
		kv.end = value.end;
		kv.key = key;
		kv.assign = assign.pos;
		kv.value = value;
		x = kv;
	}

	return x;
}

parse_element_list :: proc(p: ^Parser) -> []^Ast_Expr {
	list: [dynamic]^Ast_Expr;
	for p.curr_tok.kind != .Close_Brace && p.curr_tok.kind != .EOF {
		elem := parse_element(p);
		append(&list, elem);
		if !parser_at_comma(p, "compound literal", .Close_Brace) {
			break;
		}
		advance_token(p);
	}

	return list[:];
}


parse_literal_value :: proc(p: ^Parser, type: ^Ast_Expr) -> ^Ast_Comp_Lit {
	open := expect_token(p, .Open_Brace);
	elems: []^Ast_Expr;
	p.expr_level += 1;
	if p.curr_tok.kind != .Close_Brace {
		elems = parse_element_list(p);
	}
	p.expr_level -= 1;
	close := expect_closing(p, .Close_Brace, "compound literal");


	comp := new_node(p, Ast_Comp_Lit);
	comp.pos = type.pos if type != nil else open.pos;
	comp.end = end_pos(close);
	comp.type = type;
	comp.open = open.pos;
	comp.elems = elems;
	comp.close = close.pos;
	return comp;
}

parse_rhs_or_type :: proc(p: ^Parser) -> ^Ast_Expr {
	return parser_check_expr_or_type(p, parse_expr(p, false));
}

parse_field_decl :: proc(p: ^Parser) -> ^Ast_Field {
	names := parse_ident_list(p);
	type := parse_type(p);
	expect_semicolon(p);
	return nil;
}

parse_try_type :: proc(p: ^Parser) -> ^Ast_Expr {
	#partial switch p.curr_tok.kind {
	case .Ident:
		return parse_ident(p);

	case .Open_Bracket:
		open := expect_token(p, .Open_Bracket);

		len: ^Ast_Expr;

		p.expr_level += 1;
		if p.curr_tok.kind != .Close_Bracket {
			len = parse_rhs(p);
		}
		p.expr_level -= 1;

		close := expect_token(p, .Close_Bracket);
		
		elem := parse_type(p);

		at := new_node(p, Ast_Array_Type);
		at.pos = open.pos;
		at.end = elem.end;
		at.open = open.pos;
		at.len = len;
		at.close = close.pos;
		at.elem = elem;
		return at;

	case .Struct:
		tok := expect_token(p, .Struct);
		open := expect_token(p, .Open_Brace);
		fields: [dynamic]^Ast_Field;
		for p.curr_tok.kind == .Ident {
			field := parse_field_decl(p);
			append(&fields, field);
		}

		close := expect_token(p, .Close_Brace);

		field_list := new_node(p, Ast_Field_List);
		field_list.pos = open.pos;
		field_list.end = end_pos(close);
		field_list.open = open.pos;
		field_list.list = fields[:];
		field_list.close = close.pos;

		st := new_node(p, Ast_Struct_Type);
		st.pos = tok.pos;
		st.end = end_pos(close);
		st.tok = tok;
		st.fields = field_list;
		return st;

	case .Pointer:
		tok := expect_token(p, .Pointer);
		elem := parse_type(p);

		x := new_node(p, Ast_Pointer_Type);
		x.pos = tok.pos;
		x.end = elem.end;
		x.tok = tok;
		x.elem = elem;

		return x;

	case .Proc:
		return parse_proc_type(p);

	case .Open_Paren:
		open := expect_token(p, .Open_Paren);
		type := parse_type(p);
		close := expect_token(p, .Close_Paren);

		x := new_node(p, Ast_Paren_Expr);
		x.pos = open.pos;
		x.end = end_pos(close);

		x.open = open.pos;
		x.expr = type;
		x.close = close.pos;

		return x;
	}
	return nil;
}

parse_type :: proc(p: ^Parser)-> ^Ast_Expr {
	type := parse_try_type(p);
	if type != nil {
		return type;
	}

	tok := p.curr_tok;
	parser_error(p, tok.pos, "expected type");
	parser_fix_sync(p, &_expr_end_map);
	be := new_node(p, Ast_Bad_Expr);
	be.pos = tok.pos;
	be.end = end_pos(p.curr_tok);
	return be;
}


is_literal_type :: proc(x: ^Ast_Expr) -> bool {
	#partial switch t in x.variant {
	case ^Ast_Bad_Expr:
	case ^Ast_Ident:
	case ^Ast_Selector_Expr:
	case ^Ast_Array_Type:
	case ^Ast_Struct_Type:
	case:
		return false;
	}
	return true;
}





parser_fix_sync :: proc(p: ^Parser, to: ^[Token_Kind]bool) {
	for ; p.curr_tok.kind != .EOF; _ = advance_token(p) {
		if to[p.curr_tok.kind] {
			if pos_compare(p.curr_tok.pos, p.sync_pos) == 0 && p.sync_count < MAX_SYNC_COUNT {
				p.sync_count += 1;
				return;
			}
			if pos_compare(p.curr_tok.pos, p.sync_pos) > 0 {
				p.sync_pos = p.curr_tok.pos;
				p.sync_count = 0;
				return;
			}
		}
	}
}


_stmt_start_map := [Token_Kind]bool{
	.Break       = true,
	.Continue    = true,
	.Fallthrough = true,

	.Defer  = true,
	.If     = true,
	.For    = true,
	.Switch = true,
	.Return = true,

	.Const = true,
	.Var   = true,
	.Type  = true,
};

_decl_start_map := [Token_Kind]bool{
	.Const   = true,
	.Var     = true,
	.Type    = true,
	.Proc    = true,
	.Import  = true,
	.Foreign = true,
	.Export  = true,
};

_expr_end_map := [Token_Kind]bool{
	.Semicolon     = true,
	.Comma         = true,
	.Colon         = true,
	.Close_Paren   = true,
	.Close_Bracket = true,
	.Close_Brace   = true,
};
