package frontend

import "core:reflect"

check_assignment :: proc(c: ^Checker_Context, x: ^Operand, T: ^Type, ctx: string) {
	check_no_tuple(c, x);

	if x.mode == .Invalid {
		return;
	}

	if type_is_untyped(x.type) {
		// TODO(bill): edge case for untyped types
	}

	if T == nil {
		return;
	}

	if ok := is_assignable_to(c, x, T); !ok {
		check_error(x.expr.pos, "cannot use '{}' as '{}' value in {}", x, T, ctx);
	}
}


is_assignable_to :: proc(c: ^Checker_Context, x: ^Operand, T: ^Type) -> (ok: bool) {
	if x.mode == .Invalid || T == &btype[.invalid] {
		return true;
	}

	V := x.type;

	if are_identical(V, T) {
		return true;
	}

	V_u := type_underlying(V);
	T_u := type_underlying(T);

	if type_is_untyped(V_u) {
		#partial switch t in T_u.variant {
		case ^Basic:
			if is_operand_nil(x) && t.kind == .rawptr {
				return true;
			}
			if x.mode == .Constant {
				return representable_as_constant(c, x.value, t, nil);
			}

			if V_b, ok := V_u.variant.(^Basic); ok && V_b != nil {
				return V_b.kind == .untyped_bool && type_is_boolean(T_u);
			}

		case ^Pointer, ^Signature, ^Slice:
			return is_operand_nil(x);
		}
	}

	return false;
}


open_scope :: proc(c: ^Checker_Context, s: ^Ast_Stmt, comment: string) {
	scope := create_scope(c.scope, s.pos, s.end);
	c.scope = scope;
}

close_scope :: proc(c: ^Checker_Context) {
	c.scope = c.scope.parent;
}

Stmt_Flag :: enum {
	Break_Ok,
	Continue_Ok,
	Fallthrough_Ok,
}
Stmt_Flags :: bit_set[Stmt_Flag];

check_stmt_list :: proc(c: ^Checker_Context, stmt_flags: Stmt_Flags, list: []^Ast_Stmt) {
	stmt_flags := stmt_flags;
	fallthrough_ok := .Fallthrough_Ok in stmt_flags;
	stmt_flags &~= {.Fallthrough_Ok};
	stmts := trim_trailing_empty_stmts(list);

	for stmt, i in stmts do if s, ok := stmt.variant.(^Ast_Decl_Stmt); ok {
		check_decl_stmt_out_of_order(c, s.decl);
	}

	for stmt, i in stmts {
		inner := stmt_flags;
		if fallthrough_ok && i+1 == len(stmts) {
			inner |= {.Fallthrough_Ok};
		}
		check_stmt(c, inner, stmt);
	}

	for e in c.scope.delayed {
		assert(e != nil);
		check_entity_decl(c, e, nil);
	}
	for e in c.scope.delayed {
		if e.decl.proc_decl != nil {
			p := e.variant.(^Procedure);
			sig := p.type.variant.(^Signature);
			ctx := c^;
			ctx.scope = sig.scope;
			ctx.decl = p.decl;
			ctx.proc_name = p.name;
			ctx.curr_proc_decl = e.decl;
			ctx.curr_proc_type = sig;
			check_proc_body(&ctx, p, e.decl);
		}
	}
	clear(&c.scope.delayed);
	delete(c.scope.delayed);
}

trim_trailing_empty_stmts :: proc(list: []^Ast_Stmt) -> []^Ast_Stmt {
	for i := len(list); i > 0; i -= 1 {
		if _, ok := list[i-1].variant.(^Ast_Empty_Stmt); !ok {
			return list[:i];
		}
	}
	return nil;
}

check_assign_var :: proc(c: ^Checker_Context, lhs: ^Ast_Expr, x: ^Operand) -> ^Type {
	if x.mode == .Invalid || x.type == &btype[.invalid] {
		return nil;
	}

	ident, _ := unparen(lhs).variant.(^Ast_Ident);

	if ident != nil && ident.name == "_" {
		ident.entity = nil;
		check_assignment(c, x, nil, "assignment to _ identifier");
		if x.mode == .Invalid {
			return nil;
		}
		return x.type;
	}

	y: Operand;
	check_expr(c, &y, lhs);
	if y.mode == .Invalid || y.type == &btype[.invalid] {
		return nil;
	}

	if y.mode == .Variable {
		// Okay
	} else {
		check_error(y.expr.pos, "cannot assign to {}", &y);
		return nil;
	}

	check_assignment(c, x, y.type, "assignment");
	if x.mode == .Invalid {
		return nil;
	}

	return x.type;
}


check_stmt :: proc(c: ^Checker_Context, stmt_flags: Stmt_Flags, stmt: ^Ast_Stmt) {
	if stmt == nil {
		return;
	}
	// check_error(stmt.pos, "{}", reflect.union_variant_typeid(stmt.variant));

	switch s in stmt.variant {
	case ^Ast_Bad_Stmt, ^Ast_Empty_Stmt:
		// Ignore

	case ^Ast_Expr_Stmt:
		x: Operand;
		kind := check_raw_expr(c, &x, s.expr, nil);
		#partial switch x.mode {
		case .Builtin:
			check_error(x.expr.pos, "{} must be called", &x);
		case .Type:
			check_error(x.expr.pos, "{} is not an expression", &x);
		case:
			if kind == .Stmt {
				return;
			}
			check_error(x.expr.pos, "{} is not used", &x);
		}

	case ^Ast_Decl_Stmt:
		check_decl_stmt_in_order(c, s.decl);

	case ^Ast_Labeled_Stmt:
		check_error(stmt.pos, "TODO(bill): {:T}", s);

	case ^Ast_Assign_Stmt:
		if s.op.kind == .Assign {
			if len(s.lhs) == 0 {
				check_error(s.pos, "invalid AST: missing left hand side in assignment");
				return;
			}
			// TODO(bill): assigning
		} else {
			if len(s.lhs) != 1 || len(s.rhs) != 1 {
				check_error(s.pos, "assignment operation {} requires single-valued expressions", s.op.text);
				return;
			}

			op := assign_op(s.op.kind);
			if op == .Invalid {
				check_error(s.pos, "invalid AST: unknown assignment operator {}", s.op.text);;
				return;
			}

			x: Operand;
			check_binary(c, &x, nil, s.lhs[0], s.rhs[0], op);
			if x.mode == .Invalid {
				return;
			}

			check_assign_var(c, s.lhs[0], &x);
		}

	case ^Ast_Defer_Stmt:
		check_error(stmt.pos, "TODO(bill): {:T}", s);

	case ^Ast_Branch_Stmt:
		check_error(stmt.pos, "TODO(bill): {:T}", s);

	case ^Ast_Block_Stmt:
		open_scope(c, s, "block");
		defer close_scope(c);

		check_stmt_list(c, stmt_flags, s.stmts);

	case ^Ast_If_Stmt:
		open_scope(c, s, "if");
		defer close_scope(c);

		check_stmt(c, nil, s.init);

		x: Operand;
		check_expr(c, &x, s.cond);
		if x.mode != .Invalid && !type_is_boolean(x.type) {
			check_error(s.cond.pos, "non-boolean condition in if statement");
		}

		check_stmt(c, stmt_flags, s.body);
		if s.else_stmt != nil {
			#partial switch e in s.else_stmt.variant {
			case ^Ast_Bad_Stmt:
			case ^Ast_If_Stmt, ^Ast_Block_Stmt:
				check_stmt(c, stmt_flags, s.else_stmt);
			case:
				check_error(s.else_stmt.pos, "invalid else statement in if statement");
			}
		}

	case ^Ast_For_Stmt:
		open_scope(c, s, "for");
		defer close_scope(c);

		check_stmt(c, nil, s.init);

		x: Operand;
		check_expr(c, &x, s.cond);
		if x.mode != .Invalid && !type_is_boolean(x.type) {
			check_error(s.cond.pos, "non-boolean condition in for statement");
		}

		check_stmt(c, nil, s.post);
		check_stmt(c, stmt_flags, s.body);

	case ^Ast_Switch_Stmt:
		check_error(stmt.pos, "TODO(bill): {:T}", s);

	case ^Ast_Return_Stmt:
		check_error(stmt.pos, "TODO(bill): {:T}", s);

	case ^Ast_Case_Clause:
		check_error(stmt.pos, "TODO(bill): {:T}", s);

	case:
		check_error(stmt.pos, "invalid statement");
	}
}
