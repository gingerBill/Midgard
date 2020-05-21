package frontend



Package :: struct {
	id:       int,
	name:     string,
	fullpath: string,
	files:    []^File,
	scope: ^Scope,
}

File :: struct {
	id:  int,
	pkg: ^Package,
	scope: ^Scope,

	fullpath: string,
	src:      []byte,

	decls: [dynamic]^Ast_Decl,

	syntax_error_count: int,
}

Ast_Comment_Group :: struct {
	list: []Token,
}



// Base Types
Ast_Node :: struct {
	pos, end: Pos, // TODO(bill): should these be just procedures instead?
}

Ast_Expr :: struct {
	using node_base: Ast_Node,
	tav: Type_And_Value,
	variant: union {
		^Ast_Bad_Expr,
		^Ast_Ident,
		^Ast_Basic_Lit,
		^Ast_Unary_Expr,
		^Ast_Binary_Expr,
		^Ast_Paren_Expr,
		^Ast_Deref_Expr,
		^Ast_Call_Expr,
		^Ast_Comp_Lit,
		^Ast_Proc_Lit,
		^Ast_Selector_Expr,
		^Ast_Index_Expr,
		^Ast_Slice_Expr,
		^Ast_Key_Value_Expr,

		^Ast_Pointer_Type,
		^Ast_Array_Type,
		^Ast_Struct_Type,
		^Ast_Proc_Type,

		^Ast_Field,
		^Ast_Field_List,
	},
}
Ast_Stmt :: struct {
	using node_base: Ast_Node,
	variant: union {
		^Ast_Bad_Stmt,
		^Ast_Empty_Stmt,
		^Ast_Expr_Stmt,
		^Ast_Decl_Stmt,
		^Ast_Labeled_Stmt,
		^Ast_Assign_Stmt,
		^Ast_Defer_Stmt,
		^Ast_Branch_Stmt,
		^Ast_Block_Stmt,
		^Ast_If_Stmt,
		^Ast_For_Stmt,
		^Ast_Switch_Stmt,
		^Ast_Return_Stmt,
		^Ast_Case_Clause,
	},
}
Ast_Decl :: struct {
	using node_base: Ast_Node,
	variant: union {
		^Ast_Bad_Decl,
		^Ast_Gen_Decl,
		^Ast_Proc_Decl,
		^Ast_Foreign_Decl,
	},
}

// Declarations

Ast_Spec :: struct {
	using node_base: Ast_Node,
	variant: union {
		^Ast_Import_Spec,
		^Ast_Value_Spec,
		^Ast_Type_Spec,
		^Ast_Export_Spec,
	},
}


Ast_Bad_Decl :: struct {
	using node: Ast_Decl,
}

Ast_Gen_Decl :: struct {
	using node: Ast_Decl,

	tok:         Token,
	open_paren:  Pos,
	specs:       []^Ast_Spec,
	close_paren: Pos,
}

Ast_Proc_Decl :: struct {
	using node: Ast_Decl,
	name: ^Ast_Ident,
	type: ^Ast_Proc_Type,
	body: ^Ast_Block_Stmt,
}

Ast_Foreign_Decl :: struct {
	using node:  Ast_Decl,
	tok:         Token,
	lib:         Token,
	open_paren:  Pos,
	decls:       []^Ast_Decl,
	close_paren: Pos,
}

Ast_Import_Spec :: struct {
	using node: Ast_Spec,
	name: ^Ast_Ident,
	path: ^Ast_Basic_Lit,
}

Ast_Value_Spec :: struct {
	using node: Ast_Spec,
	keyword: Token,
	names:   []^Ast_Ident,
	type:    ^Ast_Expr,
	values:  []^Ast_Expr,
}

Ast_Type_Spec :: struct {
	using node: Ast_Spec,
	name:   ^Ast_Ident,
	assign: Pos, // position of '=', if any (type alias)
	type:   ^Ast_Expr,
}

Ast_Export_Spec :: struct {
	using node: Ast_Spec,
	decl: ^Ast_Decl,
}



// Statements

Ast_Bad_Stmt :: struct {
	using node: Ast_Stmt,
}

Ast_Empty_Stmt :: struct {
	using node: Ast_Stmt,
	semicolon:  Pos, // Position of the following ';'
	implicit:   bool,
}
Ast_Expr_Stmt :: struct {
	using node: Ast_Stmt,
	expr:       ^Ast_Expr,
}
Ast_Decl_Stmt :: struct {
	using node: Ast_Stmt,
	decl:       ^Ast_Decl,
}

Ast_Labeled_Stmt :: struct {
	using node: Ast_Stmt,
	label: ^Ast_Ident,
	colon: Pos,
	stmt:  ^Ast_Stmt,
}


Ast_Assign_Stmt :: struct {
	using node: Ast_Stmt,
	lhs: []^Ast_Expr,
	op:  Token,
	rhs: []^Ast_Expr,
}

Ast_Block_Stmt :: struct {
	using node: Ast_Stmt,
	open:  Pos,
	stmts: []^Ast_Stmt,
	close: Pos,
}

Ast_Defer_Stmt :: struct {
	using node: Ast_Stmt,
	tok:  Token,
	stmt: ^Ast_Stmt,
}

Ast_Branch_Stmt :: struct {
	using node: Ast_Stmt,
	tok:  Token,
	label: ^Ast_Ident,
}


Ast_If_Stmt :: struct {
	using node: Ast_Stmt,
	if_pos:    Pos,
	init:      ^Ast_Stmt,
	cond:      ^Ast_Expr,
	body:      ^Ast_Stmt,
	else_stmt: ^Ast_Stmt,
}

Ast_For_Stmt :: struct {
	using node: Ast_Stmt,
	for_pos: Pos,
	init:    ^Ast_Stmt,
	cond:    ^Ast_Expr,
	post:    ^Ast_Stmt,
	body:    ^Ast_Stmt,
}

Ast_Switch_Stmt :: struct {
	using node: Ast_Stmt,
	switch_pos: Pos,
	init:       ^Ast_Stmt,
	cond:       ^Ast_Expr,
	open:       Pos,
	clauses:    []^Ast_Case_Clause,
	close:      Pos,
}

Ast_Case_Clause :: struct {
	using node: Ast_Stmt,
	tok:     Token,
	clauses: []^Ast_Expr,
	colon:   Pos,
	body:    []^Ast_Stmt,
}



Ast_Return_Stmt :: struct {
	using node: Ast_Stmt,
	tok_pos: Pos,
	results: []^Ast_Expr,
}


// Expressions

Ast_Bad_Expr :: struct {
	using node: Ast_Expr,
}

Ast_Ident :: struct {
	using node: Ast_Expr,
	name: string,
	entity: ^Entity,
}

Ast_Basic_Lit :: struct {
	using node: Ast_Expr,
	tok: Token,
}

Ast_Unary_Expr :: struct {
	using node: Ast_Expr,
	op:   Token,
	expr: ^Ast_Expr,
}

Ast_Binary_Expr :: struct {
	using node: Ast_Expr,
	left:  ^Ast_Expr,
	op:    Token,
	right: ^Ast_Expr,
}

Ast_Paren_Expr :: struct {
	using node: Ast_Expr,
	open:  Pos,
	expr:  ^Ast_Expr,
	close: Pos,
}

Ast_Deref_Expr :: struct {
	using node: Ast_Expr,
	expr: ^Ast_Expr,
	op: Pos,
}

Ast_Call_Expr :: struct {
	using node: Ast_Expr,
	expr:  ^Ast_Expr,
	open:  Pos,
	args:  []^Ast_Expr,
	close: Pos,
}

Ast_Comp_Lit :: struct {
	using node:  Ast_Expr,
	type:       ^Ast_Expr,
	open:       Pos,
	elems:      []^Ast_Expr,
	close:      Pos,
}

Ast_Proc_Lit :: struct {
	using node:  Ast_Expr,
	type: ^Ast_Proc_Type,
	body: ^Ast_Block_Stmt,
}

Ast_Selector_Expr :: struct {
	using node: Ast_Expr,
	x:   ^Ast_Expr,
	sel: ^Ast_Ident,
}

Ast_Index_Expr :: struct {
	using node: Ast_Expr,
	x:     ^Ast_Expr,
	open:  Pos,
	index: ^Ast_Expr,
	close: Pos,
}

Ast_Slice_Expr :: struct {
	using node: Ast_Expr,
	x:     ^Ast_Expr,
	open:  Pos,
	low:   ^Ast_Expr,
	high:  ^Ast_Expr,
	close: Pos,
}


Ast_Key_Value_Expr :: struct {
	using node: Ast_Expr,
	key:    ^Ast_Expr,
	assign: Pos,
	value:  ^Ast_Expr,
}

// Field

Ast_Field :: struct {
	using node: Ast_Expr,
	names: []^Ast_Ident,
	type:  ^Ast_Expr,
	implicit: ^Entity,
}

Ast_Field_List :: struct {
	using node: Ast_Expr,
	open:  Pos,
	list:  []^Ast_Field,
	close: Pos,
}

field_count :: proc(f: ^Ast_Field_List) -> int {
	n := 0;
	if f != nil {
		for field in f.list {
			m := len(field.names);
			if m == 0 {
				m = 1;
			}
			n += m;
		}
	}
	return n;
}


// Types

Ast_Pointer_Type :: struct {
	using node: Ast_Expr,
	tok:  Token,
	elem: ^Ast_Expr,
}
Ast_Array_Type :: struct {
	using node: Ast_Expr,
	open:  Pos,
	len:   ^Ast_Expr,
	close: Pos,
	elem:  ^Ast_Expr,
}

Ast_Struct_Type :: struct {
	using node: Ast_Expr,
	tok:        Token,
	fields:     ^Ast_Field_List,
}


Ast_Proc_Type :: struct {
	using node: Ast_Expr,
	tok:     Token,
	params:  ^Ast_Field_List,
	results: ^Ast_Field_List,
	scope:   ^Scope,
}

