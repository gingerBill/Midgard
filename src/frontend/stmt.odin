package frontend

check_assignment :: proc(c: ^Checker_Context, x: ^Operand, T: ^Type, ctx: string) {
	check_no_tuple(c, x);

	if x.mode == .Invalid {
		return;
	}

	if type_is_untyped(x.type) {

	}

	if T == nil {
		return;
	}

	if ok, reason := is_assignable_to(c, x, T); !ok {
		if reason != "" {
			check_error(x.expr.pos, "cannot use {} as {} value in {}: {}", x, T, ctx, reason);
		} else {
			check_error(x.expr.pos, "cannot use {} as {} value in {}", x, T, ctx);
		}
	}
}


is_assignable_to :: proc(c: ^Checker_Context, x: ^Operand, T: ^Type) -> (ok: bool, reason: string) {
	if x.mode == .Invalid || T == t_invalid {
		return true, reason;
	}

	V := x.type;

	if are_identical(V, T) {
		return true, reason;
	}

	return false, reason;
}


Stmt_Flag :: enum {
	Break_Ok,
	Continue_Ok,
	Fallthrough_Ok,
}
Stmt_Flags :: bit_set[Stmt_Flag];

check_stmt_list :: proc(c: ^Checker_Context, stmt_flags: Stmt_Flags, list: []^Ast_Stmt) {
	fallthrough_ok := .Fallthrough_Ok in stmt_flags;
	inner_ := stmt_flags &~ {.Fallthrough_Ok};
	stmts := trim_trailing_empty_stmts(list);
	for stmt, i in stmts {
		inner := inner_;
		if fallthrough_ok && i+1 == len(stmts) {
			inner |= {.Fallthrough_Ok};
		}
		check_stmt(c, inner, stmt);
	}
}

trim_trailing_empty_stmts :: proc(list: []^Ast_Stmt) -> []^Ast_Stmt {
	for i := len(list); i > 0; i -= 1 {
		if _, ok := list[i-1].variant.(^Ast_Empty_Stmt); !ok {
			return list[:i];
		}
	}
	return nil;
}

check_stmt :: proc(c: ^Checker_Context, stmt_flags: Stmt_Flags, stmt: ^Ast_Stmt) {
	check_error(stmt.pos, "{:T}", stmt);

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

	case ^Ast_Labeled_Stmt:

	case ^Ast_Assign_Stmt:

	case ^Ast_Block_Stmt:

	case ^Ast_Defer_Stmt:

	case ^Ast_Branch_Stmt:

	case ^Ast_If_Stmt:

	case ^Ast_For_Stmt:

	case ^Ast_Switch_Stmt:

	case ^Ast_Return_Stmt:

	case ^Ast_Case_Clause:

	case:
		check_error(stmt.pos, "invalid statement");
	}
}
