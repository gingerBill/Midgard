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
