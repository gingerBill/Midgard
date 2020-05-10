package frontend

import "../constant"
import "core:strconv"
import "core:fmt"

constant_from_token :: proc(tok: Token, allocator := context.allocator) -> constant.Value {
	#partial switch tok.kind {
	case .Integer:
		if val, ok := strconv.parse_u64_maybe_prefixed(tok.text); ok {
			return val;
		}
	case .Float:
		if val, ok := strconv.parse_f64(tok.text); ok {
			return val;
		}
	case .Rune:
		if r, _, _, ok := strconv.unquote_char(tok.text, '\''); ok {
			return r;
		}
	case .String:
		if r, allocated, ok := strconv.unquote_string(tok.text, allocator); ok {
			return r;
		}
	case:
		panic("invalid token");
	}

	return nil;
}

add_type_and_value :: proc(c: ^Checker_Context, e: ^Ast_Expr, mode: Addressing_Mode, type: ^Type, value: constant.Value) {
	e.tav.mode = mode;
	e.tav.type = type;
	e.tav.value = value;
}

Expr_Kind :: enum {
	Expr,
	Stmt,
}


check_expr :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Expr) {
	check_multi_expr(c, x, e);
	check_no_tuple(c, x);
}


check_expr_or_type :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Expr) {
	check_raw_expr(c, x, e, nil);
	check_no_tuple(c, x);
	if x.mode == .No_Value {
		check_error(e.pos, "{} used as value or type", x);
		x.mode = .Invalid;
	}
}

check_expr_with_type_hint :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Expr, type_hint: ^Type) {
	assert(type_hint != nil);
	check_raw_expr(c, x, e, type_hint);
	check_no_tuple(c, x);
	#partial switch x.mode {
	case:
		return;
	case .No_Value:
		check_error(e.pos, "{} used as value", x);
	case .Builtin:
		check_error(e.pos, "built-in procedure {} must be called", x);
	case .Type:
		check_error(e.pos, "{} is not an expression", x);
	}

	x.mode = .Invalid;
}

check_multi_expr :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Expr) {
	check_raw_expr(c, x, e, nil);
	#partial switch x.mode {
	case:
		return;
	case .No_Value:
		check_error(e.pos, "{} used as value", x);
	case .Builtin:
		check_error(e.pos, "built-in procedure {} must be called", x);
	case .Type:
		check_error(e.pos, "{} is not an expression", x);
	}

	x.mode = .Invalid;
}

check_no_tuple :: proc(c: ^Checker_Context, x: ^Operand) {
	if x.mode == .Value && x.type != nil {
		if t, ok := x.type.variant.(^Tuple); ok {
			assert(len(t.vars) != 1);
			check_error(x.expr.pos, "{}-valued {} where single value was expected", len(t.vars), x);
			x.mode = .Invalid;
		}
	}
}


check_raw_expr :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Expr, type_hint: ^Type) -> Expr_Kind {
	kind := check_expr_internal(c, x, e, type_hint);

	type: ^Type;
	value: constant.Value;
	#partial switch x.mode {
	case .Invalid:
		type = t_invalid;
	case .No_Value:
		type = nil;
	case .Constant:
		type = x.type;
		value = x.value;
	case:
		type = x.type;
	}

	add_type_and_value(c, e, x.mode, type, value);

	return kind;
}


check_expr_internal :: proc(c: ^Checker_Context, x: ^Operand, expr: ^Ast_Expr, type_hint: ^Type) -> Expr_Kind {
	expr_err :: proc(x: ^Operand, e: ^Ast_Expr) -> Expr_Kind {
		x.mode = .Invalid;
		x.expr = e;
		return .Stmt;
	}

	x.mode = .Invalid;
	x.type = t_invalid;

	switch e in expr.variant {
	case ^Ast_Bad_Expr:
		return expr_err(x, expr);

	case ^Ast_Ident:
		check_ident(c, x, e, nil, false);

	case ^Ast_Basic_Lit:
		operand_set_constant(x, e.tok);
		if x.mode == .Invalid {
			check_error(expr.pos, "compiler-error: invalid basic literal {}", expr);
			return expr_err(x, expr);
		}

	case ^Ast_Unary_Expr:
		check_expr(c, x, e.expr);
		if x.mode == .Invalid {
			return expr_err(x, expr);
		}

	case ^Ast_Binary_Expr:
		// TODO(bill): binary expression


	case ^Ast_Paren_Expr:
		kind := check_raw_expr(c, x, e.expr, nil);
		x.expr = e;
		return kind;

	case ^Ast_Deref_Expr:
		check_expr(c, x, e.expr);
		if x.mode == .Invalid {
			return expr_err(x, expr);
		}

		if pt, ok := type_underlying(x.type).variant.(^Pointer); ok {
			x.mode = .Variable;
			x.type = pt.elem;
		} else {
			check_error(e.pos, "cannot dereference {}", x);
			return expr_err(x, expr);
		}

	case ^Ast_Call_Expr:
		// TODO(bill): ^Ast_Call_Expr

	case ^Ast_Comp_Lit:
		// TODO(bill): ^Ast_Comp_Lit

	case ^Ast_Proc_Lit:
		// TODO(bill): ^Ast_Proc_Lit

	case ^Ast_Selector_Expr:
		// TODO(bill): ^Ast_Selector_Expr

	case ^Ast_Index_Expr:
		// TODO(bill): ^Ast_Index_Expr

	case ^Ast_Slice_Expr:
		// TODO(bill): ^Ast_Slice_Expr

	case ^Ast_Key_Value_Expr:
		// TODO(bill): ^Ast_Key_Value_Expr

	case ^Ast_Pointer_Type, ^Ast_Array_Type, ^Ast_Struct_Type, ^Ast_Proc_Type:
		x.mode = .Type;
		x.type = check_type(c, expr);

	case ^Ast_Field, ^Ast_Field_List:
		panic("invalid expression type");
	case:
		panic("unknown expression type");
	}

	x.expr = expr;
	return .Expr;
}


representable_as_constant :: proc(c: ^Checker_Context, value: constant.Value, t: ^Basic, rounded: ^constant.Value) -> bool {
	if value == nil {
		// ignore propagated errors
		return true;
	}

	switch {
	case type_is_integer(t):
		x, ok := constant.as_int(value);
		if !ok {
			return false;
		}
		if rounded != nil {
			rounded^ = x;
		}

		return true;
	}

	return false;
}



