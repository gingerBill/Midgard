package frontend

import "../constant"
import "core:strconv"
import "core:fmt"
import "core:reflect"

constant_from_token :: proc(tok: Token, allocator := context.allocator) -> constant.Value {
	#partial switch tok.kind {
	case .Integer:
		if val, ok := strconv.parse_i64(tok.text); ok {
			return i128(val);
		} else if val, ok := strconv.parse_u64(tok.text); ok {
			return i128(val);
		}
	case .Float:
		if val, ok := strconv.parse_f64(tok.text); ok {
			return val;
		}
	case .Rune:
		if r, _, _, ok := strconv.unquote_char(tok.text, '\''); ok {
			return i128(r);
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

constant_op :: proc(op: Token_Kind) -> constant.Op {
	#partial switch op {
	case .Add:     return .Add;
	case .Sub:     return .Sub;
	case .Mul:     return .Mul;
	case .Quo:     return .Quo;
	case .Mod:     return .Mod;

	case .And:     return .And;
	case .Or:      return .Or;
	case .Xor:     return .Xor;
	case .Shl:     return .Shl;
	case .Shr:     return .Shr;
	case .And_Not: return .And_Not;

	case .Cmp_And: return .Cmp_And;
	case .Cmp_Or:  return .Cmp_Or;

	case .Cmp_Eq:  return .Cmp_Eq;
	case .Not_Eq:  return .Not_Eq;
	case .Lt:      return .Lt;
	case .Gt:      return .Gt;
	case .Lt_Eq:   return .Lt_Eq;
	case .Gt_Eq:   return .Gt_Eq;

	case .Not:     return .Not;
	}
	return .Invalid;
}


add_type_and_value :: proc(c: ^Checker_Context, e: ^Ast_Expr, mode: Addressing_Mode, type: ^Type, value: constant.Value) {
	e.tav.mode = mode;
	e.tav.type = type;
	e.tav.value = value;
}

Expr_Kind :: enum {
	Expr,
	Stmt,
	Conv,
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
		type = &btype[.invalid];
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
	assert(expr != nil);
	// check_error(expr.pos, "{} {}", reflect.union_variant_typeid(expr.variant), expr);

	x.mode = .Invalid;
	x.type = &btype[.invalid];

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
		check_error(expr.pos, "TODO(bill): ^Ast_Unary_Expr");
		check_expr(c, x, e.expr);
		if x.mode == .Invalid {
			return expr_err(x, expr);
		}

	case ^Ast_Binary_Expr:
		check_binary(c, x, e, e.left, e.right, e.op.kind);

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
		return check_call(c, x, e);

	case ^Ast_Comp_Lit:
		check_error(expr.pos, "TODO(bill): ^Ast_Comp_Lit");
		return expr_err(x, expr);


	case ^Ast_Proc_Lit:
		check_error(expr.pos, "TODO(bill): ^Ast_Proc_Lit");
		return expr_err(x, expr);


	case ^Ast_Selector_Expr:
		check_error(expr.pos, "TODO(bill): ^Ast_Selector_Expr");
		return expr_err(x, expr);


	case ^Ast_Index_Expr:
		check_error(expr.pos, "TODO(bill): ^Ast_Index_Expr");
		return expr_err(x, expr);


	case ^Ast_Slice_Expr:
		check_error(expr.pos, "TODO(bill): ^Ast_Slice_Expr");
		return expr_err(x, expr);


	case ^Ast_Key_Value_Expr:
		check_error(expr.pos, "TODO(bill): ^Ast_Key_Value_Expr");
		return expr_err(x, expr);


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


check_comparison :: proc(c: ^Checker_Context, x, y: ^Operand, op: Token_Kind) {
	if is_assignable_to(c, x, y.type) || is_assignable_to(c, y, x.type) {
		ok := false;
		#partial switch op {
		case .Cmp_Eq, .Not_Eq:
			ok = (type_is_comparable(x.type) && type_is_comparable(y.type)) ||
			     (is_operand_nil(x) && type_has_nil(y.type)) ||
			     (is_operand_nil(y) && type_has_nil(x.type));

		case .Lt, .Lt_Eq, .Gt, .Gt_Eq:
			ok = type_is_ordered(x.type) && type_is_ordered(y.type);

		case:
			unreachable();
		}

		if !ok {
			type := x.type;
			if is_operand_nil(x) {
				type = y.type;
			}
			check_error(x.expr.pos, "cannot compare {} {} {} (operator {} not defined for {})", x.expr, op, y.expr, op, type);
			x.mode = .Invalid;
			return;
		}
	} else {
		check_error(x.expr.pos, "cannot compare {} {} {} (mismatched types {} and {})", x.expr, op, y.expr, x.type, y.type);
		x.mode = .Invalid;
		return;
	}

	if x.mode == .Constant && y.mode == .Constant {
		x.value = constant.binary_op(x.value, constant_op(op), y.value);
	}

	x.type = &btype[.untyped_bool];
}

Op_Predicate :: #type proc(t: ^Type) -> bool;

check_op :: proc(c: ^Checker_Context, preds: [Token_Kind]Op_Predicate, x: ^Operand, op: Token_Kind) -> bool {
	if pred := preds[op]; pred != nil {
		if !pred(x.type) {
			check_error(x.expr.pos, "operator {} not defined for {}", op, x);
			return false;
		}
		return true;
	}
	check_error(x.expr.pos, "invalid AST: unknown operator {}", op);
	return false;
}

unary_op_preds := [Token_Kind]Op_Predicate {
	.Add = type_is_numeric,
	.Sub = type_is_numeric,

	.Xor = type_is_integer,
	.Not = type_is_boolean,
};

binary_op_preds := [Token_Kind]Op_Predicate {
	.Add = type_is_numeric,
	.Sub = type_is_numeric,
	.Mul = type_is_numeric,
	.Quo = type_is_numeric,
	.Mod = type_is_numeric,

	.And     = type_is_integer,
	.Or      = type_is_integer,
	.Xor     = type_is_integer,
	.And_Not = type_is_integer,

	.Cmp_And = type_is_boolean,
	.Cmp_Or  = type_is_boolean,
};



check_unary :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Unary_Expr, op: Token_Kind) {
	if op == .And {
		if x.mode != .Variable {
			check_error(x.expr.pos, "cannot take address of {}", x);
			x.mode = .Invalid;
			return;
		}
		x.mode = .Value;
		x.type = new_pointer(x.type);
		return;
	}

	if !check_op(c, unary_op_preds, x, op) {
		x.mode = .Invalid;
		return;
	}

	if x.mode == .Constant {
		type := type_underlying(x.type).variant.(^Basic);
		x.value = constant.unary_op(constant_op(op), x.value);
		if type_is_typed(type) {
			if e != nil {
				x.expr = e;
			}
			check_representable_as_constant(c, x, type);
		}
		return;
	}

	x.mode = .Value;
}



check_binary :: proc(c: ^Checker_Context, x: ^Operand, e: ^Ast_Binary_Expr, lhs, rhs: ^Ast_Expr, op: Token_Kind) {
	y := &Operand{};

	check_expr(c, x, lhs);
	check_expr(c, y, rhs);
	if x.mode == .Invalid {
		return;
	}
	if y.mode == .Invalid {
		x.mode = .Invalid;
		x.expr = y.expr;
		return;
	}

	if is_comparison(op) {
		check_comparison(c, x, y, op);
		return;
	}

	if !are_identical(x.type, y.type) {
		if x.type != &btype[.invalid] && y.type != &btype[.invalid] {
			check_error(x.expr.pos, "mismatched types {} and {}", x.type, y.type);
		}
		x.mode = .Invalid;
		return;
	}

	if !check_op(c, binary_op_preds, x, op) {
		x.mode = .Invalid;
		return;
	}

	if op == .Quo || op == .Mod {
		if (x.mode == .Constant || type_is_integer(x.type)) && y.mode == .Constant && constant.sign(y.value) == 0 {
			check_error(y.expr.pos, "division by zero");
			x.mode = .Invalid;
			return;
		}
	}

	if x.mode == .Constant && y.mode == .Constant {
		xval := x.value;
		yval := y.value;

		type := type_underlying(x.type).variant.(^Basic);
		x.value = constant.binary_op(xval, constant_op(op), yval);
		if type_is_typed(type) {
			if e != nil {
				x.expr = e;
			}
			check_representable_as_constant(c, x, type);
		}
		return;
	}

	x.mode = .Value;
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

		if x, ok := x.(i128); ok {
			#partial switch t.kind {
			case .i8:
				s :: 8;
				return -1<<(s-1) <= x && x <= 1<<(s-1)-1;
			case .i16:
				s :: 16;
				return -1<<(s-1) <= x && x <= 1<<(s-1)-1;
			case .i32:
				s :: 32;
				return -1<<(s-1) <= x && x <= 1<<(s-1)-1;
			case .i64, .untyped_int:
				return true;
			case .isize:
				s := i64(SIZES.word_size*8);
				return -1<<u64(s-1) <= x && x <= 1<<u64(s-1)-1;

			case .u8:
				s :: 8;
				return 0 <= x && x <= 1<<s-1;
			case .u16:
				s :: 16;
				return 0 <= x && x <= 1<<s-1;
			case .u32:
				s :: 32;
				return 0 <= x && x <= 1<<s-1;
			case .u64:
				return 0 <= x;

			case .usize:
				s := i64(SIZES.word_size*8);
				return 0 <= x && x <= 1<<u64(s)-1;

			case:
				unreachable();
			}
		}

		return true;

	case type_is_float(t):
		x, ok := constant.as_float(value);
		if !ok {
			return false;
		}
		if rounded != nil {
			rounded^ = x;
		}
		return true;

	case type_is_boolean(t):
		_, ok := constant.as_bool(value);
		return ok;

	case type_is_string(t):
		_, ok := value.(string);
		return ok;
	}

	return false;
}




check_representable_as_constant :: proc(c: ^Checker_Context, x: ^Operand, type: ^Basic) {
	assert(x.mode == .Constant);
	if !representable_as_constant(c, x.value, type, &x.value) {
		if type_is_numeric(x.type) && type_is_numeric(type) {
			if !type_is_integer(x.type) && type_is_integer(type) {
				check_error(x.expr.pos, "{} truncated to {}", x, type);
				x.mode = .Invalid;
				return;
			}
		} else {
			check_error(x.expr.pos, "cannot convert {} to {}", x, type);
			x.mode = .Invalid;
			return;
		}
	}
}

is_convertible_to :: proc(c: ^Checker_Context, x: ^Operand, T: ^Type) -> bool {
	panic("TODO: is_convertible_to");
	return false;
}

check_conversion :: proc(c: ^Checker_Context, x: ^Operand, T: ^Type) -> bool {
	ok := false;
	switch {
	case x.mode == .Constant && is_const_type(T):
		t := type_underlying(T).variant.(^Basic);
		ok = representable_as_constant(c, x.value, t, &x.value);
	case is_convertible_to(c, x, T):
		x.mode = .Value;
		ok = true;
	}

	if !ok {
		check_error(x.expr.pos, "cannot convert {} to {}", x, T);
		x.mode = .Invalid;
		return false;
	}

	x.type = T;
	return true;
}
