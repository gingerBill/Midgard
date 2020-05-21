package frontend

// import "core:strconv"
// import "core:strings"
// import "core:unicode/utf8"

Value :: union{bool, string, i128, f64};

Op :: enum u8 {
	Invalid,

	Add,
	Sub,
	Mul,
	Quo,
	Mod,
	And,
	Or,
	Xor,
	Shl,
	Shr,
	And_Not,
	Cmp_And,
	Cmp_Or,
	Cmp_Eq,
	Not_Eq,
	Lt,
	Gt,
	Lt_Eq,
	Gt_Eq,
	Not,
};


as_int :: proc(value: Value) -> (res: Value, ok: bool) {
	switch v in value {
	case bool:
		return;
	case string:
		return;
	case i128:
		res = i128(v);
		ok = true;
	case f64:
		res = i128(v);
		ok = true;
	}

	return;
}


as_float :: proc(value: Value) -> (res: Value, ok: bool) {
	switch v in value {
	case bool:
		return;
	case string:
		return;
	case i128:
		res = f64(v);
		ok = true;
	case f64:
		res = f64(v);
		ok = true;
	}

	return;
}


as_i64 :: proc(value: Value) -> (res: i64, ok: bool) {
	switch v in value {
	case bool:
		return;
	case string:
		return;
	case i128:
		res = i64(v);
		ok = true;
	case f64:
		res = i64(v);
		ok = true;
	}

	return;
}


as_bool :: proc(value: Value) -> (res: Value, ok: bool) {
	#partial switch v in value {
	case bool:
		return v, true;
	}

	return;
}


sign :: proc(value: Value) -> int {
	switch v in value {
	case bool:
		return 0;
	case string:
		return 0;
	case i128:
		if v < 0 {
			return -1;
		} else if v > 0 {
			return +1;
		}
	case f64:
		if v < 0 {
			return -1;
		} else if v > 0 {
			return +1;
		}
	}

	return 0;
}


order :: proc(value: Value) -> int {
	if value == nil {
		return 0;
	}
	switch v in value {
	case bool, string:
		return 1;
	case i128:
		return 2;
	case f64:
		return 3;
	}
	return -1;
}

match :: proc(x, y: Value) -> (Value, Value) {
	x, y := x, y;
	if order(x) > order(y) {
		y, x = match(y, x);
		return x, y;
	}

	switch a in x {
	case bool, string:
		return x, y;
	case i128:
		#partial switch b in y {
		case i128: return x, y;
		case f64: return x, i128(b);
		}
	case f64:
		#partial switch b in y {
		case i128: return x, f64(b);
		case f64: return x, y;
		}
	}

	return x, x;
}

unary_op :: proc(op: Op, x: Value) -> Value {
	switch x in x {
	case bool:
		#partial switch op {
		case .Not: return !x;
		}
	case i128:
		#partial switch op {
		case .Xor: return ~x;
		}
	case f64:
	case string:
	}
	return nil;
}

binary_op :: proc(x_: Value, op: Op, y_: Value) -> Value {
	x, y := match(x_, y_);
	if x == nil {
		return x;
	}
	switch x in x {
	case bool:
		y := y.(bool);
		#partial switch op {
		case .And:     return x & y;
		case .Or:      return x | y;
		case .Xor:     return x ~ y;
		case .Cmp_And: return x && y;
		case .Cmp_Or:  return x || y;
		case .Cmp_Eq:  return x == y;
		case .Not_Eq:  return x != y;
		}
	case i128:
		y := y.(i128);
		#partial switch op {
		case .Add:     return x + y;
		case .Sub:     return x - y;
		case .Mul:     return x * y;
		case .Quo:     return x / y;
		case .Mod:     return x % y;
		case .And:     return x & y;
		case .Or:      return x | y;
		case .Xor:     return x ~ y;
		case .Shl:     return x << u64(y);
		case .Shr:     return x >> u64(y);
		case .And_Not: return x &~ y;
		case .Cmp_Eq:  return x == y;
		case .Not_Eq:  return x != y;
		case .Lt:      return x < y;
		case .Gt:      return x > y;
		case .Lt_Eq:   return x <= y;
		case .Gt_Eq:   return x >= y;
		}
	case f64:
		y := y.(f64);
		#partial switch op {
		case .Add:     return x + y;
		case .Sub:     return x - y;
		case .Mul:     return x * y;
		case .Quo:     return x / y;
		case .Cmp_Eq:  return x == y;
		case .Not_Eq:  return x != y;
		case .Lt:      return x < y;
		case .Gt:      return x > y;
		case .Lt_Eq:   return x <= y;
		case .Gt_Eq:   return x >= y;
		}
	case string:
		y := y.(string);
		#partial switch op {
		case .Cmp_Eq:  return x == y;
		case .Not_Eq:  return x != y;
		case .Lt:      return x < y;
		case .Gt:      return x > y;
		case .Lt_Eq:   return x <= y;
		case .Gt_Eq:   return x >= y;
		}

	}

	return x;
}
