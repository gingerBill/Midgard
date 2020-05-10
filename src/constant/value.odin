package frontend

// import "core:strconv"
// import "core:strings"
// import "core:unicode/utf8"

Value :: union{bool, string, rune, i64, u64, f64};


as_int :: proc(value: Value) -> (res: Value, ok: bool) {
	switch v in value {
	case bool:
		return;
	case string:
		return;
	case rune:
		res = i64(v);
		ok = true;
	case i64:
		res = i64(v);
		ok = true;
	case u64:
		res = u64(v);
		ok = true;
	case f64:
		res = i64(v);
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
	case rune:
		res = i64(v);
		ok = true;
	case i64:
		res = i64(v);
		ok = true;
	case u64:
		res = i64(v);
		ok = true;
	case f64:
		res = i64(v);
		ok = true;
	}

	return;
}


