package frontend

import "core:fmt"

import "../constant"

check_ident :: proc(c: ^Checker_Context, x: ^Operand, ident: ^Ast_Ident, def: ^Named, want_type: bool) {
	assert(ident != nil);

	x.mode = .Invalid;
	x.expr = ident;
	x.type = t_invalid;

	scope, e := scope_lookup_parent(c.scope, ident.name);
	if e == nil {
		if ident.name == "_" {
			check_error(ident.pos, "cannot use _ as a value or type");
		} else {
			check_error(ident.pos, "undeclared name: {}", ident.name);
		}
		return;
	}

	type := e.type;
	if _, got_type := e.variant.(^Type_Name); type == nil || got_type && want_type {
		check_entity_decl(c, e, def);
		type = e.type;
	}
	assert(type != nil);


	switch e in e.variant {
	case ^Import_Name:
		check_error(e.pos, "use of package {} not in selector expression", e.name);
		return;
	case ^Constant:
		add_decl_dep(c, e);
		if type == t_invalid {
			return;
		}
		x.value = e.value;
		assert(x.value != nil);
		x.mode = .Constant;

	case ^Variable:
		if e.pkg == c.pkg {
			e.flags |= {.Used};
		}
		add_decl_dep(c, e);
		if type == t_invalid {
			return;
		}
		x.mode = .Variable;

	case ^Type_Name:
		x.mode = .Type;

	case ^Procedure:
		add_decl_dep(c, e);
		x.mode = .Value;

	case ^Builtin:
		x.mode = .Builtin;
		x.builtin_id = e.built_id;

	case ^Nil:
		x.mode = .Value;

	case:
		unreachable();
	}

	x.type = type;
}

check_type :: proc(c: ^Checker_Context, e: ^Ast_Expr, def: ^Named = nil) -> ^Type {
	t := check_type_internal(c, e, def);
	assert(type_is_typed(t));
	add_type_and_value(c, e, .Type, t, nil);
	return t;
}

check_type_internal :: proc(c: ^Checker_Context, expr: ^Ast_Expr, def: ^Named) -> ^Type {
	assert(expr != nil);
	#partial switch e in expr.variant {
	case ^Ast_Bad_Expr:
		// ignore

	case ^Ast_Ident:
		x: Operand;
		check_ident(c, &x, e, def, true);

		#partial switch x.mode {
		case .Type:
			type := x.type;
			set_underlying(def, type);
			return type;
		case .Invalid:
			// Ignore the error
		case .No_Value:
			check_error(e.pos, "{} used as type", e.name);
		case:
			check_error(e.pos, "{} is not a type", e.name);
		}

	case ^Ast_Paren_Expr:
		return check_type(c, e.expr, def);

	case ^Ast_Selector_Expr:
		panic("todo: ^Ast_Selector_Expr");

	case ^Ast_Pointer_Type:

		type := new_type(Pointer);
		set_underlying(def, type);

		push_entity_path(c);
		type.elem = check_type(c, e.elem);
		pop_entity_path(c);

		return type;

	case ^Ast_Array_Type:
		if e.len != nil {
			type := new_type(Array);
			set_underlying(def, type);
			type.len = check_array_length(c, e.len);
			type.elem = check_type(c, e.elem);
			return type;
		} else {
			type := new_type(Slice);
			set_underlying(def, type);
			push_entity_path(c);
			type.elem = check_type(c, e.elem);
			pop_entity_path(c);
			return type;
		}

	case ^Ast_Struct_Type:
		type := new_type(Struct);
		set_underlying(def, type);
		check_struct_type(c, type, e);
		return type;

	case ^Ast_Proc_Type:
		type := new_type(Signature);
		set_underlying(def, type);
		check_proc_type(c, type, e);
		return type;

	case:
		check_error(expr.pos, "expression is not a type");
	}

	type := t_invalid;
	set_underlying(def, type);
	return type;
}



check_array_length :: proc(c: ^Checker_Context, n: ^Ast_Expr) -> i64 {
	x: Operand;
	check_expr(c, &x, n);
	if x.mode != .Constant {
		if x.mode != .Invalid {
			check_error(n.pos, "array length {} must be constant", &x);
		}
		return -1;
	}
	if type_is_untyped(x.type) || type_is_integer(x.type) {
		if val, ok := constant.as_int(x.value); ok {
			if representable_as_constant(c, val, &basic_types[.isize], nil) {
				if n, ok := constant.as_i64(val); ok && n >= 0 {
					return i64(n);
				}
				check_error(n.pos, "invalid array length {}", &x);
				return -1;
			}
		}
	}
	check_error(n.pos, "array length {} must be integer", &x);
	return -1;
}
check_struct_type :: proc(c: ^Checker_Context, type: ^Struct, expr: ^Ast_Struct_Type) {
	list := expr.fields;
	if list == nil {
		return;
	}
	fields: [dynamic]^Variable;


	for f in list.list {
		assert(f != nil);
		assert(f.type != nil);
		ftype := check_type(c, f.type);
		assert(len(f.names) > 0);
		for ident in f.names {
			name := ident.name;
			field := new_field(ident.pos, c.pkg, name, ftype);
			field.field_index = i32(len(fields));
			field.field_src_index = i32(len(fields));
			append(&fields, field);
			ident.entity = field;
		}
	}

	type.fields = fields[:];
}
check_proc_type :: proc(c: ^Checker_Context, type: ^Signature, expr: ^Ast_Proc_Type) {
	return;
}
