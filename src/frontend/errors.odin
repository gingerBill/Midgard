package frontend

import "core:fmt"
import "core:strings"
import "core:sync"

Error_Handler :: #type proc(pos: Pos, fmt: string, args: ..any);

error_mutex: sync.Mutex;
total_error_count: int;

init_error_system :: proc() {
	sync.mutex_init(&error_mutex);
}

check_error :: proc(pos: Pos, msg: string, args: ..any) {
	sync.mutex_lock(&error_mutex);
	default_error_handler(pos, msg, ..args);
	sync.mutex_unlock(&error_mutex);
}



default_error_handler :: proc(pos: Pos, msg: string, args: ..any) {
	total_error_count += 1;
	fullpath := "";
	if pos.file != nil {
		fullpath = pos.file.fullpath;
	}
	fmt.eprint(pos_string(pos), ' ');

	args_ := args;
	for arg_, i in args_ {
		arg := arg_;
		switch a in arg {
		case Operand:
			panic("internal error: ^Operand should be used");

		case ^Operand:   arg = operand_string(a);
		case Token:      arg = a.text;
		case Token_Kind: arg = tokens[a];

		case ^Type:      arg = type_string(a);
		case ^Basic:     arg = type_string(a);
		case ^Array:     arg = type_string(a);
		case ^Slice:     arg = type_string(a);
		case ^Struct:    arg = type_string(a);
		case ^Pointer:   arg = type_string(a);
		case ^Tuple:     arg = type_string(a);
		case ^Signature: arg = type_string(a);
		case ^Named:     arg = type_string(a);

		case Pos:                 arg = pos_string(a);

		case ^Ast_Expr:           arg = expr_string(a);
		case ^Ast_Bad_Expr:       arg = expr_string(a);
		case ^Ast_Key_Value_Expr: arg = expr_string(a);
		case ^Ast_Field_List:     arg = expr_string(a);
		case ^Ast_Field:          arg = expr_string(a);
		case ^Ast_Ident:          arg = expr_string(a);
		case ^Ast_Basic_Lit:      arg = expr_string(a);
		case ^Ast_Unary_Expr:     arg = expr_string(a);
		case ^Ast_Binary_Expr:    arg = expr_string(a);
		case ^Ast_Paren_Expr:     arg = expr_string(a);
		case ^Ast_Deref_Expr:     arg = expr_string(a);
		case ^Ast_Call_Expr:      arg = expr_string(a);
		case ^Ast_Comp_Lit:       arg = expr_string(a);
		case ^Ast_Proc_Lit:       arg = expr_string(a);
		case ^Ast_Selector_Expr:  arg = expr_string(a);
		case ^Ast_Index_Expr:     arg = expr_string(a);
		case ^Ast_Slice_Expr:     arg = expr_string(a);
		case ^Ast_Pointer_Type:   arg = expr_string(a);
		case ^Ast_Array_Type:     arg = expr_string(a);
		case ^Ast_Struct_Type:    arg = expr_string(a);
		case ^Ast_Proc_Type:      arg = expr_string(a);
		}

		args_[i] = arg;
	}

	fmt.eprintf(msg, ..args_);
	fmt.eprintf("\n");
}



operand_string :: proc(x: ^Operand, allocator := context.allocator) -> string {
	write_byte :: strings.write_byte;
	write_string :: strings.write_string;

	b := strings.make_builder(allocator);
	if x.expr != nil {
		write_expr(&b, x.expr);
	} else {
		#partial switch x.mode {
		case .Builtin:
			write_string(&b, builtin_procs[x.builtin_id].name);
		case .Type:
			write_type(&b, x.type);
		case .Constant:
			fmt.sbprint(&b, x.value);
		}
	}
	enclose := len(b.buf) != 0;
	if enclose {
		write_string(&b, " (");
	}

	has_type := false;
	switch x.mode {
	case .Invalid, .No_Value, .Type, .Builtin:
		// ignore

	case: fallthrough;
	case .Value, .Variable, .Constant:
		if x.type != nil {
			if type_is_untyped(x.type) {
				write_string(&b, x.type.variant.(^Basic).name);
				write_byte(&b, ' ');
				break;
			}
			has_type = true;
		}
	}

	write_string(&b, addressing_mode_strings[x.mode]);

	if has_type {
		if x.type != &btype[.invalid] {
			write_string(&b, " of type ");
			write_type(&b, x.type);
		} else {
			write_string(&b, " with invalid type");
		}
	}

	if enclose {
		write_byte(&b, ')');
	}

	return strings.to_string(b);
}

expr_string :: proc(x: ^Ast_Expr, allocator := context.temp_allocator) -> string {
	b := strings.make_builder(allocator);
	write_expr(&b, x);
	return strings.to_string(b);
}

write_expr :: proc(b: ^strings.Builder, expr: ^Ast_Expr) {
	write_byte :: strings.write_byte;
	write_string :: strings.write_string;

	switch x in expr.variant {
	case:
		write_string(b, "(bad expr)");
	case ^Ast_Bad_Expr:
		write_string(b, "(bad expr)");
	case ^Ast_Key_Value_Expr:
		write_string(b, "(bad expr - key value)");
	case ^Ast_Field_List:
		write_string(b, "(bad expr - field list)");
	case ^Ast_Field:
		write_string(b, "(bad expr - field)");

	case ^Ast_Ident:
		write_string(b, x.name);

	case ^Ast_Basic_Lit:
		write_string(b, x.tok.text);

	case ^Ast_Unary_Expr:
		write_string(b, x.op.text);
		write_expr(b, x.expr);

	case ^Ast_Binary_Expr:
		write_expr(b, x.left);
		write_byte(b, ' ');
		write_string(b, x.op.text);
		write_byte(b, ' ');
		write_expr(b, x.right);

	case ^Ast_Paren_Expr:
		write_byte(b, '(');
		write_expr(b, x.expr);
		write_byte(b, ')');

	case ^Ast_Deref_Expr:
		write_expr(b, x.expr);
		write_byte(b, '^');

	case ^Ast_Call_Expr:
		write_expr(b, x.expr);
		write_byte(b, '(');
		for arg, i in x.args {
			if i > 0 {
				write_string(b, ", ");
			}
			write_expr(b, arg);
		}
		write_byte(b, ')');

	case ^Ast_Comp_Lit:

	case ^Ast_Proc_Lit:
		write_byte(b, '(');
		write_expr(b, x.type);
		write_string(b, " literal)");

	case ^Ast_Selector_Expr:
		write_expr(b, x.x);
		write_byte(b, '.');
		write_expr(b, x.sel);

	case ^Ast_Index_Expr:
		write_expr(b, x.x);
		write_byte(b, '[');
		write_expr(b, x.index);
		write_byte(b, ']');

	case ^Ast_Slice_Expr:
		write_expr(b, x.x);
		write_byte(b, '[');
		if x.low != nil {
			write_expr(b, x.low);
		}
		write_byte(b, ':');
		if x.high != nil {
			write_expr(b, x.high);
		}

		write_byte(b, ']');

	case ^Ast_Pointer_Type:
		write_byte(b, '^');
		write_expr(b, x.elem);

	case ^Ast_Array_Type:
		write_byte(b, '[');
		if x.len != nil {
			write_expr(b, x.len);
		}
		write_byte(b, ']');
		write_expr(b, x.elem);

	case ^Ast_Struct_Type:
		write_string(b, "struct{");
		write_field_list(b, x.fields, "; ");

	case ^Ast_Proc_Type:
		write_string(b, "proc");
		write_signature_expr(b, x);
	}
}


write_signature_expr :: proc(b: ^strings.Builder, sig: ^Ast_Proc_Type) {
	write_byte :: strings.write_byte;
	write_string :: strings.write_string;

	write_byte(b, '(');
	write_field_list(b, sig.params, ", ");
	write_byte(b, ')');

	res := sig.results;
	n := field_count(res);
	if n == 0 {
		return;
	}
	write_byte(b, ' ');
	if n == 1 && len(res.list[0].names) == 0 {
		write_expr(b, res.list[0].type);
		return;
	}

	write_byte(b, '(');
	write_field_list(b, res, ", ");
	write_byte(b, ')');
}

write_field_list :: proc(b: ^strings.Builder, fields: ^Ast_Field_List, sep: string) {
	write_byte :: strings.write_byte;
	write_string :: strings.write_string;

	for f, i in fields.list {
		if i > 0 {
			write_string(b, sep);
		}

		for name, j in f.names {
			if j > 0 {
				write_string(b, ", ");
				write_string(b, name.name);
			}
		}

		if len(f.names) > 0 {
			write_byte(b, ' ');
		}

		write_expr(b, f.type);
	}
}

type_string :: proc(type: ^Type, allocator := context.temp_allocator) -> string {
	b := strings.make_builder(allocator);
	write_type(&b, type);
	return strings.to_string(b);
}


write_type :: proc(b: ^strings.Builder, type: ^Type) {
	write_tuple :: proc(b: ^strings.Builder, t: ^Tuple, parens: bool) {
		if len(t.vars) == 1 && t.vars[0].name == "" {
			write_type(b, t.vars[0].type);
			return;
		}
		if parens do write_byte(b, '(');
		for var, i in t.vars {
			if i > 0 {
				write_string(b, ", ");
			}
			if var.name != "" {
				write_string(b, var.name);
				write_byte(b, ' ');
			}
			write_type(b, var.type);
		}
		if parens do write_byte(b, ')');
	}

	write_byte :: strings.write_byte;
	write_i64 :: strings.write_i64;
	write_string :: strings.write_string;

	if type == nil {
		write_string(b, "<nil>");
		return;
	}

	switch t in type.variant {
	case ^Basic:
		write_string(b, t.name);
	case ^Array:
		write_byte(b, '[');
		write_i64(b, t.len);
		write_byte(b, ']');
		write_type(b, t.elem);

	case ^Slice:
		write_string(b, "[]");
		write_type(b, t.elem);

	case ^Struct:
		write_string(b, "struct {");
		for field, i in t.fields {
			if i > 0 {
				write_string(b, "; ");
			}
			if field.name != "" {
				write_string(b, field.name);
				write_byte(b, ' ');
			}
			write_type(b, field.type);
		}
		write_byte(b, '}');

	case ^Pointer:
		write_byte(b, '^');
		write_type(b, t.elem);

	case ^Tuple:
		write_tuple(b, t, true);

	case ^Signature:
		write_string(b, "proc");
		write_tuple(b, t.params, true);
		if t.results != nil {
			write_tuple(b, t.results, true);
		}

	case ^Named:
		write_string(b, t.entity.name);
	}

}
