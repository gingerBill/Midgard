package frontend

import "intrinsics"
import "core:fmt"
import "core:strings"
import "core:thread"
import "../constant"

universal_scope: ^Scope;
universal_pkg: ^Package;


Checker :: struct {
	pkgs: []^Package,

	entities: [dynamic]^Entity,

	builtin_pkg: ^Package,
	init_scope: ^Scope,

	task_pool: thread.Pool,
	procs_to_check: [dynamic]Proc_Info,

	err: Error_Handler,
}

Proc_Info :: struct {
	entity: ^Procedure,
	decl: ^Decl_Info,
	type: ^Signature,
	body: ^Ast_Block_Stmt,
}


Entity_Path :: struct {
	parent: ^Entity_Path,
	path: [dynamic]^Entity, // TODO(bill): Should this be a linked list with a custom allocator?
}

Checker_Context :: struct {
	checker: ^Checker,

	pkg:   ^Package,
	file:  ^File,
	scope: ^Scope,
	decl:  ^Decl_Info,

	proc_name:      string,
	curr_proc_decl: ^Decl_Info,
	curr_proc_type: ^Signature,

	entity_path: ^Entity_Path,
}

Addressing_Mode :: enum u8 {
	Invalid,
	No_Value,
	Value,
	Variable,
	Constant,
	Type,
	Builtin,
}

addressing_mode_strings := [Addressing_Mode]string {
	.Invalid  = "invalid",
	.No_Value = "no value",
	.Value    = "value",
	.Variable = "variable",
	.Constant = "constant",
	.Type     = "type",
	.Builtin  = "builtin",
};

Type_And_Value :: struct {
	mode: Addressing_Mode,
	type: ^Type,
	value: constant.Value
}

Operand :: struct {
	using tav:  Type_And_Value,
	expr:       ^Ast_Expr,
	builtin_id: Builtin_Id,
}

SIZES := Standard_Sizes{
	word_size = 4,
	max_align = 8,
};

is_operand_nil :: proc(x: ^Operand) -> bool {
	return x.type == &btype[.untyped_nil];
}


push_entity_path :: proc(c: ^Checker_Context) {
	path := new(Entity_Path);
	if c.entity_path != nil {
		c.entity_path.parent = path;
	}
	c.entity_path = path;
}
pop_entity_path :: proc(c: ^Checker_Context) {
	assert(c.entity_path != nil);
	parent := c.entity_path.parent;
	delete(c.entity_path.path);
	free(c.entity_path);
	c.entity_path = parent;
}


operand_set_constant :: proc(x: ^Operand, tok: Token) {
	kind: Basic_Kind;
	#partial switch tok.kind {
	case .Integer:
		kind = .untyped_int;
	case .Float:
		kind = .untyped_float;
	case .Rune:
		kind = .untyped_rune;
	case .String:
		kind = .untyped_string;
	case:
		unreachable();
	}

	x.mode  = .Constant;
	x.type  = &btype[kind];
	x.value = constant_from_token(tok);
}

init_universal :: proc(c: ^Checker) {
	universal_scope = create_scope(nil, {}, {});
	universal_pkg = new(Package);
	universal_pkg.scope = universal_scope;
	universal_pkg.name = "builtin";

	for _, i in btype {
		bt := &btype[i];
		bt.variant = bt; // Initialize its variant here
		if strings.contains(bt.name, " ") {
			continue;
		}

		tname := new_type_name({}, universal_pkg, bt.name, bt);
		scope_insert(universal_scope, tname);
	}
	_, e_u8 := scope_lookup(universal_scope, "u8");
	scope_insert_with_name(universal_scope, e_u8, "byte");

	scope_insert(universal_scope, new_constant({}, universal_pkg, "false", &btype[.untyped_bool], false));
	scope_insert(universal_scope, new_constant({}, universal_pkg, "true", &btype[.untyped_bool], true));
	scope_insert(universal_scope, new_nil_entity({}, universal_pkg, "nil"));
}


check_pkgs :: proc(c: ^Checker, pkgs: []^Package) {
	init_universal(c);

	for pkg in pkgs {
		pkg.scope = create_scope(universal_scope, {}, {});
		pkg.scope.kind = .Package;
		pkg.scope.pkg = pkg;
		for file in pkg.files {
			pos, end: Pos;
			file.scope = create_scope(pkg.scope, pos, end);
			file.scope.kind = .File;
			file.scope.pkg  = pkg;
			file.scope.file = file;
		}
		// TODO(bill): Fix this logic
		if c.init_scope == nil {
			c.init_scope = pkg.scope;
		}
	}

	thread.pool_init(&c.task_pool, 1);
	defer thread.pool_destroy(&c.task_pool);

	// collect entities
	for pkg in pkgs {
		ctx := Checker_Context{};
		ctx.checker = c;
		ctx.pkg = pkg;

		for file in pkg.files {
			ctx.file = file;
			ctx.scope = file.scope;
			collect_entities(&ctx, file.decls[:]);
		}
	}

	{ // check package entities
		alias_list: [dynamic]^Type_Name;
		defer delete(alias_list);
		for e in c.entities {
			if tn, _ := e.variant.(^Type_Name); tn != nil && e.decl != nil && e.decl.is_alias {
				append(&alias_list, tn);
				continue;
			}

			ctx := Checker_Context{
				checker = c,
				pkg = e.pkg,
				scope = e.scope,
			};
			push_entity_path(&ctx);
			defer pop_entity_path(&ctx);

			check_entity_decl(&ctx, e, nil);
		}

		// NOTE(bill): Handle aliases after other declarations
		for e in alias_list {
			ctx := Checker_Context{
				checker = c,
				pkg = e.pkg,
				scope = e.scope,
			};
			push_entity_path(&ctx);
			defer pop_entity_path(&ctx);

			check_entity_decl(&ctx, e, nil);
		}

	}

	{ // Check Procedure bodies
		for _, i in c.procs_to_check {
			thread.pool_add_task(&c.task_pool, proc(task: ^thread.Task) {
				c := (^Checker)(task.data);
				info := c.procs_to_check[task.user_index];
				e := info.entity;
				d := info.decl;
				ctx := &Checker_Context{
					checker = c,
					pkg = e.pkg,
					decl = d,
					scope = info.type.scope,
				};
				check_proc_body(ctx, e, d);
			}, c, i);
		}
		
		thread.pool_start(&c.task_pool);
		thread.pool_wait_and_process(&c.task_pool);
	}
}
