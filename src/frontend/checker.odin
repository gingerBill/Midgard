package frontend

import "intrinsics"
import "core:fmt"
import "core:strings"
import "../constant"

universal_scope: ^Scope;
universal_pkg: ^Package;


Checker :: struct {
	pkgs: []^Package,

	entities: [dynamic]^Entity,

	builtin_pkg: ^Package,
	init_scope: ^Scope,

	procs_to_check: [dynamic]Proc_Info,

	err: Error_Handler,
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


operand_to_string :: proc(x: ^Operand, allocator := context.allocator) -> string {
	b := strings.make_builder(allocator);
	return strings.to_string(b);
}

operand_set_constant :: proc(x: ^Operand, tok: Token) {
	kind: Basic_Kind;
	#partial switch tok.kind {
	case .Integer:
		kind = .isize;
	case .Float:
		kind = .f64;
	case .Rune:
		kind = .rune;
	case .String:
		kind = .string;
	case:
		unreachable();
	}

	x.mode  = .Constant;
	x.type  = &basic_types[kind];
	x.value = constant_from_token(tok);
}

init_universal :: proc(c: ^Checker) {
	universal_scope = create_scope(nil);
	universal_pkg = new(Package);
	universal_pkg.scope = universal_scope;
	universal_pkg.name = "builtin";

	for _, i in basic_types {
		bt := &basic_types[i];
		bt.variant = bt; // Initialize its variant here
		if strings.contains(bt.name, " ") {
			continue;
		}

		tname := new_type_name({}, universal_pkg, bt.name, bt);
		scope_insert(universal_scope, tname);
	}
	_, e_u8 := scope_lookup_parent(universal_scope, "u8");
	scope_insert_with_name(universal_scope, e_u8, "byte");
}


check_pkgs :: proc(c: ^Checker, pkgs: []^Package) {
	init_universal(c);

	for pkg in pkgs {
		pkg.scope = create_scope(universal_scope);
		pkg.scope.kind = .Package;
		pkg.scope.pkg = pkg;
		for file in pkg.files {
			file.scope = create_scope(pkg.scope);
			file.scope.kind = .File;
			file.scope.pkg  = pkg;
			file.scope.file = file;
		}
		// TODO(bill): Fix this logic
		if c.init_scope == nil {
			c.init_scope = pkg.scope;
		}
	}


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

			ctx := Checker_Context{};
			ctx.checker = c;
			ctx.pkg = e.pkg;
			ctx.scope = e.scope;
			push_entity_path(&ctx);
			defer pop_entity_path(&ctx);

			check_entity_decl(&ctx, e, nil);
		}

		// NOTE(bill): Handle aliases after other declarations
		for e in alias_list {
			ctx := Checker_Context{};
			ctx.checker = c;
			ctx.pkg = e.pkg;
			ctx.scope = e.scope;
			push_entity_path(&ctx);
			defer pop_entity_path(&ctx);

			check_entity_decl(&ctx, e, nil);
		}
	}
}
