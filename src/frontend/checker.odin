package wasm_frontend

import "core:fmt"
import "intrinsics"

Checker :: struct {
	pkgs: []^Package,

	entities: [dynamic]^Entity,

	builtin_pkg: ^Package,
	init_scope: ^Scope,

	procs_to_check: [dynamic]Proc_Info,

	err: Error_Handler,
}

Checker_Context :: struct {
	using checker: ^Checker,

	pkg:   ^Package,
	file:  ^File,
	scope: ^Scope,
	decl:  ^Decl_Info,

	proc_name:      string,
	curr_proc_decl: ^Decl_Info,
	curr_proc_type: ^Type_Proc,

	type_path:      ^[dynamic]^Entity,
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
	value: Constant_Value
}

Operand :: struct {
	using tav:  Type_And_Value,
	expr:       ^Ast_Expr,
	builtin_id: Builtin_Id,
}

Scope_Kind :: enum {
	Normal,
	Package,
	File,
	Type,
}
Scope :: struct {
	parent:     ^Scope,
	prev, next: ^Scope,
	first_child, last_child: ^Scope,
	elements: map[string]^Entity,

	kind: Scope_Kind,
	pkg:  ^Package,
	file: ^File,
}

Entity :: struct {
	name:  string,
	pos:   Pos,
	ident: ^Ast_Ident,
	type:  ^Type,
	scope: ^Scope,
	pkg:   ^Package,

	link_name:   string,
	is_exported: bool,
	is_foreign:  bool,

	variant: union {
		Entity_Constant,
		Entity_Variable,
		Entity_Typename,
		Entity_Procedure,
		Entity_Import_Name,
		Entity_Library_Name,
		Entity_Builtin,
		Entity_Nil,
	},
}

Constant_Value :: union{bool, i64, u64, f64, string};

Entity_Constant :: struct {
	value: Constant_Value,
}
Entity_Variable :: struct {
	field_index: i32,
	field_src_index: i32,
}
Entity_Typename :: struct {
}
Entity_Procedure :: struct {
}
Entity_Import_Name :: struct {
	path: string,
	name: string,
	scope: ^Scope,
}
Entity_Library_Name :: struct {
	path: string,
	name: string,
}
Entity_Builtin :: struct {
	id: Builtin_Id,
}
Entity_Nil :: struct {
}


Decl_Info :: struct {
	parent: ^Decl_Info,

	entity: ^Entity,
	scope: ^Scope,

	type_expr: ^Ast_Expr,
	init_expr: ^Ast_Expr,
	proc_lit:  ^Ast_Proc_Lit,

	deps: map[^Entity]bool,
}

Proc_Info :: struct {
	file: ^File,
	decl: ^Decl_Info,
	type: ^Type,
	body: ^Ast_Block_Stmt,
}

create_scope :: proc(parent: ^Scope) -> ^Scope {
	s := new_clone(Scope{
		parent = parent,
	});

	if parent != nil {
		if parent.first_child == nil {
			parent.first_child = s;
			parent.last_child = s;
		} else {
			parent.last_child.next = s;
			parent.last_child.prev = parent.last_child;
			parent.last_child = s.next;
		}
	}
	return s;
}

scope_insert :: proc(s: ^Scope, e: ^Entity) -> ^Entity {
	return scope_insert_with_name(s, e, e.name);
}

scope_insert_with_name :: proc(s: ^Scope, e: ^Entity, name: string) -> ^Entity {
	if name == "" {
		return nil;
	}
	found := s.elements[name];
	if found != nil {
		return found;
	}

	s.elements[name] = e;
	if e.scope == nil {
		e.scope = s;
	}
	return nil;
}	

collect_entities :: proc(c: ^Checker_Context, decls: []^Ast_Decl) {
	for decl in decls {
		switch d in decl.derived {
		case Ast_Bad_Decl:
			// Ignore

		case Ast_Gen_Decl:
			fmt.println(d.tok.text);
			for spec in d.specs {
				switch s in spec.derived {
				case Ast_Import_Spec:
				case Ast_Value_Spec:
					#partial switch s.keyword.kind {
					case .Const:
						type := s.type;
						for name, i in s.names {
							init: ^Ast_Expr;
							if i < len(s.values) {
								init = s.values[i];
							}
						}
					case .Var:
						
					case:
						c.err(decl.pos, "invalid token: {}", s.keyword.kind);
					}
				case Ast_Type_Spec:
				case Ast_Foreign_Spec:
				}
			}

		case Ast_Proc_Decl:
			fmt.println(d.type.tok.text, d.name.name);

		case:
			c.err(decl.pos, "invalid AST, unknown Ast_Decl {:T}", d);
		}
	}
}


check_pkgs :: proc(c: ^Checker, pkgs: []^Package) {
	for pkg in pkgs {
		pkg.scope = create_scope(nil);
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
}
