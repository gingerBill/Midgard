package frontend

import "core:fmt"
import "core:strings"

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
	elems: map[string]^Entity,

	kind: Scope_Kind,
	pkg:  ^Package,
	file: ^File,
}


Decl_Info :: struct {
	parent: ^Decl_Info,

	lhs: []^Variable,
	scope: ^Scope,

	type_expr: ^Ast_Expr,
	init_expr: ^Ast_Expr,
	proc_decl: ^Ast_Proc_Decl,
	is_alias:  bool,

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
	if s == nil || e == nil {
		return nil;
	}
	if name == "" {
		return nil;
	}
	found := s.elems[name];
	if found != nil {
		return found;
	}

	s.elems[name] = e;
	if e.scope == nil {
		e.scope = s;
	}
	return nil;
}	

scope_lookup :: proc(s: ^Scope, name: string) -> ^Entity {
	if name == "" || name == "_" {
		return nil;
	}
	return s.elems[name];
}

scope_lookup_parent :: proc(scope: ^Scope, name: string) -> (^Scope, ^Entity) {
	for s := scope; s != nil; s = s.parent {
		if e := s.elems[name]; e != nil {
			return s, e;
		}
	}
	return nil, nil;
}


add_decl_dep :: proc(c: ^Checker_Context, to: ^Entity) {
	from := c.decl;
	if from == nil {
		return;
	}
	if to != nil {
		if found := to.decl; found != nil {
			from.deps[to] = true;
		}
	}
}

report_prev_decl :: proc(c: ^Checker_Context, prev: ^Entity) {
	assert(prev != nil);
	if prev.pos.line > 0 {
		check_error(prev.pos, "        other declaration of %s", prev.name);
	}
}

declare_entity :: proc(c: ^Checker_Context, scope: ^Scope, ident: ^Ast_Ident, e: ^Entity, pos: Pos) {
	assert(e != nil);
	e.ident = ident;
	e.pos = ident.pos;
	ident.entity = e;

	if e.name != "_" {
		if prev := scope_insert(scope, e); prev != nil {
			check_error(e.pos, "{} redeclared in this block", e.name);
			report_prev_decl(c, prev);
		}
	}
}

declare_pkg_entity :: proc(c: ^Checker_Context, ident: ^Ast_Ident, e: ^Entity, d: ^Decl_Info) {
	assert(e != nil);
	assert(ident.name == e.name);
	e.decl = d;

	append(&c.checker.entities, e);

	declare_entity(c, c.pkg.scope, ident, e, {});
}

check_arity_match :: proc(c: ^Checker_Context, s: ^Ast_Value_Spec) {
	l := len(s.names);
	r := len(s.values);

	switch {
	case r == 0:
		if s.type == nil {
			check_error(s.pos, "missing type or initial expression");
		} else if s.keyword.kind == .Const {
			check_error(s.pos, "missing initial expression");
		}
	case l < r:
		if l < len(s.values) {
			n := s.values[l];
			check_error(n.pos, "extra initial expression");
		} else {
			check_error(s.pos, "extra initial expression");
		}
	case l > r && r != 1:
		n := s.names[r];
		check_error(n.pos, "missing initial expression for {}", n.name);
	}
}

collect_entities :: proc(c: ^Checker_Context, decls: []^Ast_Decl) {
	for decl in decls {
		switch d in decl.variant {
		case ^Ast_Bad_Decl:
			// Ignore

		case ^Ast_Gen_Decl:
			for spec in d.specs {
				switch s in spec.variant {
				case ^Ast_Import_Spec:


				case ^Ast_Value_Spec:
					#partial switch s.keyword.kind {
					case .Const:
						for name, i in s.names {
							e := new_constant(name.pos, c.pkg, name.name, nil, i64(0));

							init: ^Ast_Expr = nil;
							if i < len(s.values) {
								init = s.values[i];
							}

							d := new_clone(Decl_Info{
								scope = c.file.scope,
								type_expr = s.type,
								init_expr = init,
							});
							declare_pkg_entity(c, name, e, d);
						}

						check_arity_match(c, s);

					case .Var:
						lhs := make([]^Variable, len(s.names));

						d1: ^Decl_Info = nil;
						if len(s.values) == 1 {
							d1 = new_clone(Decl_Info{
								scope = c.file.scope,
								lhs = lhs,
								type_expr = s.type,
								init_expr = s.values[0],
							});
						}

						for name, i in s.names {
							e := new_variable(name.pos, c.pkg, name.name, nil);
							lhs[i] = e;

							d := d1;
							if d == nil {
								init: ^Ast_Expr;
								if i < len(s.values) {
									init = s.values[i];
								}
								d = new_clone(Decl_Info{
									scope = c.file.scope,
									type_expr = s.type,
									init_expr = init,
								});
							}

							declare_pkg_entity(c, name, e, d);
						}

						check_arity_match(c, s);

					case:
						check_error(decl.pos, "invalid token: {}", s.keyword.kind);
					}

				case ^Ast_Type_Spec:
					e := new_type_name(s.name.pos, c.pkg, s.name.name, nil);
					declare_pkg_entity(c, s.name, e, new_clone(Decl_Info{
						scope = c.file.scope,
						type_expr = s.type,
						is_alias = pos_is_valid(s.assign),
					}));

				case ^Ast_Foreign_Spec:
					panic("TODO foreign declarations");
				}
			}

		case ^Ast_Proc_Decl:
			name := d.name.name;
			e := new_procedure(d.name.pos, c.pkg, name, nil);
			declare_entity(c, c.pkg.scope, d.name, e, {});
			e.decl = new_clone(Decl_Info{
				scope = c.file.scope,
				proc_decl = d,
			});

		case:
			check_error(decl.pos, "invalid AST, unknown Ast_Decl {:T}", d);
		}
	}

	for scope := c.pkg.scope.first_child; scope != nil; scope = scope.next {
		for name, e in scope.elems {
			if prev := scope_lookup(c.pkg.scope, e.name); prev != nil {
				check_error(e.pos, "{} redeclared in this package", e.name);
				report_prev_decl(c, prev);
			}
		}
	}
}

check_push_entity :: proc(c: ^Checker_Context, e: ^Entity) -> int {
	assert(e != nil);
	append(&c.entity_path.path, e);
	return len(c.entity_path.path)-1;
}
check_pop_entity :: proc(c: ^Checker_Context, loc := #caller_location) -> ^Entity {
	assert(condition=len(c.entity_path.path) > 0, loc=loc);
	e := pop(&c.entity_path.path);
	return e;
}

check_cycle_error :: proc(c: ^Checker_Context, cycle: []^Entity) {
	i := 0;
	e := cycle[i];
	check_error(e.pos, "illegal cycle in declaration of {}", e.name);
	for _ in cycle {
		check_error(e.pos, "    {} refers to", e.name);
		i += 1;
		if i >= len(cycle) {
			i = 0;
		}
		e = cycle[i];
	}
	check_error(e.pos, "    {}", e.name);
}

check_cycle :: proc(c: ^Checker_Context, entity: ^Entity) -> bool {
	if entity.colour < Entity_Colour_Grey {
		return false;
	}
	assert(entity.colour >= Entity_Colour_Grey);
	start := entity.colour - Entity_Colour_Grey;
	cycle := c.entity_path.path[start:];
	val_count := 0;
	def_count := 0;

	for e in cycle {
		assert(e != nil);
		switch e in e.variant {
		case ^Constant, ^Variable:
			val_count += 1;

		case ^Type_Name:
			def_count += 1;

		case ^Procedure:
			// Ignore

		case ^Import_Name, ^Builtin, ^Nil:
			unreachable();
		case:
			unreachable();
		}
	}

	if val_count == 0 && def_count == 0 {
		return false;
	}

	check_cycle_error(c, cycle);

	return false;
}


check_entity_decl :: proc(c: ^Checker_Context, entity: ^Entity, def: ^Named) {
	// entity_path_string :: proc(path: ^Entity_Path, allocator := context.temp_allocator) -> string {
	// 	b := strings.make_builder(allocator);
	// 	strings.write_byte(&b, '[');
	// 	for p, i in path.path {
	// 		if i > 0 {
	// 			strings.write_string(&b, "->");
	// 		}
	// 		strings.write_string(&b, p.name);
	// 	}
	// 	strings.write_byte(&b, ']');

	// 	return strings.to_string(b);
	// }
	// check_error({}, "-- checking {} ({}, path = {})", entity.name, entity.colour, entity_path_string(c.entity_path));
	// defer check_error({}, "=> {} ({})", entity.name, entity.colour);

	assert(entity != nil);

	// NOTE(bill): Three possible states: 
	// - Type unknown yet (white)
	// - Type in the process of being inferred (grey)
	// - Type fully inferred (black)

	// NOTE(bill): If an entity has been assigned a type, it means it is defined
	// and update the colour to black
	if entity.colour == Entity_Colour_White && entity.type != nil {
		entity.colour = Entity_Colour_Black;
		return;
	}

	set_to_black := false;
	defer if set_to_black {
		e := check_pop_entity(c);
		if e != nil {
			assert(e != nil);
			entity.colour = Entity_Colour_Black;
		}
	}

	switch entity.colour {
	case Entity_Colour_White:
		assert(entity.type == nil);
		idx := check_push_entity(c, entity);
		entity.colour = Entity_Colour_Grey + Entity_Colour(idx);
		set_to_black = true;

	case Entity_Colour_Black:
		assert(entity.type != nil);
		return;

	case: fallthrough; // Anything not White or Black is Grey (infinite shades of Grey)
	case Entity_Colour_Grey:
		// CYCLE!
		switch e in entity.variant {
		case ^Constant:
			if check_cycle(c, e) || e.type == nil {
				e.type = t_invalid;
			}
		case ^Variable:
			if check_cycle(c, e) || e.type == nil {
				e.type = t_invalid;
			}
		case ^Type_Name:
			if check_cycle(c, e) {
				// NOTE(bill): Stop the cycle
				e.type = t_invalid;
			}
		case ^Procedure:
			if check_cycle(c, e) {
				// NOTE(bill): Don't set
			}
		case ^Import_Name, ^Builtin, ^Nil:
			unreachable();
		case:
			unreachable();
		}
		return;
	}
	d := entity.decl;
	if d == nil {
		check_error(entity.pos, "{} should be declared!", entity.name);
		unreachable();
	}

	ctx := c^;
	ctx.scope = d.scope;

	switch e in entity.variant {
	case ^Constant:
		check_constant_decl(&ctx, e, d.type_expr, d.init_expr);
	case ^Variable:
		check_variable_decl(&ctx, e, d.lhs, d.init_expr);
	case ^Type_Name:
		check_type_decl(&ctx, e, d.type_expr, def, d.is_alias);
	case ^Procedure:
		check_procedure_decl(&ctx, e, d);

	case ^Import_Name, ^Builtin, ^Nil:
		unreachable();
	case:
		unreachable();
	}
}


check_init_constant :: proc(c: ^Checker_Context, lhs: ^Constant, x: ^Operand) {
	if x.mode == .Invalid || x.type == t_invalid || lhs.type == t_invalid {
		if lhs.type == nil {
			lhs.type = t_invalid;
		}
		return;
	}

	if x.mode != .Constant {
		check_error(x.expr.pos, "{} is not a constant", x);
		if lhs.type == nil {
			lhs.type = t_invalid;
		}
		return;
	}

	if lhs.type == nil {
		lhs.type = x.type;
	}

	check_assignment(c, x, lhs.type, "constant declaration");
	if x.mode == .Invalid {
		return;
	}
	lhs.value = x.value;
}

check_constant_decl :: proc(c: ^Checker_Context, e: ^Constant, type, init: ^Ast_Expr) {
	assert(e.type == nil);
	e.value = nil;

	if type != nil {
		t := check_type(c, type);
		if !is_const_type(t) {
			check_error(type.pos, "invalid constant type");
			e.type = t_invalid;
			return;
		}
		e.type = t;
	}
	x: Operand;
	if init != nil {
		check_expr(c, &x, init);
	}
	check_init_constant(c, e, &x);
}
check_variable_decl :: proc(c: ^Checker_Context, e: ^Variable, lhs: []^Variable, init: ^Ast_Expr) {
	// TODO(bill): check_variable_decl
}
check_type_decl :: proc(c: ^Checker_Context, e: ^Type_Name, type_expr: ^Ast_Expr, def: ^Named, is_alias: bool) {
	assert(e.type == nil);

	defer check_valid_type(c, e.type, nil);

	if is_alias {
		e.type = t_invalid;
		e.type = check_type(c, type_expr);
	} else {
		named := new(Named);
		set_underlying(def, named);
		e.type = named;
		named.original = check_type(c, type_expr, named);
		named.underlying = type_underlying(named);
	}
}
check_procedure_decl :: proc(c: ^Checker_Context, e: ^Procedure, d: ^Decl_Info) {
	// TODO(bill): check_procedure_decl
}




check_valid_type :: proc(c: ^Checker_Context, type: ^Type, path: ^[dynamic]^Entity) -> Named_State {
	#partial switch t in type.variant {
	case ^Array:
		return check_valid_type(c, t.elem, path);
	case ^Struct:
		for f in t.fields {
			if check_valid_type(c, f.type, path) == .Invalid {
				return .Invalid;
			}
		}
	case ^Tuple:
		for v in t.vars {
			if check_valid_type(c, v.type, path) == .Invalid {
				return .Invalid;
			}
		}

	case ^Named:
		if t.entity.pkg != c.pkg {
			return .Valid;
		}

		if t.underlying == t_invalid {
			t.state = .Invalid;
			return .Invalid;
		}

		#partial switch t.state {
		case .Unknown:
			t.state = .Processing;

			create_path := false;
			temp_path: [dynamic]^Entity;
			p := path;
			if p == nil {
				create_path = true;
				p = &temp_path;
			}
			defer if create_path {
				delete(temp_path);
			}

			append(p, t.entity);
			t.state = check_valid_type(c, t.original, p);
			pop(p);

		case .Processing: // Cycle!
			for tname, i in path {
				if t.entity.pkg != c.pkg {
					panic("internal-error for type cycle");
				}

				if tname == t.entity {
					spath: []^Entity;
					if path != nil {
						spath = path[i:];
					}
					check_cycle_error(c, spath);
					t.state = .Invalid;
					t.underlying = t_invalid;
					return t.state;
				}
			}	
		}
		return t.state;
	}

	return .Valid;
}
