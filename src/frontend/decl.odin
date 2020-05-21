package frontend

import "core:fmt"
import "core:strings"
import "core:strconv"

Scope_Kind :: enum {
	Normal,
	Package,
	File,
	Type,
	Procedure,
}
Scope :: struct {
	parent:     ^Scope,
	prev, next: ^Scope,
	first_child, last_child: ^Scope,
	elems: map[string]^Entity,

	pos: Pos,
	end: Pos,

	kind: Scope_Kind,
	pkg:  ^Package,
	file: ^File,
	comment: string,

	delayed: [dynamic]^Entity,
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

create_scope :: proc(parent: ^Scope, pos, end: Pos, comment: string = "") -> ^Scope {
	s := new_clone(Scope{
		parent = parent,
		comment = comment,
		pos = pos,
		end = end,
	});

	if parent != nil && parent != universal_scope {
		if parent.first_child == nil {
			parent.first_child = s;
			parent.last_child = s;
		} else {
			s.prev = parent.last_child;
			parent.last_child.next = s;
			parent.last_child = s;
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

scope_lookup_current :: proc(s: ^Scope, name: string) -> ^Entity {
	if name == "" || name == "_" {
		return nil;
	}
	return s.elems[name];
}

scope_lookup :: proc(scope: ^Scope, name: string) -> (found_scope: ^Scope, found_entity: ^Entity) {
	gone_thru_boundary := false;

	for s := scope; s != nil; s = s.parent {
		if e := s.elems[name]; e != nil {
			if gone_thru_boundary {
				if _, is_var := e.variant.(^Variable); is_var && (e.scope.kind != .File && e.scope.kind != .Package) {
					continue;
				}
			}

			found_scope = s;
			found_entity = e;
			return;
		}

		if s.kind == .Procedure {
			gone_thru_boundary = true;
		}
		if s.kind == .Package {
			gone_thru_boundary = true;
		}
	}
	return;
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

declare_entity :: proc(c: ^Checker_Context, scope: ^Scope, ident: ^Ast_Ident, e: ^Entity) {
	assert(e != nil);
	e.ident = ident;
	e.pos = ident.pos;
	ident.entity = e;
	e.foreign_library = c.foreign_library;

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

	declare_entity(c, c.pkg.scope, ident, e);
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
							e := new_constant(name.pos, c.pkg, name.name, nil, i128(0));

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

				case ^Ast_Export_Spec:
					// TODO
				}
			}

		case ^Ast_Proc_Decl:
			name := d.name.name;
			e := new_procedure(d.name.pos, c.pkg, name, nil);
			declare_entity(c, c.pkg.scope, d.name, e);
			e.decl = new_clone(Decl_Info{
				scope = c.file.scope,
				proc_decl = d,
			});
			append(&c.checker.entities, e);

		case ^Ast_Foreign_Decl:
			lib, allocated, ok := strconv.unquote_string(d.lib.text[1:len(d.lib.text)-1]);
			if !ok || lib == "" {
				check_error(d.tok.pos, "invalid foreign library string");
				if allocated {
					delete(lib);
				}
				lib = "";
			}

			prev_foreign_library := c.foreign_library;
			defer c.foreign_library = prev_foreign_library;
			c.foreign_library = lib;
			collect_entities(c, d.decls);

		case:
			check_error(decl.pos, "invalid AST, unknown Ast_Decl {:T}", d);
		}
	}

	for scope := c.pkg.scope.first_child; scope != nil; scope = scope.next {
		for name, e in scope.elems {
			if prev := scope_lookup_current(c.pkg.scope, e.name); prev != nil {
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
				e.type = &btype[.invalid];
			}
		case ^Variable:
			if check_cycle(c, e) || e.type == nil {
				e.type = &btype[.invalid];
			}
		case ^Type_Name:
			if check_cycle(c, e) {
				// NOTE(bill): Stop the cycle
				e.type = &btype[.invalid];
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
		check_variable_decl(&ctx, e, d.lhs, d.type_expr, d.init_expr);
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
	if x.mode == .Invalid || x.type == &btype[.invalid] || lhs.type == &btype[.invalid] {
		if lhs.type == nil {
			lhs.type = &btype[.invalid];
		}
		return;
	}

	if x.mode != .Constant {
		check_error(x.expr.pos, "{} is not a constant", x);
		if lhs.type == nil {
			lhs.type = &btype[.invalid];
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
			e.type = &btype[.invalid];
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
check_variable_decl :: proc(c: ^Checker_Context, e: ^Variable, lhs: []^Variable, type_expr, init: ^Ast_Expr) {
	if type_expr != nil {
		e.type = check_type(c, type_expr);
	}

	if init == nil {
		if type_expr == nil {
			e.type = &btype[.invalid];
		}
		return;
	}

	if lhs == nil || len(lhs) == 1 {
		assert(lhs == nil || lhs[0] == e);
		x: Operand;
		check_expr(c, &x, init);
		check_init_variable(c, e, &x, "variable declaration");
		return;
	}

	if type_expr != nil {
		for a in lhs {
			a.type = e.type;
		}
	}

	check_init_variables(c, lhs, {init}, {});
}

check_init_variable :: proc(c: ^Checker_Context, lhs: ^Variable, x: ^Operand, ctx: string) -> ^Type {
	if x.mode == .Invalid || x.type == &btype[.invalid] || lhs.type == &btype[.invalid] {
		if lhs.type == nil {
			lhs.type = &btype[.invalid];
		}
		return nil;
	}

	if lhs.type == nil {
		type := x.type;
		if type_is_untyped(type) {
			if type == &btype[.untyped_nil] {
				check_error(x.expr.pos, "use of untyped nil in {}", ctx);
				lhs.type = &btype[.invalid];
				return nil;
			}
			// type = default_type(type);
		}
		lhs.type = type;
	}

	check_assignment(c, x, lhs.type, ctx);
	if x.mode == .Invalid {
		return nil;
	}

	return x.type;
}

check_init_variables :: proc(c: ^Checker_Context, lhs: []^Variable, inits: []^Ast_Expr, pos: Pos) {
	// TODO(bill): check_init_variables
	// Tuple unpacking logic
}

check_type_decl :: proc(c: ^Checker_Context, e: ^Type_Name, type_expr: ^Ast_Expr, def: ^Named, is_alias: bool) {
	assert(e.type == nil);

	defer check_valid_type(c, e.type, nil);

	if is_alias {
		e.type = &btype[.invalid];
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
	assert(e.type == nil);

	sig := new_type(Signature);
	e.type = sig;
	proc_decl := d.proc_decl;
	check_proc_type(c, sig, proc_decl.type);

	if proc_decl.body != nil && (c.scope.kind != .Type && c.scope.kind != .Procedure) {
		info := Proc_Info{
			decl = d,
			type = sig,
			body = proc_decl.body,
			entity = e,
		};
		append(&c.checker.procs_to_check, info);
	}
}

check_proc_body :: proc(c: ^Checker_Context, e: ^Procedure, d: ^Decl_Info) {
	push_entity_path(c);
	defer pop_entity_path(c);

	check_stmt_list(c, nil, d.proc_decl.body.stmts);
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

		if t.underlying == &btype[.invalid] {
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
					t.underlying = &btype[.invalid];
					return t.state;
				}
			}
		}
		return t.state;
	}

	return .Valid;
}



check_decl_stmt_out_of_order :: proc(c: ^Checker_Context, decl: ^Ast_Decl) {
	switch d in decl.variant {
	case ^Ast_Bad_Decl:
		// Ignore

	case ^Ast_Gen_Decl:
		spec_loop: for spec in d.specs {
			switch s in spec.variant {
			case ^Ast_Import_Spec:
				check_error(s.pos, "import declarations are not allowed within a procedure");
				break spec_loop;

			case ^Ast_Export_Spec:
				check_error(s.pos, "export declarations are not allowed within a procedure");
				break spec_loop;

			case ^Ast_Value_Spec:
				#partial switch s.keyword.kind {
				case .Const:
					for name, i in s.names {
						e := new_constant(name.pos, c.pkg, name.name, nil, i128(0));

						init: ^Ast_Expr = nil;
						if i < len(s.values) {
							init = s.values[i];
						}
						e.decl = new_clone(Decl_Info{
							parent = c.decl,
							scope = c.scope,
							type_expr = s.type,
							init_expr = init,
						});
						append(&c.scope.delayed, e);

						declare_entity(c, c.scope, name, e);
					}
					check_arity_match(c, s);

				case .Var:
					// Handled later in order
				case:
					check_error(decl.pos, "invalid token: {}", s.keyword.kind);
				}

			case ^Ast_Type_Spec:
				e := new_type_name(s.name.pos, c.pkg, s.name.name, nil);
				declare_entity(c, c.scope, s.name, e);

				e.decl = new_clone(Decl_Info{
					parent = c.decl,
					scope = c.scope,
					type_expr = s.type,
					is_alias = pos_is_valid(s.assign),
				});
				append(&c.scope.delayed, e);
			}
		}

	case ^Ast_Proc_Decl:
		name := d.name.name;
		e := new_procedure(d.name.pos, c.pkg, name, nil);
		declare_entity(c, c.scope, d.name, e);
		e.decl = new_clone(Decl_Info{
			scope = c.scope,
			proc_decl = d,
		});
		append(&c.scope.delayed, e);


	case ^Ast_Foreign_Decl:
		check_error(decl.pos, "TODO {:T}", d);

	case:
		check_error(decl.pos, "invalid AST, unknown Ast_Decl {:T}", d);
	}
}

check_decl_stmt_in_order :: proc(c: ^Checker_Context, decl: ^Ast_Decl) {
	switch d in decl.variant {
	case ^Ast_Bad_Decl:
		// Ignore

	case ^Ast_Gen_Decl:
		spec_loop: for spec in d.specs {
			switch s in spec.variant {
			case ^Ast_Import_Spec:
				// ignore

			case ^Ast_Export_Spec:
				// ignore

			case ^Ast_Value_Spec:
				if s.keyword.kind == .Var {
					variables := make([]^Variable, len(s.names));
					for name, i in s.names {
						variables[i] = new_variable(name.pos, c.pkg, name.name, nil);
					}

					for v, i in variables {
						lhs: []^Variable = nil;
						init: ^Ast_Expr = nil;

						if len(s.values) == 1 {
							lhs = variables;
							init = s.values[0];
						} else {
							// sanity check
							if i < len(s.values) {
								init = s.values[i];
							}
						}

						check_variable_decl(c, v, lhs, s.type, init);
						if len(s.values) == 1 {
							break;
						}
					}

					check_arity_match(c, s);

					for name, i in s.names {
						declare_entity(c, c.scope, name, variables[i]);
					}
				}

			case ^Ast_Type_Spec:
			}
		}

	case ^Ast_Proc_Decl:

	case ^Ast_Foreign_Decl:

	}
}
