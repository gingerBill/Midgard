package frontend

import "../constant"

Entity_Flag :: enum {
	Used,
	Exported,
	Field,
}

Entity_Flags :: bit_set[Entity_Flag; u32];

Entity_Colour :: distinct u32;
Entity_Colour_White :: Entity_Colour(0);
Entity_Colour_Black :: Entity_Colour(1);
Entity_Colour_Grey  :: Entity_Colour(2);

Entity :: struct {
	pos:   Pos,
	pkg:   ^Package,
	name:  string,
	type:  ^Type,

	decl: ^Decl_Info,

	ident: ^Ast_Ident,
	scope: ^Scope,

	colour: Entity_Colour,

	link_name:       string,
	foreign_library: ^Entity,
	flags:           Entity_Flags,

	variant: union {
		^Constant,
		^Variable,
		^Type_Name,
		^Procedure,
		^Import_Name,
		^Builtin,
		^Nil,
	},
}

Constant :: struct {
	using entity: Entity,
	value: constant.Value,
}
Variable :: struct {
	using entity: Entity,
	field_index: i32,
	field_src_index: i32,
}
Type_Name :: struct {
	using entity: Entity,
	underlying: ^Type,
}
Procedure :: struct {
	using entity: Entity,
}
Import_Name :: struct {
	using entity: Entity,
	import_path: string,
	import_name: string,
	import_scope: ^Scope,
}
Builtin :: struct {
	using entity: Entity,
	built_id: Builtin_Id,
}
Nil :: struct {
	using entity: Entity,
}


entity_colour_for :: proc(t: ^Type) -> Entity_Colour {
	if t != nil {
		return Entity_Colour_Black;
	}
	return Entity_Colour_White;
}

new_variable :: proc(pos: Pos, pkg: ^Package, name: string, type: ^Type) -> ^Variable {
	e := new(Variable);
	e.variant = e;
	e.pkg = pkg;
	e.name = name;
	e.type = type;
	e.colour = entity_colour_for(type);

	return e;
}

new_constant :: proc(pos: Pos, pkg: ^Package, name: string, type: ^Type, value: constant.Value) -> ^Constant {
	e := new(Constant);
	e.variant = e;
	e.pkg = pkg;
	e.name = name;
	e.type = type;
	e.colour = entity_colour_for(type);
	e.value = value;

	return e;
}
new_procedure :: proc(pos: Pos, pkg: ^Package, name: string, type: ^Type) -> ^Procedure {
	e := new(Procedure);
	e.variant = e;
	e.pkg = pkg;
	e.name = name;
	e.type = type;
	e.colour = entity_colour_for(type);

	return e;
}


new_type_name :: proc(pos: Pos, pkg: ^Package, name: string, type: ^Type) -> ^Type_Name {
	e := new(Type_Name);
	e.variant = e;
	e.pkg = pkg;
	e.name = name;
	e.type = type;
	e.colour = entity_colour_for(type);
	return e;
}

new_field :: proc(pos: Pos, pkg: ^Package, name: string, type: ^Type) -> ^Variable {
	f := new_variable(pos, pkg, name, type);
	f.flags |= {.Field};
	return f;
}


type_name_is_alias :: proc(tname: ^Type_Name) -> bool {
	if tname.type == nil {
		return false;
	}
	#partial switch t in tname.type.variant {
	case ^Basic:
		return tname.pkg != nil || t.name != tname.name;
	case ^Named:
		return tname != t.entity;
	}
	return true;
}

set_underlying :: proc(def: ^Named, type: ^Type) {
	if def != nil {
		def.underlying = type;
	}
}
