package frontend

import "core:strings"

Standard_Sizes :: struct {
	word_size: i64,
	max_align: i64,
}

Type :: struct {
	cached_size:  i64,
	cached_align: i64,
	variant: union {
		^Basic,
		^Array,
		^Slice,
		^Struct,
		^Pointer,
		^Tuple,
		^Signature,
		^Named,
	},
}

Basic_Kind :: enum u8 {
	Invalid,

	bool,

	i8,
	i16,
	i32,
	i64,
	isize,
	u8,
	u16,
	u32,
	u64,
	usize,

	rune,

	f32,
	f64,

	rawptr,
	string,

	untyped_nil,

	byte = u8,
}

Basic_Flag :: enum u8 {
	Boolean,
	Integer,
	Unsigned,
	Float,
	Pointer,
	String,
	Rune,
	Untyped,
}

Basic_Flags :: distinct bit_set[Basic_Flag; u32];

Basic_Flags_Numeric :: Basic_Flags{.Integer, .Float};
Basic_Flags_Ordered :: Basic_Flags{.Integer, .Float, .String, .Pointer, .Rune};
Basic_Flags_Ordered_Numeric :: Basic_Flags_Ordered & Basic_Flags_Numeric;
Basic_Flags_Constant_Type :: Basic_Flags{.Boolean, .Integer, .Unsigned, .Float, .Pointer, .String, .Rune};

Basic :: struct {
	using base: Type,
	kind:  Basic_Kind,
	flags: Basic_Flags,
	size:  i64,
	align: i64,
	name:  string,
}


basic_types := [Basic_Kind]Basic{
	.Invalid = {},

	.bool   = {{}, .bool,   {.Boolean},            1,           1,  "bool"},

	.i8     = {{}, .i8,     {.Integer},            1,           1,  "i8"},
	.i16    = {{}, .i16,    {.Integer},            2,           2,  "i16"},
	.i32    = {{}, .i32,    {.Integer},            4,           4,  "i32"},
	.i64    = {{}, .i64,    {.Integer},            8,           8,  "i64"},
	.isize  = {{}, .isize,  {.Integer},            -1,          -1, "isize"},
	.u8     = {{}, .u8,     {.Integer, .Unsigned}, 1,           1,  "u8"},
	.u16    = {{}, .u16,    {.Integer, .Unsigned}, 2,           2,  "u16"},
	.u32    = {{}, .u32,    {.Integer, .Unsigned}, 4,           4,  "u32"},
	.u64    = {{}, .u64,    {.Integer, .Unsigned}, 8,           8,  "u64"},
	.usize  = {{}, .usize,  {.Integer, .Unsigned}, -1,          -1, "usize"},

	.rune   = {{}, .rune,   {.Rune},               4,           4,  "rune"},

	.f32    = {{}, .f32,    {.Float},              4,           4,  "f32"},
	.f64    = {{}, .f64,    {.Float},              8,           8,  "f64"},

	.rawptr = {{}, .rawptr, {.Pointer},            -1,          -1, "rawptr"},
	.string = {{}, .string, {.String},             -2,          -1, "string"},

	.untyped_nil = {{}, .untyped_nil, {.Untyped},  -1,          -1, "untyped nil"},
};

t_invalid     := &basic_types[.Invalid];
t_bool        := &basic_types[.bool];
t_i8          := &basic_types[.i8];
t_i16         := &basic_types[.i16];
t_i32         := &basic_types[.i32];
t_i64         := &basic_types[.i64];
t_isize       := &basic_types[.isize];
t_u8          := &basic_types[.u8];
t_u16         := &basic_types[.u16];
t_u32         := &basic_types[.u32];
t_u64         := &basic_types[.u64];
t_usize       := &basic_types[.usize];
t_rune        := &basic_types[.rune];
t_f32         := &basic_types[.f32];
t_f64         := &basic_types[.f64];
t_rawptr      := &basic_types[.rawptr];
t_string      := &basic_types[.string];
t_untyped_nil := &basic_types[.untyped_nil];


Array :: struct {
	using base: Type,
	len:  i64,
	elem: ^Type,
}

Slice :: struct {
	using base: Type,
	elem: ^Type,
}

Struct :: struct {
	using base: Type,
	fields: []^Variable,
	offsets: []i64,
}

Pointer :: struct {
	using base: Type,
	elem: ^Type,
}

Tuple :: struct {
	using base: Type,
	vars: []^Variable,
	offsets: []i64,
}

Signature :: struct {
	using base: Type,
	scope:   ^Scope,
	params:  ^Tuple,
	results: ^Tuple,
}

Named_State :: enum {
	Unknown,
	Processing,
	Invalid,
	Valid,
}

Named :: struct {
	using base: Type,
	state:      Named_State,
	entity:     ^Type_Name,
	original:   ^Type,
	underlying: ^Type,
}

new_type :: proc($T: typeid) -> ^T {
	ptr := new(T);
	ptr.variant = ptr;

	_x: Type = ptr^; // enforce check
	_ = _x;

	return ptr;
}


type_underlying :: proc(t: ^Type) -> ^Type {
	if tname, ok := t.variant.(^Named); ok {
		return tname.underlying;
	}
	return t;
}

type_align_of :: proc(s: ^Standard_Sizes, t: ^Type) -> i64 {
	switch t in t.variant {
	case ^Basic:
		align := t.align;
		if align < 0 {
			align = -s.word_size;
		}
		return clamp(align, 1, s.max_align);

	case ^Array:
		return type_align_of(s, t.elem);

	case ^Slice, ^Pointer, ^Signature:
		return s.word_size;

	case ^Struct:
		max := i64(1);
		for f in t.fields {
			if a := type_align_of(s, f.type); a > max {
				max = a;
			}
		}
		return max;

	case ^Tuple:
		max := i64(1);
		for v in t.vars {
			if a := type_align_of(s, v.type); a > max {
				max = a;
			}
		}
		return max;

	case ^Named:
		return type_align_of(s, t.underlying);
	}
	return 1;
}

type_size_of :: proc(s: ^Standard_Sizes, t: ^Type) -> i64 {
	switch t in t.variant {
	case ^Basic:
		size := t.size;
		if size < 0 {
			size = -s.word_size;
		}
		return size;

	case ^Array:
		n := t.len;
		if n <= 0 {
			return 0;
		}
		a := type_align_of(s, t.elem);
		z := type_size_of(s, t.elem);
		return align_formula(z, a)*(n-1) + z;

	case ^Slice:
		return 2*s.word_size;
	case ^Pointer:
		return s.word_size;
	case ^Signature:
		return s.word_size;

	case ^Struct:
		n := len(t.fields);
		if n == 0 {
			return 0;
		}
		return t.offsets[n-1] + type_size_of(s, t.fields[n-1].type);

	case ^Tuple:
		n := len(t.vars);
		if n == 0 {
			return 0;
		}
		return t.offsets[n-1] + type_size_of(s, t.vars[n-1].type);

	case ^Named:
		return type_align_of(s, t.underlying);
	}

	// Catch All
	return s.word_size;
}


align_formula :: proc(size, align: i64) -> i64 {
	s := size + align - 1;
	return s - s%align;
}


type_is_untyped :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .Untyped in b.flags;
	}
	return false;
}

type_is_typed :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .Untyped not_in b.flags;
	}
	return true;
}


type_is_integer :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .Integer in b.flags;		
	}
	return false;
}

type_is_unsigned :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .Unsigned in b.flags;		
	}
	return false;
}

type_is_boolean :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .Boolean in b.flags;		
	}
	return false;
}
type_is_float :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .Float in b.flags;		
	}
	return false;
}
type_is_string :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return .String in b.flags;		
	}
	return false;
}
type_is_pointer :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	#partial switch b in bt.variant {
	case ^Basic:
		return .Pointer in b.flags;	
	case ^Pointer:
		return true;
	} 
	return false;
}

is_const_type :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	#partial switch b in bt.variant {
	case ^Basic:
		return b.flags & Basic_Flags_Constant_Type != nil;	
	} 
	return false;
}

are_identical :: proc(x, y: ^Type) -> bool {
	if x == y {
		return true;
	}
	if x == nil || y == nil {
		return false;
	}
	switch x in x.variant {
	case ^Basic:
		if y, ok := y.variant.(^Basic); ok {
			return x.kind == y.kind;
		}
	case ^Array:
		if y, ok := y.variant.(^Array); ok {
			return (x.len == y.len) && are_identical(x.elem, y.elem);
		}
	case ^Slice:
		if y, ok := y.variant.(^Slice); ok {
			return are_identical(x.elem, y.elem);
		}
	case ^Struct:
		if y, ok := y.variant.(^Struct); ok {
			if len(x.fields) == len(y.fields) {
				for f, i in x.fields {
					g := y.fields[i];
					if f.name != g.name || !are_identical(f.type, g.type) {
						return false;
					}
				}
				return true;
			}
		}
	case ^Pointer:
		if y, ok := y.variant.(^Pointer); ok {
			return are_identical(x.elem, y.elem);
		}
	case ^Tuple:
		if y, ok := y.variant.(^Tuple); ok {
			if len(x.vars) == len(y.vars) {
				for f, i in x.vars {
					g := y.vars[i];
					if f.name != g.name || !are_identical(f.type, g.type) {
						return false;
					}
				}
				return true;
			}
		}
	case ^Signature:
		if y, ok := y.variant.(^Signature); ok {
			return are_identical(x.params, y.params) && are_identical(x.params, y.params);
		}
	case ^Named:
		if y, ok := y.variant.(^Named); ok {
			return x.entity == y.entity;
		}
	}


	return false;
}


