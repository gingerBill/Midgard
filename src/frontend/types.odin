package frontend

import "core:strings"

Standard_Sizes :: struct {
	word_size: i64,
	max_align: i64,
}

Type :: struct {
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
	invalid,

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
	untyped_bool,
	untyped_int,
	untyped_float,
	untyped_rune,
	untyped_string,
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


btype := [Basic_Kind]Basic{
	.invalid = {},

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

	.untyped_nil    = {{}, .untyped_nil,    {.Untyped},            -1, -1, "untyped nil"},
	.untyped_bool   = {{}, .untyped_bool,   {.Untyped, .Boolean},  -1, -1, "untyped bool"},
	.untyped_int    = {{}, .untyped_int,    {.Untyped, .Integer},  -1, -1, "untyped int"},
	.untyped_float  = {{}, .untyped_float,  {.Untyped, .Float},    -1, -1, "untyped float"},
	.untyped_rune   = {{}, .untyped_rune,   {.Untyped, .Rune},     -1, -1, "untyped rune"},
	.untyped_string = {{}, .untyped_string, {.Untyped, .String},   -1, -1, "untyped string"},
};



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
	t := new(T);
	t^.variant = t;
	return t;
}

new_tuple :: proc(vars: ..^Variable) -> ^Tuple {
	t := new_type(Tuple);
	t.vars = vars;
	return t;
}

new_pointer :: proc(elem: ^Type) -> ^Pointer {
	ptr := new_type(Pointer);
	ptr.elem = elem;
	return ptr;
}

tuple_len :: proc(t: ^Tuple) -> int {
	if t == nil {
		return 0;
	}
	return len(t.vars);
}


type_underlying :: proc(t: ^Type) -> ^Type {
	if t == nil {
		return nil;
	}
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

default_type :: proc(t: ^Type) -> ^Type {
	if bt, ok := t.variant.(^Basic); ok {
		#partial switch bt.kind {
		case .untyped_bool:   return &btype[.bool];
		case .untyped_int:    return &btype[.isize];
		case .untyped_float:  return &btype[.f64];
		case .untyped_rune:   return &btype[.rune];
		case .untyped_string: return &btype[.string];
		}
	}
	return t;
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


type_has_nil :: proc(type: ^Type) -> bool {
	switch t in type_underlying(type).variant {
	case ^Basic:
		return t.kind == .rawptr || t.kind == .untyped_nil;
	case ^Array, ^Struct, ^Tuple:
		return false;
	case ^Slice, ^Pointer, ^Signature:
		return true;
	case ^Named:
		unreachable();
	}
	return false;
}

type_is_comparable :: proc(type: ^Type) -> bool {
	switch t in type_underlying(type).variant {
	case ^Basic:
		return t.flags & Basic_Flags_Ordered != nil;
	case ^Array, ^Slice, ^Struct, ^Tuple:
	case ^Pointer, ^Signature:
		return true;
	case ^Named:
		unreachable();
	}
	return false;
}
type_is_ordered :: proc(type: ^Type) -> bool {
	switch t in type_underlying(type).variant {
	case ^Basic:
		return t.flags & Basic_Flags_Ordered != nil;
	case ^Array, ^Slice, ^Struct, ^Tuple, ^Signature:
		return false;
	case ^Pointer:
		return true;
	case ^Named:
		unreachable();
	}
	return false;
}


type_is_numeric :: proc(t: ^Type) -> bool {
	bt := type_underlying(t);
	if b, ok := bt.variant.(^Basic); ok {
		return b.flags & Basic_Flags_Numeric != nil;
	}
	return false;
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


