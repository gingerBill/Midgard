package wasm_frontend

WORD_SIZE :: size_of(i32);

Type :: struct {
	derived: any,
	cached_size:  i32,
	cached_align: i32,
}

Type_Basic_Kind :: enum u8 {
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

	byte = u8,
}

Type_Basic_Flag :: enum u8 {
	Boolean,
	Integer,
	Unsigned,
	Float,
	Pointer,
	String,
	Rune,
	Untyped,
}

Type_Basic_Flags :: distinct bit_set[Type_Basic_Flag; u32];

Type_Basic_Flags_Numeric :: Type_Basic_Flags{.Integer, .Float};
Type_Basic_Flags_Ordered :: Type_Basic_Flags{.Integer, .Float, .String, .Pointer, .Rune};
Type_Basic_Flags_Ordered_Numeric :: Type_Basic_Flags_Ordered & Type_Basic_Flags_Numeric;
Type_Basic_Flags_Constant_Type :: Type_Basic_Flags{.Boolean, .Integer, .Unsigned, .Float, .Pointer, .String, .Rune};

Type_Basic :: struct {
	using base: Type,
	kind:  Type_Basic_Kind,
	flags: Type_Basic_Flags,
	size:  i32,
	align: i32,
	name:  string,
}


basic_types := [Type_Basic_Kind]Type_Basic{
	.Invalid = {},

	.bool   = {{}, .bool,   {.Boolean},            1,           1,         "bool"},

	.i8     = {{}, .i8,     {.Integer},            1,           1,         "i8"},
	.i16    = {{}, .i16,    {.Integer},            2,           2,         "i16"},
	.i32    = {{}, .i32,    {.Integer},            4,           4,         "i32"},
	.i64    = {{}, .i64,    {.Integer},            8,           8,         "i64"},
	.isize  = {{}, .isize,  {.Integer},            WORD_SIZE,   WORD_SIZE, "isize"},
	.u8     = {{}, .u8,     {.Integer, .Unsigned}, 1,           1,         "u8"},
	.u16    = {{}, .u16,    {.Integer, .Unsigned}, 2,           2,         "u16"},
	.u32    = {{}, .u32,    {.Integer, .Unsigned}, 4,           4,         "u32"},
	.u64    = {{}, .u64,    {.Integer, .Unsigned}, 8,           8,         "u64"},
	.usize  = {{}, .usize,  {.Integer, .Unsigned}, WORD_SIZE,   WORD_SIZE, "usize"},

	.rune   = {{}, .rune,   {.Rune},               4,           4,         "rune"},

	.f32    = {{}, .f32,    {.Float},              4,           4,         "f32"},
	.f64    = {{}, .f64,    {.Float},              8,           8,         "f64"},

	.rawptr = {{}, .rawptr, {.Pointer},            WORD_SIZE,   WORD_SIZE, "rawptr"},
	.string = {{}, .string, {.String},             2*WORD_SIZE, WORD_SIZE, "string"},
};



Type_Array :: struct {
	using base: Type,
	len: i32,
	elem: ^Type,
}

Type_Slice :: struct {
	using base: Type,
	elem: ^Type,
}

Type_Struct :: struct {
	using base: Type,
	fields: []^Entity_Variable,
}

Type_Pointer :: struct {
	using base: Type,
	elem: ^Type,
}

Type_Tuple :: struct {
	using base: Type,
	vars: []^Entity_Variable,
}

Type_Proc :: struct {
	using base: Type,
	scope: ^Scope,
	params: ^Type_Tuple,
	results: ^Type_Tuple,
}

Type_Named_State :: enum {
	Unknown,
	Processing,
	Invalid,
	Valid,
}

Type_Named :: struct {
	using base: Type,
	state: Type_Named_State,
	entity: ^Entity,
	underlying: ^Type,
}
