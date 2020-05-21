package wasm_assembler

import "core:fmt"

PAGE_SIZE :: 1<<16;

Val_Type :: enum u8 {
	void  = 0x00,
	i32   = 0x7f,
	i64   = 0x7e,
	f32   = 0x7d,
	f64   = 0x7c,
}

Op :: enum u16le {
	Unreachable   = 0x00,
	Nop           = 0x01,
	Block         = 0x02,
	Loop          = 0x03,
	If            = 0x04,
	End           = 0x0b,
	Br            = 0x0c,
	Br_If         = 0x0d,
	Br_Table      = 0x0e,
	Return        = 0x0f,
	Call          = 0x10,
	Call_Indirect = 0x11,

	Drop   = 0x1a,
	Select = 0x1b,

	Local_Get  = 0x20,
	Local_Set  = 0x21,
	Local_Tee  = 0x22, // Sets and then returns value
	Global_Get = 0x23,
	Global_Set = 0x24,

	Load_i32 = 0x28,
	Load_i64 = 0x29,
	Load_f32 = 0x2a,
	Load_f64 = 0x2b,

	Load_i32_as_s8  = 0x2c,
	Load_i32_as_u8  = 0x2d,
	Load_i32_as_s16 = 0x2e,
	Load_i32_as_u16 = 0x2f,

	Load_i64_as_s8  = 0x30,
	Load_i64_as_u8  = 0x31,
	Load_i64_as_s16 = 0x32,
	Load_i64_as_u16 = 0x33,
	Load_i64_as_s32 = 0x34,
	Load_i64_as_u32 = 0x35,

	Store_i32 = 0x36,
	Store_i64 = 0x37,
	Store_f32 = 0x38,
	Store_f64 = 0x39,

	Store_i32_i8  = 0x3a,
	Store_i32_i16 = 0x3b,

	Store_i64_i8  = 0x3c,
	Store_i64_i16 = 0x3d,
	Store_i64_i32 = 0x3e,

	Memory_Size = 0x3f,
	Memory_Grow = 0x40,

	Const_i32 = 0x41,
	Const_i64 = 0x42,
	Const_f32 = 0x43,
	Const_f64 = 0x44,

	Eqz_i32 = 0x45,
	Eq_i32  = 0x46,
	Ne_i32  = 0x47,
	Lt_s32  = 0x48,
	Lt_u32  = 0x49,
	Gt_s32  = 0x4a,
	Gt_u32  = 0x4b,
	Le_s32  = 0x4c,
	Le_u32  = 0x4d,
	Ge_s32  = 0x4e,
	Ge_u32  = 0x4f,

	Eqz_i64 = 0x50,
	Eq_i64  = 0x51,
	Ne_i64  = 0x52,
	Lt_s64  = 0x53,
	Lt_u64  = 0x54,
	Gt_s64  = 0x55,
	Gt_u64  = 0x56,
	Le_s64  = 0x57,
	Le_u64  = 0x58,
	Ge_s64  = 0x59,
	Ge_u64  = 0x5a,

	Eq_f32 = 0x5b,
	Ne_f32 = 0x5c,
	Lt_f32 = 0x5d,
	Gt_f32 = 0x5e,
	Le_f32 = 0x5f,
	Ge_f32 = 0x60,

	Eq_f64 = 0x61,
	Ne_f64 = 0x62,
	Lt_f64 = 0x63,
	Gt_f64 = 0x64,
	Le_f64 = 0x65,
	Ge_f64 = 0x66,


	Clz_i32 = 0x67,
	Ctz_i32,
	Popcnt_i32,
	Add_i32,
	Sub_i32,
	Mul_i32,
	Div_s32,
	Div_u32,
	Rem_s32,
	Rem_u32,
	And_i32,
	Or_i32,
	Xor_i32,
	Shl_i32,
	Shr_s32,
	Shr_u32,
	Rotl_i32,
	Rotr_i32 = 0x78,

	Clz_i64 = 0x79,
	Ctz_i64,
	Popcnt_i64,
	Add_i64,
	Sub_i64,
	Mul_i64,
	Div_s64,
	Div_u64,
	Rem_s64,
	Rem_u64,
	And_i64,
	Or_i64,
	Xor_i64,
	Shl_i64,
	Shr_s64,
	Shr_u64,
	Rotl_i64,
	Rotr_i64 = 0x8a,

	Abs_f32 = 0x8b,
	Neg_f32,
	Ceil_f32,
	Floor_f32,
	Trunc_f32,
	Nearest_f32,
	Sqrt_f32,
	Add_f32,
	Sub_f32,
	Mul_f32,
	Div_f32,
	Min_f32,
	Max_f32,
	Copysign_f32 = 0x98,

	Abs_f64 = 0x99,
	Neg_f64,
	Ceil_f64,
	Floor_f64,
	Trunc_f64,
	Nearest_f64,
	Sqrt_f64,
	Add_f64,
	Sub_f64,
	Mul_f64,
	Div_f64,
	Min_f64,
	Max_f64,
	Copysign_f64 = 0xa6,


	Wrap_i64_to_i32 = 0xa7,
	Trunc_f32_s32,
	Trunc_f32_u32,
	Trunc_f64_s32,
	Trunc_f64_u32,
	Extend_i32_s64,
	Extend_i32_u64,
	Trunc_f32_s64,
	Trunc_f32_u64,
	Trunc_f64_s64,
	Trunc_f64_u64,
	Convert_s32_f32,
	Convert_u32_f32,
	Convert_s64_f32,
	Convert_u64_f32,
	Demote_f64_to_f32,
	Convert_s32_to_f64,
	Convert_u32_to_f64,
	Convert_s64_to_f64,
	Convert_u64_to_f64,
	Promote_f32_to_f64,
	Reinterpret_f32_to_i32,
	Reinterpret_f64_to_i64,
	Reinterpret_i32_to_f32,
	Reinterpret_i64_to_f64 = 0xbf,

	Extend_s8_to_i32  = 0xc0,
	Extend_s16_to_i32 = 0xc1,
	Extend_s8_to_i64  = 0xc2,
	Extend_s16_to_i64 = 0xc3,
	Extend_s32_to_i64 = 0xc4,

	GAP = 0xff,

	trunc_sat_f32_to_s32 = 0x00_fc,
	trunc_sat_f32_to_u32 = 0x01_fc,
	trunc_sat_f64_to_s32 = 0x02_fc,
	trunc_sat_f64_to_u32 = 0x03_fc,
	trunc_sat_f32_to_s64 = 0x04_fc,
	trunc_sat_f32_to_u64 = 0x05_fc,
	trunc_sat_f64_to_s64 = 0x06_fc,
	trunc_sat_f64_to_u64 = 0x07_fc,
}

Section :: enum u8 {
	Custom   = 0x00,
	Type     = 0x01,
	Import   = 0x02,
	Function = 0x03,
	Table    = 0x04,
	Memory   = 0x05,
	Global   = 0x06,
	Export   = 0x07,
	Start    = 0x08,
	Element  = 0x09,
	Code     = 0x0a,
	Data     = 0x0b,
}


Emitter :: struct {
	binary_data: [dynamic]byte,
}

Assembler :: struct {
	emitter:      Emitter,
	signatures:   [dynamic]Signature,
	funcs:        [dynamic]Function,
	imports:      [dynamic]Import,
	globals:      [dynamic]Global,
	memories:     [1]Memory,
	memories_len: u32,
	func_indices: map[string]u32,
	global_indices: map[string]u32,

	data_section_size: u32,
	datums: [dynamic]Datum,
}


MAGIC := [4]byte{0x00, 0x61, 0x73, 0x6d};
VERSION := [4]byte{0x01, 0x00, 0x00, 0x00};

MAX_PARAM_LEN :: 16;

Signature :: struct {
	index:      u32,

	params:     [MAX_PARAM_LEN]Val_Type,
	param_len:  u8,
	results:    [1]Val_Type,
	result_len: u8,
}

Location_Kind :: enum {
	Internal = 0,
	Export,
	Import,
}

Function :: struct {
	name: string,
	sig:  ^Signature,
	kind: Location_Kind,
	code: Emitter,
}

Import_Kind :: enum u8 {
	Func = 0x00,
	Table = 0x01,
	Mem = 0x02,
	Global = 0x03,
}

Import :: struct {
	kind:   Import_Kind,
	module: string,
	name:   string,
	type:   union{Val_Type, ^Signature},
}

Global_Expr :: union {
	i32, i64,
	f32, f64,
	[]byte,
}

Global :: struct {
	type:     Val_Type,
	is_const: bool,
	expr: Global_Expr,
	name: string,
	kind: Location_Kind,
}

Memory :: struct {
	limit_min: u32,
	limit_max: u32,
	name: string,
}

Datum :: struct {
	offset: i32,
	memory: []byte,
}

EXPORT_DESC_FUNC   :: 0x00;
EXPORT_DESC_TABLE  :: 0x01;
EXPORT_DESC_MEM    :: 0x02;
EXPORT_DESC_GLOBAL :: 0x03;


emit_header :: proc(using e: ^Emitter) {
	append(&binary_data, ..MAGIC[:]);
	append(&binary_data, ..VERSION[:]);
}

emit_byte :: proc(using e: ^Emitter, b: byte) {
	append(&binary_data, b);
}

emit_bytes :: proc(using e: ^Emitter, bytes: ..byte) {
	append(&binary_data, ..bytes);
}

emit_type :: proc(using e: ^Emitter, t: Val_Type) {
	append(&binary_data, byte(t));
}

emit_op :: proc(using e: ^Emitter, op: Op) {
	if op <= .GAP {
		append(&binary_data, byte(op));
	} else {
		data := transmute([2]u8)u16le(op);
		append(&binary_data, data[0], data[1]);
	}
}

u32_byte_length :: proc(u: int) -> u32 {
	length := u32(0);
	value := u32le(u);
	for {
		b := byte(value & 0x7f);
		value >>= 7;
		if value != 0 {
			b |= 0x80;
		}
		length += 1;
		if value == 0 {
			break;
		}
	}
	return length;
}

u32_byte_length32 :: proc(u: u32) -> u32 {
	length := u32(0);
	value := u32le(u);
	for {
		b := byte(value & 0x7f);
		value >>= 7;
		if value != 0 {
			b |= 0x80;
		}
		length += 1;
		if value == 0 {
			break;
		}
	}
	return length;
}
i32_byte_length32 :: proc(u: i32) -> u32 {
	length := u32(0);

	value := i32le(u);
	negative := value < 0;
	size := size_of(value)*8;

	more := true;
	for more {
		b := byte(value & 0x7f);
		value >>= 7;

		if negative {
			value |= ~i32le(0) << u32le(size - 7);
		}

		if value ==  0 && (b & 0x40) != 0x40 ||
		   value == -1 && (b & 0x40) == 0x40 {
			more = false;
		} else {
			b |= 0x80;
		}
		length += 1;
	}
	return length;
}
u64_byte_length32 :: proc(u: u64) -> u32 {
	length := u32(0);
	value := u64le(u);
	for {
		b := byte(value & 0x7f);
		value >>= 7;
		if value != 0 {
			b |= 0x80;
		}
		length += 1;
		if value == 0 {
			break;
		}
	}
	return length;
}
i64_byte_length32 :: proc(u: i64) -> u32 {
	length := u32(0);

	value := i64le(u);
	negative := value < 0;
	size := size_of(value)*8;

	more := true;
	for more {
		b := byte(value & 0x7f);
		value >>= 7;

		if negative {
			value |= ~i64le(0) << u64le(size - 7);
		}

		if value ==  0 && (b & 0x40) != 0x40 ||
		   value == -1 && (b & 0x40) == 0x40 {
			more = false;
		} else {
			b |= 0x80;
		}
		length += 1;
	}
	return length;
}

emit_u32 :: proc(using e: ^Emitter, u: u32) {
	value := u32le(u);
	for {
		b := byte(value & 0x7f);
		value >>= 7;
		if value != 0 {
			b |= 0x80;
		}
		append(&binary_data, b);
		if value == 0 {
			break;
		}
	}
}
emit_u64 :: proc(using e: ^Emitter, u: u64) {
	value := u64le(u);
	for {
		b := byte(value & 0x7f);
		value >>= 7;
		if value != 0 {
			b |= 0x80;
		}
		append(&binary_data, b);
		if value == 0 {
			break;
		}
	}
}

emit_i32 :: proc(using e: ^Emitter, u: i32) {
	value := i32le(u);
	negative := value < 0;
	size := size_of(value)*8;

	more := true;
	for more {
		b := byte(value & 0x7f);
		value >>= 7;

		if negative {
			value |= ~i32le(0) << u32le(size - 7);
		}

		if value ==  0 && (b & 0x40) != 0x40 ||
		   value == -1 && (b & 0x40) == 0x40 {
			more = false;
		} else {
			b |= 0x80;
		}
		append(&binary_data, b);
	}
}
emit_i64 :: proc(using e: ^Emitter, u: i64) {
	value := i64le(u);
	negative := value < 0;
	size := size_of(value)*8;

	more := true;
	for more {
		b := byte(value & 0x7f);
		value >>= 7;

		if negative {
			value |= ~i64le(0) << u64le(size - 7);
		}

		if value ==  0 && (b & 0x40) != 0x40 ||
		   value == -1 && (b & 0x40) == 0x40 {
		   	more = false;
		} else {
			b |= 0x80;
		}
		append(&binary_data, b);
	}
}

emit_f32 :: proc(using e: ^Emitter, f: f32) {
	value := transmute([4]byte)f32le(f);
	append(&binary_data, ..value[:]);
}
emit_f64 :: proc(using e: ^Emitter, f: f64) {
	value := transmute([8]byte)f64le(f);
	append(&binary_data, ..value[:]);
}

emit_name :: proc(using e: ^Emitter, name: string) {
	n := u32(len(name));
	emit_u32(e, n);
	append(&binary_data, name);
}

emit_name_byte_length :: proc(name: string) -> u32 {
	return u32_byte_length(len(name)) + u32(len(name));
}

emit_section :: proc(using e: ^Emitter, section: Section, size: u32) {
	append(&binary_data, byte(section));
	emit_u32(e, size);
}

emit_call :: proc(e: ^Emitter, func_index: u32) {
	emit_op(e, .Call);
	emit_u32(e, func_index);
}

emit_limits_min :: proc(e: ^Emitter, min: u32) {
	emit_byte(e, 0x00);
	emit_u32(e, min);
}
emit_limits_min_max :: proc(e: ^Emitter, min, max: u32) {
	emit_byte(e, 0x01);
	emit_u32(e, min);
	emit_u32(e, max);
}

emit_table_type_min :: proc(e: ^Emitter, min: u32) {
	emit_byte(e, 0x70);
	emit_limits_min(e, min);
}
emit_table_type_min_max :: proc(e: ^Emitter, min, max: u32) {
	emit_byte(e, 0x70);
	emit_limits_min_max(e, min, max);
}

emit_global_type :: proc(e: ^Emitter, t: Val_Type, is_const: bool) {
	emit_type(e, t);
	emit_byte(e, 0x01 if is_const else 0x00);
}

emit_const_i32 :: proc(e: ^Emitter, val: i32) {
	emit_op(e, .Const_i32); emit_i32(e, val);
}
emit_const_i64 :: proc(e: ^Emitter, val: i64) {
	emit_op(e, .Const_i64); emit_i64(e, val);
}
emit_const_f32 :: proc(e: ^Emitter, val: f32) {
	emit_op(e, .Const_f32); emit_f32(e, val);
}
emit_const_f64 :: proc(e: ^Emitter, val: f64) {
	emit_op(e, .Const_f64); emit_f64(e, val);
}

emit_mem_arg :: proc(e: ^Emitter, offset, align: u32) {
	emit_u32(e, offset);
	emit_u32(e, align);
}

emit_load_i32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Load_i32); emit_mem_arg(e, offset, align);
}

emit_load_i64 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64); emit_mem_arg(e, offset, align);
}

emit_load_f32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(f32le)) {
	emit_op(e, .Load_f32); emit_mem_arg(e, offset, align);
}

emit_load_f64 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(f64le)) {
	emit_op(e, .Load_f64); emit_mem_arg(e, offset, align);
}


emit_load_i32_as_s8 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Load_i32_as_s8); emit_mem_arg(e, offset, align);
}
emit_load_i32_as_u8 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Load_i32_as_u8); emit_mem_arg(e, offset, align);
}
emit_load_i32_as_s16 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Load_i32_as_s16); emit_mem_arg(e, offset, align);
}
emit_load_i32_as_u16 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Load_i32_as_u16); emit_mem_arg(e, offset, align);
}

emit_load_i64_as_s8 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64_as_s8); emit_mem_arg(e, offset, align);
}
emit_load_i64_as_u8 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64_as_u8); emit_mem_arg(e, offset, align);
}
emit_load_i64_as_s16 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64_as_s16); emit_mem_arg(e, offset, align);
}
emit_load_i64_as_u16 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64_as_u16); emit_mem_arg(e, offset, align);
}
emit_load_i64_as_s32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64_as_s32); emit_mem_arg(e, offset, align);
}
emit_load_i64_as_u32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i64le)) {
	emit_op(e, .Load_i64_as_u32); emit_mem_arg(e, offset, align);
}

emit_store_i32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i32); emit_mem_arg(e, offset, align);
}
emit_store_i64 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i64); emit_mem_arg(e, offset, align);
}
emit_store_f32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_f32); emit_mem_arg(e, offset, align);
}
emit_store_f64 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_f64); emit_mem_arg(e, offset, align);
}

emit_store_i32_i8 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i32_i8); emit_mem_arg(e, offset, align);
}
emit_store_i32_i16 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i32_i16); emit_mem_arg(e, offset, align);
}

emit_store_i64_i8 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i64_i8); emit_mem_arg(e, offset, align);
}
emit_store_i64_i16 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i64_i16); emit_mem_arg(e, offset, align);
}
emit_store_i64_i32 :: proc(e: ^Emitter, offset: u32, align: u32 = align_of(i32le)) {
	emit_op(e, .Store_i64_i32); emit_mem_arg(e, offset, align);
}


signature_equal :: proc(a, b: Signature) -> bool {
	if a.param_len != b.param_len {
		return false;
	}
	if a.result_len != b.result_len {
		return false;
	}
	return a.params == b.params && a.results == b.results;
}

signature :: proc(a: ^Assembler, params: []Val_Type, result: Val_Type) -> ^Signature {
	assert(len(params) <= MAX_PARAM_LEN);

	sig: Signature;
	sig.param_len = min(u8(len(params)), len(sig.params));
	copy(sig.params[:], params);

	sig.result_len = 1 if result != .void else 0;
	sig.results[0] = result;

	// TODO(bill): Hash Map Cache
	for signature, i in a.signatures {
		if signature_equal(sig, signature) {
			return &a.signatures[i];
		}
	}

	index := u32(len(a.signatures));
	sig.index = index;
	append(&a.signatures, sig);
	return &a.signatures[index];
}

create_function :: proc(a: ^Assembler, def: Function) -> ^Emitter {
	assert(def.name not_in a.func_indices);
	append(&a.funcs, def);
	n := len(a.funcs)-1;
	func := &a.funcs[n];
	a.func_indices[func.name] = u32(n + len(a.imports));
	return &func.code;
}

func_index :: proc(a: ^Assembler, name: string) -> u32 {
	index, ok := a.func_indices[name];
	assert(ok);
	return index;
}


func_sig_index :: proc(a: ^Assembler, name: string) -> u32 {
	index, ok := a.func_indices[name];
	assert(ok);
	return index;
}

get_func :: proc(a: ^Assembler, name: string) -> ^Function {
	index, ok := a.func_indices[name];
	assert(ok);
	return &a.funcs[index];
}

import_function :: proc(a: ^Assembler, module, name: string, sig: ^Signature) -> u32 {
	assert(len(a.funcs) == 0, "imports must happen before functions are declared");

	imp := Import{
		kind = .Func,
		module = module,
		name = name,
		type = sig,
	};
	append(&a.imports, imp);
	return u32(len(a.imports)-1);
}

add_global :: proc(a: ^Assembler, global: Global) -> u32 {
	n := u32(len(a.globals));
	g := global;
	if g.type == .void {
		switch x in g.expr {
		case i32: g.type = .i32;
		case i64: g.type = .i64;
		case f32: g.type = .f32;
		case f64: g.type = .f64;
		case []byte: panic("a byte expression must specify a type");
		}
	}
	if g.expr == nil {
		assert(g.type != .void);
	}
	if g.kind != .Internal {
		assert(g.name != "");
	}
	if g.name != "" {
		a.global_indices[g.name] = u32(len(a.globals));
	}

	append(&a.globals, g);
	return n;
}

add_memory :: proc(a: ^Assembler, min: u32, name: string) {
	assert(a.memories_len == 0);
	min_pages := (min + PAGE_SIZE-1)/PAGE_SIZE;

	a.memories[a.memories_len] = Memory{
		limit_min = min_pages,
		limit_max = max(u32),
		name = name,
	};

	a.memories_len += 1;
}

add_datum :: proc(a: ^Assembler, offset: i32, data: []byte) {
	datum := Datum {
		offset = offset,
		memory = data,
	};
	append(&a.datums, datum);
	a.data_section_size += u32(len(datum.memory));
}

add_datum_string :: proc(a: ^Assembler, offset: i32, str: string) {
	datum := Datum {
		offset = offset,
		memory = transmute([]byte)str,
	};
	append(&a.datums, datum);
	a.data_section_size += u32(len(datum.memory));
}


generate_assembly :: proc(using a: ^Assembler) {
	e := &emitter;
	emit_header(e);

	{ // Type Section
		section_size := u32(0);
		section_size += u32_byte_length(len(signatures));
		for sigature in signatures {
			section_size += 1; // func type
			// param vec count
			section_size += u32_byte_length(int(sigature.param_len));
			// params
			section_size += u32(sigature.param_len);

			// result vec count
			section_size += u32_byte_length(int(sigature.result_len));
			// results
			section_size += u32(sigature.result_len);
		}

		emit_section(e, .Type, section_size);
		emit_u32(e, u32(len(signatures)));
		for sig in signatures {
			emit_byte(e, 0x60);
			emit_u32(e, u32(sig.param_len));
			for i in 0..<sig.param_len {
				emit_type(e, sig.params[i]);
			}
			emit_u32(e, u32(sig.result_len));
			for i in 0..<sig.result_len {
				emit_type(e, sig.results[i]);
			}
		}
	}

	{ // Import
		section_size := u32(0);
		section_size += u32_byte_length(len(imports));
		for imp in imports {
			section_size += emit_name_byte_length(imp.module);
			section_size += emit_name_byte_length(imp.name);
			section_size += 1;
			switch imp.kind {
			case .Func:
				sig := imp.type.(^Signature);
				section_size += u32_byte_length(int(sig.index));
			case .Table:
				panic("TODO: Import Table");
			case .Mem:
				panic("TODO: Import Mem");
			case .Global:
				panic("TODO: Import Global");

			}
		}

		emit_section(e, .Import, section_size);
		emit_u32(e, u32(len(imports)));
		for imp in imports {
			emit_name(e, imp.module);
			emit_name(e, imp.name);
			emit_byte(e, byte(imp.kind));
			switch imp.kind {
			case .Func:
				sig := imp.type.(^Signature);
				emit_u32(e, sig.index);
			case .Table:
			case .Mem:
			case .Global:

			}
		}
	}

	{ // Function Section
		section_size := u32(0);
		section_size += u32_byte_length(len(funcs));
		for func in funcs {
			section_size += u32_byte_length(int(func.sig.index));
		}

		emit_section(e, .Function, section_size);
		emit_u32(e, u32(len(funcs)));
		for func in funcs {
			emit_u32(e, func.sig.index);
		}
	}
	{ // Memory Section
		section_size := u32(0);
		section_size += u32_byte_length32(memories_len);
		for mem in memories[:memories_len] {
			section_size += 1;
			section_size += u32_byte_length32(mem.limit_min);
			if mem.limit_max != max(u32) {
				section_size += u32_byte_length32(mem.limit_max);
			}
		}

		emit_section(e, .Memory, section_size);
		emit_u32(e, memories_len);
		for mem in memories[:memories_len] {
			if mem.limit_max == max(u32) {
				emit_limits_min(e, mem.limit_min);
			} else {
				emit_limits_min_max(e, mem.limit_min, mem.limit_max);
			}
		}
	}

	{ // Global Section
		section_size := u32(0);
		section_size += u32_byte_length(len(globals));
		for global in globals {
			section_size += 2;
			switch x in global.expr {
			case i32: section_size += 1 + i32_byte_length32(x);
			case i64: section_size += 1 + i64_byte_length32(x);
			case f32: section_size += 1 + 4;
			case f64: section_size += 1 + 8;
			case []byte: section_size += u32(len(x));
			}
			section_size += 1;
		}

		emit_section(e, .Global, section_size);
		emit_u32(e, u32(len(globals)));
		for global in globals {
			emit_global_type(e, global.type, global.is_const);
			switch x in global.expr {
			case i32: emit_const_i32(e, x);
			case i64: emit_const_i64(e, x);
			case f32: emit_const_f32(e, x);
			case f64: emit_const_f64(e, x);
			case []byte: emit_bytes(e, ..x);
			}
			emit_op(e, .End);
		}

	}

	{ // Export Section
		section_size := u32(0);
		exported_count := u32(0);
		for f, i in funcs {
			if f.kind != .Export do continue;

			exported_count += 1;
			section_size += u32_byte_length(len(f.name));
			section_size += u32(len(f.name));
			section_size += 1;
			section_size += u32_byte_length(i);
		}
		for mem, i in memories[:memories_len] {
			if mem.name == "" do continue;

			exported_count += 1;
			section_size += u32_byte_length(len(mem.name));
			section_size += u32(len(mem.name));
			section_size += 1;
			section_size += u32_byte_length(i);
		}
		for g, i in globals {
			if g.kind != .Export do continue;

			exported_count += 1;
			section_size += u32_byte_length(len(g.name));
			section_size += u32(len(g.name));
			section_size += 1;
			section_size += u32_byte_length(i);
		}


		section_size += u32_byte_length32(exported_count);

		emit_section(e, .Export, section_size);
		emit_u32(e, exported_count);
		for f, i in funcs {
			if f.kind != .Export do continue;

			emit_name(e, f.name);
			emit_byte(e, EXPORT_DESC_FUNC);
			emit_u32(e, func_index(a, f.name));
		}
		for mem, i in memories[:memories_len] {
			if mem.name == "" do continue;

			emit_name(e, mem.name);
			emit_byte(e, EXPORT_DESC_MEM);
			emit_u32(e, u32(i));
		}
		for g, i in globals {
			if g.kind != .Export do continue;

			emit_name(e, g.name);
			emit_byte(e, EXPORT_DESC_GLOBAL);
			emit_u32(e, u32(i));
		}

	}

	{ // Code Section
		section_size := u32(0);
		section_size += u32_byte_length(len(funcs));
		for f in funcs {
			section_size += u32_byte_length(len(f.code.binary_data));
			section_size += u32(len(f.code.binary_data));
		}

		emit_section(e, .Code, section_size);
		emit_u32(e, u32(len(funcs)));

		for f in funcs {
			emit_u32(e, u32(len(f.code.binary_data)));
			emit_bytes(e, ..f.code.binary_data[:]);
		}
	}

	{ // Data Section
		section_size := u32(0);
		section_size += u32_byte_length(len(datums));
		for datum in datums {
			section_size += u32_byte_length32(0);
			section_size += 1;
			section_size += i32_byte_length32(datum.offset);
			section_size += 1;
			section_size += u32_byte_length(len(datum.memory));
			section_size += u32(len(datum.memory));
		}

		emit_section(e, .Data, section_size);
		emit_u32(e, u32(len(datums)));
		for datum in datums {
			emit_u32(e, 0);
			emit_const_i32(e, datum.offset);
			emit_op(e, .End);
			emit_u32(e, u32(len(datum.memory)));
			emit_bytes(e, ..datum.memory);
		}
	}
}

