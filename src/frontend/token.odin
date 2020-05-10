package frontend

import "core:strings"
import "core:fmt"

Token :: struct {
	kind: Token_Kind,
	text: string,
	pos:  Pos,
}

Pos :: struct {
	file:   ^File,
	offset: int, // starting at 0
	line:   u32, // starting at 1
	column: u32, // starting at 1
}

pos_is_valid :: proc(using p: Pos, ignore_file := false) -> bool {
	return (file != nil || ignore_file) && offset >= 0 && line > 0 && column > 0;
}

pos_compare :: proc(lhs, rhs: Pos) -> int {
	if lhs.offset != rhs.offset {
		return -1 if (lhs.offset < rhs.offset) else +1;
	}
	if lhs.line != rhs.line {
		return -1 if (lhs.line < rhs.line) else +1;
	}
	if lhs.column != rhs.column {
		return -1 if (lhs.column < rhs.column) else +1;
	}
	if lhs.file == rhs.file {
		return 0;
	}
	if lhs.file != nil && rhs.file == nil {
		return -1;
	}
	if lhs.file == nil && rhs.file != nil {
		return +1;
	}
	return strings.compare(lhs.file.fullpath, rhs.file.fullpath);
}

pos_string :: proc(using p: Pos, allocator := context.temp_allocator) -> string {
	b := strings.make_builder(allocator);
	if file != nil {
		strings.write_string(&b, file.fullpath);
	}

	if pos_is_valid(p, true) {
		strings.write_byte(&b, '(');
		strings.write_u64(&b, u64(line));
		strings.write_byte(&b, ':');
		strings.write_u64(&b, u64(column));
		strings.write_byte(&b, ')');
	}
	if len(b.buf) == 0 {
		strings.write_byte(&b, '-');
	}

	return strings.to_string(b);
}


Token_Kind :: enum u8 {
	Invalid,
	EOF,
	Comment,

	B_Literal_Begin,
		Ident,
		Integer,
		Float,
		Rune,
		String,
	B_Literal_End,

	B_Operator_Begin,
		Add, // +
		Sub, // -
		Mul, // *
		Quo, // /
		Mod, // %

		And,     // &
		Or,      // |
		Xor,     // ~
		Shl,     // <<
		Shr,     // >>
		And_Not, // &~

		Add_Assign, // +=
		Sub_Assign, // -=
		Mul_Assign, // *=
		Quo_Assign, // /=
		Mod_Assign, // %=

		And_Assign,     // &=
		Or_Assign,      // |=
		Xor_Assign,     // ~=
		Shl_Assign,     // <<=
		Shr_Assign,     // >>=
		And_Not_Assign, // &~=

		Cmp_And, // &&
		Cmp_Or,  // ||

		Cmp_Eq, // ==
		Not_Eq, // !=
		Lt,     // <
		Gt,     // >
		Lt_Eq,  // <=
		Gt_Eq,  // >=

		Assign,      // =
		Not,         // !
		Arrow_Right, // ->
		Pointer,     // ^

		Open_Paren,   // (
		Open_Bracket, // [
		Open_Brace,   // {
		Comma,        // ,
		Period,       // .

		Close_Paren,   // )
		Close_Bracket, // ]
		Close_Brace,   // }
		Semicolon,     // ;
		Colon,         // :
	B_Operator_End,

	B_Keyword_Begin,
		Break,
		Continue,
		Fallthrough,

		Case,
		Switch,
		For,
		If,
		Else,
		Defer,
		Return,

		Proc,
		Struct,
		Enum,
		Union,

		Import,
		Export,
		Foreign,
		Const,
		Type,
		Var,
	B_Keyword_End,
};

tokens := [len(Token_Kind)]string {
	"Invalid",
	"EOF",
	"Comment",

	"",
	"Identifer",
	"Integer",
	"Float",
	"Rune",
	"String",
	"",

	"",
	"+",
	"-",
	"*",
	"/",
	"%",

	"&",
	"|",
	"~",
	"<<",
	">>",
	"&~",

	"+=",
	"-=",
	"*=",
	"/=",
	"%=",

	"&",
	"|",
	"~",
	"<<",
	">>",
	"&~",

	"&&",
	"||",

	"==",
	"!=",
	"<",
	">",
	"<=",
	">=",

	"=",
	"!",
	"->",
	"^",

	"(",
	"[",
	"{",
	",",
	".",

	")",
	"]",
	"}",
	";",
	":",
	"",

	"",
	"break",
	"continue",
	"fallthrough",

	"case",
	"switch",
	"for",
	"if",
	"else",
	"defer",
	"return",

	"proc",
	"struct",
	"enum",
	"union",

	"import",
	"export",
	"foreign",
	"const",
	"type",
	"var",
	"",
};


to_string :: proc(kind: Token_Kind) -> string {
	if Token_Kind.Invalid <= kind && kind < Token_Kind(len(Token_Kind)) {
		return tokens[kind];
	}
	return "Invalid";
}

is_literal  :: proc(kind: Token_Kind) -> bool {
	return Token_Kind.B_Literal_Begin  < kind && kind < Token_Kind.B_Literal_End;
}
is_operator :: proc(kind: Token_Kind) -> bool {
	#partial switch kind {
	case .B_Operator_Begin .. .B_Operator_End:
		return true;
	}
	return false;
}
is_keyword :: proc(kind: Token_Kind) -> bool {
	switch {
	case Token_Kind.B_Keyword_Begin < kind && kind < Token_Kind.B_Keyword_End:
		return true;
	}
	return false;
}

token_precedence :: proc(op: Token_Kind) -> int {
	#partial switch op {
	case .Cmp_Or:
		return 1;
	case .Cmp_And:
		return 2;
	case .Cmp_Eq, .Not_Eq, .Lt, .Lt_Eq, .Gt, .Gt_Eq:
		return 3;
	case .Add, .Sub, .Or, .Xor:
		return 4;
	case .Mul, .Quo, .Mod, .Shl, .Shr, .And, .And_Not:
		return 5;
	}
	return 0;	
}
