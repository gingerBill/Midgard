package frontend

Builtin_Id :: enum u16 {
	Invalid,

	Len,
	Size_Of,
	Align_Of,
	Offset_Of,
}

Builtin_Proc :: struct {
	name: string,
}

builtin_procs := [Builtin_Id]Builtin_Proc{
	.Invalid   = {"invalid"},

	.Len       = {"len"},
	.Size_Of   = {"size_of"},
	.Align_Of  = {"align_of"},
	.Offset_Of = {"offset_of"},
};
