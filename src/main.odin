package wasm

import "core:fmt"
import "core:os"
import "core:strings"

import "assembler"
import frontend "frontend"

_ :: fmt;

main :: proc() {
	using frontend;
	path := "W:/Midgard/midgard/test.midgard";
	src, ok := os.read_entire_file(path);
	defer delete(src);
	tok: Tokenizer;

	file := &File{
		id = 1,
		fullpath = path,
		src = src,
	};
	pkg := &Package{
		id = 1,
		name = "midgard",
		fullpath = "W:/Midgard/midgard/",
		files = []^File{file},
	};
	file.pkg = pkg;

	p := Parser{};
	p.err = default_error_handler;
	parse_file(&p, file);


	checker := &Checker{};
	check_pkgs(checker, {pkg});
	// for decl in file.decls {
	// 	print_node(decl);
	// }

	fmt.eprintln("[done]");
	// assemble();
}

assemble :: proc() {
	using assembler;
	a := &Assembler{};

	console_log := import_function(a,
		"platform", "console_log",
		signature(a, {.i32}, .void));

	draw_circle := import_function(a, "platform", "draw_circle", signature(a, {.f32, .f32, .f32, .f32, .f32}, .void));

	draw_text := import_function(a, "platform", "draw_text",  signature(a, {.f32, .f32, .i32, .i32}, .void));


	log_string := import_function(a,
		"platform", "log_string",
		signature(a, {.i32, .i32}, .void));


	add_memory(a, 2*PAGE_SIZE, "memory");

	STR_OFFSET :: 1024;
	STR :: "Hellope YOUTUBE LIVE!";
	add_datum_string(a, STR_OFFSET, STR);

	__heap_base := i32(PAGE_SIZE);

	add_global(a, {
		type = .i32,
		expr = __heap_base,
		name = "__heap_base",
		kind = .Export,
	});
	add_global(a, {type = .i32, expr = __heap_base, name = "rsp"});
	add_global(a, {type = .i32, expr = __heap_base, name = "rbp"});
	add_global(a, {
		type = .i32,
		expr = i32(1<<10),
		name = "__data_end",
		kind = .Export,
	});

	{
		code := create_function(a, {
			name = "main",
			sig = signature(a, {}, .void),
			kind = .Export,
		});

		emit_u32(code, 0); // Locals

		emit_const_i32(code, 123);
		emit_call(code, console_log);

		emit_const_f32(code, 95);
		emit_const_f32(code, 95);
		emit_const_f32(code, 60);
		emit_const_f32(code, 0);
		emit_const_f32(code, 6.28);
		emit_call(code, draw_circle);

		emit_const_i32(code, STR_OFFSET);
		emit_const_i32(code, len(STR));
		emit_call(code, log_string);

		emit_const_f32(code, 45);
		emit_const_f32(code, 190);
		emit_const_i32(code, STR_OFFSET);
		emit_const_i32(code, len(STR));
		emit_call(code, draw_text);

		emit_op(code, .End);
	}


	{
		code := create_function(a, {
			name = "hyp",
			sig = signature(a, {.f32, .f32}, .f32),
			kind = .Export,
		});

		emit_u32(code, 0); // Locals

		emit_op(code, .Local_Get); emit_u32(code, 0);
		emit_op(code, .Local_Get); emit_u32(code, 0);
		emit_op(code, .Mul_f32);

		emit_op(code, .Local_Get); emit_u32(code, 1);
		emit_op(code, .Local_Get); emit_u32(code, 1);
		emit_op(code, .Mul_f32);

		emit_op(code, .Add_f32);

		emit_op(code, .Sqrt_f32);

		emit_op(code, .End);
	}


	generate_assembly(a);

	// disassemble_byte_code(a.emitter.binary_data[:]);

	write_html_file(a.emitter.binary_data[:]);
	os.write_entire_file("test.wasm", a.emitter.binary_data[:]);
}

write_html_file :: proc(binary_data: []byte) {
	b := strings.make_builder();
	defer strings.destroy_builder(&b);
	defer os.write_entire_file("test.html", b.buf[:]);

	strings.write_string(&b,
`<!doctype html>
<html>
<head>
<title>WASM Test</title>
<style>
// * { margin: 0; padding: 0; }
html, body { width: 100%; height: 100%; }
canvas { display: block; }
#the-canvas {
	border: 1px solid #000;
}
</style>

<script type="module">
	function get_string(ptr, len) {
		const mem = new Uint8Array(window.wasm.exports.memory.buffer);
		const data = mem.slice(ptr).slice(0, len);
		const str = new TextDecoder('utf-8').decode(data);
		return str;
	}

	var the_canvas = document.getElementById("the-canvas");
	var ctx = the_canvas.getContext('2d');
	const imports = {
		platform: {
			console_log: value => console.log(value),
			draw_circle: function(x, y, r, t0, t1) {
				ctx.beginPath();
				ctx.arc(x, y, r, t0, t1);
				ctx.fillStyle = "orange";
				ctx.fill();
				ctx.stroke();
			},
			draw_text: function(x, y, ptr, len) {
				ctx.font = "30px Segoe UI";
				ctx.fillStyle = "black";
				ctx.fillText(get_string(ptr, len), x, y);
			},
			log_string: function(ptr, len) {
				const str = get_string(ptr, len);
				console.log(str);
			},
		},
	};

	var module_buffer = new Uint8Array([`);

	for v, i in binary_data {
		if i > 0 {
			strings.write_byte(&b, ',');
		}
		strings.write_string(&b, "0x");
		strings.write_u64(&b, u64(v), 16);
	}
	strings.write_string(&b, "]);");

	strings.write_string(&b, `
	const module = new WebAssembly.Module(module_buffer);
	window.wasm = new WebAssembly.Instance(module, imports);

	const linear_memory = window.wasm.memory;

	window.wasm.exports.main();

</script>
</head>
<body>
<canvas id="the-canvas" width="854px" height="480px"></canvas>
`);
	strings.write_string(&b, `</body></html>`);
}
