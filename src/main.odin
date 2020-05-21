package wasm

import "core:fmt"
import "core:os"
import "core:strings"

import "assembler"
import "frontend"
import "backend"

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
	init_error_system();

	p := Parser{};
	p.err = default_error_handler;
	parse_file(&p, file);

	if frontend.total_error_count != 0 {
		fmt.eprintln("[fail]");
		return;
	}


	checker := &Checker{};
	check_pkgs(checker, {pkg});
	if frontend.total_error_count != 0 {
		fmt.eprintln("[fail]");
		return;
	}

	module: backend.Module;

	backend.generate_wasm(&module, checker);

	write_html_file(module.assembler.emitter.binary_data[:]);
	os.write_entire_file("test.wasm", module.assembler.emitter.binary_data[:]);

	fmt.eprintln("[done]");
}

assemble :: proc() {
	using assembler;
	a := &Assembler{};

	env_print := import_function(a,
		"env", "print",
		signature(a, {}, .void));


	add_memory(a, 2*PAGE_SIZE, "memory");

	__heap_base := i32(PAGE_SIZE);

	add_global(a, {
		type = .i32,
		expr = __heap_base,
		name = "__heap_base",
		kind = .Export,
	});
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

		emit_call(code, env_print);

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
	const imports = {
		env: {
			print: function() {
				console.log("Hellope!");
			},
			print2: function(i) {
				console.log("Goodbye!", i);
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
`);
	strings.write_string(&b, `</body></html>`);
}
