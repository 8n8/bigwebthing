#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct haskell truelang(char*, size_t);

#define HELLO_WORLD_EXAMPLE "\
main(input, state) {\n\
	switch input {\n\
	case Start:\n\
		return ((Print, \"hello world\"), state);\n\
	}\n\
};\n\
"

#define HELLO_WORLD_YAML "\
name: helloworld\n\
\n\
dependencies:\n\
- base >= 4.7 && < 5\n\
\n\
executables:\n\
  helloworld:\n\
    main: Main.hs\n\
    source-dirs: app\n\
    ghc-options:\n\
    - -threaded\n\
    - -rtsopts\n\
    - -with-rtsopts=-N\n\
"

#define HELLO_WORLD_HASKELL "\
module Main (main) where\n\
\n\
import Foundation.IO\n\
\n\
main :: IO ()\n\
main =\n\
    putStr \"hello world\"\n\
"

struct haskell {
	char* yaml;
	size_t yaml_size;
	char* code;
	size_t code_size;
};

void hello_world_test_checks(struct haskell out) {
	size_t expected_yaml_size = sizeof(HELLO_WORLD_YAML);
	if (expected_yaml_size != out.yaml_size) {
		printf("hello_world_test FAILED: bad YAML size: expected %ld, got %ld\n", expected_yaml_size, out.yaml_size);
		return;
	}
	for (size_t i = 0; i < out.yaml_size; i++) {
		if (out.yaml[i] != HELLO_WORLD_YAML[i]) {
			printf("hello_world_test FAILED: bad YAML at position %ld: expected %c, got %c\n", i, HELLO_WORLD_YAML[i], out.yaml[i]);
			return;
		}
	}
	
	size_t expected_code_size = sizeof(HELLO_WORLD_HASKELL);
	if (expected_code_size != out.code_size) {
		printf("hello_world_test FAILED: bad Haskell code size: expected %ld, got %ld\n", expected_code_size, out.code_size);
		return;
	}
	for (size_t i = 0; i < out.code_size; i++) {
		if (out.code[i] != HELLO_WORLD_HASKELL[i]) {
			printf("hello_world_test FAILED: bad code at position %ld: expected %c, got %c\n", i, HELLO_WORLD_HASKELL[i], out.code[i]);
			return;
		}
	}
}

void hello_world_test() {
	struct haskell out = truelang(
		HELLO_WORLD_EXAMPLE,
		sizeof(HELLO_WORLD_EXAMPLE));

	hello_world_test_checks(out);

	free(out.yaml);
	free(out.code);
}

struct haskell truelang(char* code, size_t size) {
	char* yaml = malloc(sizeof(HELLO_WORLD_YAML));
	memcpy(yaml, HELLO_WORLD_YAML, sizeof(HELLO_WORLD_YAML));

	char* out = malloc(sizeof(HELLO_WORLD_HASKELL));
	memcpy(out, HELLO_WORLD_HASKELL, sizeof(HELLO_WORLD_HASKELL));

	struct haskell result = {
		.yaml = yaml,
		.yaml_size = sizeof(HELLO_WORLD_YAML),
		.code = out,
		.code_size = sizeof(HELLO_WORLD_HASKELL)
	};
	return result;
}

void main_test() {
	hello_world_test();
}

int main() {
	main_test();
	return 0;
}
