#include <stdio.h>
#include <string.h>

#define BAD_SERVER 1
#define BAD_CLIENT 2
#define STATIC_KEYS_PATH "static_keys"

char* show_err(int err) {
	if (err == BAD_CLIENT) {
		return "bad client";
	}
	if (err == BAD_SERVER) {
		return "bad server";
	}

	return "unknown error";
}

int get_static_keys(hydro_kx_keypair* static_keys) {
	FILE* file = fopen(STATIC_KEYS_PATH, "rb");
	if (file == NULL) {
		return make_new_keys(static_keys);
	}

	uint8_t secret[hydro_kx_SECRETKEYBYTES];
	size_t n = fread(secret, 1, hydro_kx_SECRETKEYBYTES, file);
	if (n != hydro_kx_SECRETKEYBYTES) {
	}
}

int update() {
	hydro_kx_keypair static_keys;
	int err = get_static_keys(&static_keys);
	if (err) {
		return err;
	}
}

int main_err(int argc, char** argv) {
	if (argc == 2 && strcmp(argv[1], "update") == 0) {
		return update();
	}
	return 0;
}

int main(int argc, char** argv) {
	int err = main_err(argc, argv);
	if (err) {
		puts(show_err(err));
	}
	return 0;
}

