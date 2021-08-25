#include "hydrogen.h"
#include <stdio.h>

#define SERVER_SECRET_FILE "server_secret"
#define KEYPAIR_BYTES 96


void print_public(uint8_t pk[hydro_sign_PUBLICKEYBYTES]) {
	printf("{");
	for (int i = 0; i < hydro_sign_PUBLICKEYBYTES; i++) {
		printf("%d, ", pk[i]);
	}
	printf("}");
}


int make_new_keys() {
	hydro_sign_keypair kp;
	hydro_sign_keygen(&kp);

	FILE* f = fopen(SERVER_SECRET_FILE, "wb");
	if (f == NULL) {
		printf("can't open file for writing");
		return -1;
	}
	size_t n = fwrite(
		kp.pk,
		1,
		hydro_sign_PUBLICKEYBYTES,
		f);
	if (n != hydro_sign_PUBLICKEYBYTES) {
		printf("couldn't write public key to file");
		return -1;
	}

	n = fwrite(
		kp.sk,
		1,
		hydro_sign_SECRETKEYBYTES,
		f);
	if (n != hydro_sign_SECRETKEYBYTES) {
		printf("couldn't write secret key to file");
		return -1;
	}
	
	print_public(kp.pk);
	return 0;
}


int main() {
	if (hydro_init()) {
		printf("bad hydrogen init");
		return -1;
	}
	FILE* f = fopen(SERVER_SECRET_FILE, "rb");
	if (f == NULL) {
		return make_new_keys();
	}

	hydro_sign_keypair kp;
	size_t n = fread(
		kp.pk,
		1,
		hydro_sign_PUBLICKEYBYTES,
		f);
	if (n != hydro_sign_PUBLICKEYBYTES) {
		printf("can't read public key from file");
		return -1;
	}

	n = fread(
		kp.sk,
		1,
		hydro_sign_SECRETKEYBYTES,
		f);
	if (n != hydro_sign_SECRETKEYBYTES) {
		printf("can't read secret key from file");
		return -1;
	}

	print_public(kp.pk);
}
