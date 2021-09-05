#include "hydrogen.h"
#include <stdio.h>

int main() {
	uint8_t key[hydro_pwhash_MASTERKEYBYTES];
	hydro_pwhash_keygen(key);
	for (int i = 0; i < hydro_pwhash_MASTERKEYBYTES; ++i) {
		printf("%d, ", key[i]);
	}
}
