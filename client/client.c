#include <stdio.h>
#include "hydrogen.h"

const char* keysPath = "bigwebthingSECRET";

int main() {
    hydro_kx_keypair crypto_keys;
    int err = make_crypto_keys(&crypto_keys);
    if (err != 0) {
        return err;
    }



    FILE* secretKeyFileHandle;
    secretKeyFileHandle = fopen(keysPath, "rb");
    if (secretKeyFileHandle == NULL) {
        
    }
}
