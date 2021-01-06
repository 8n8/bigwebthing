#include <stdio.h>
#include "hydrogen.h"


const char* keys_path = "bigwebthingSECRET";


typedef enum {
    WRITING_KEYS_TO_FILE,
    READING_KEYS_FROM_FILE,
} error;


error create_signing_keys(hydro_sign_keypair* signing_keys) {
    hydro_sign_keypair key_pair;
    hydro_sign_keygen(&key_pair);

    secret_key_file_handle = fopen(keys_path, "wb");

    const site_t pk_count = fwrite(
        key_pair.pk,
        1,
        hydro_sign_PUBLICKEYBYTES,
        secret_key_file_handle);
    if (pk_count != hydro_sign_PUBLICKEYBYTES) {
        return WRITING_KEYS_TO_FILE;
    }

    const size_t sk_count = fwrite(
        key_pair.sk,
        1,
        hydro_sign_SECRETKEYBYTES,
        secret_key_file_handle);
    if (sk_count != hydro_sign_SECRETKEYBYTES) {
        return WRITING_KEYS_TO_FILE;
    }

    if (ferror(secret_key_file_handle)) {
        return WRITING_KEYS_TO_FILE;
    }
    return 0;
}


error get_signing_keys(hydro_sign_keypair* signing_keys) {
    FILE* f = fopen(keysPath, "rb");
    if (f == NULL) {
        return create_signing_keys(signing_keys);
    }

    const size_t pk_count = fread(
        signing_keys->pk,
        1,
        hydro_sign_PUBLICKEYBYTES,
        f);
    if (pk_count != hydro_sign_PUBLICKEYBYTES) {
        return READING_KEYS_FROM_FILE;
    }

    const size_t sk_count = fread(
        signing_keys->sk,
        1,
        hydro_sign_SECRETKEYBYTES,
        f);
    if (sk_count != hydro_sign_SECRETKEYBYTES) {
        return READING_KEYS_FROM_FILE;
    }

    if (ferror(f)) {
        return READING_KEYS_FROM_FILE;
    }
    return 0;
}


int main() {
    hydro_kx_keypair crypto_keys;
    int err = make_crypto_keys(&crypto_keys);
    if (err != 0) {
        return err;
    }
}