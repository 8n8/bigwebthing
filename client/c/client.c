#include <stdio.h>
#include "hydrogen.h"


const char* keys_path = "bigwebthingSECRET";


typedef enum {
    WRITING_KEYS_TO_FILE = 1,
    READING_KEYS_FROM_FILE,
} error;


char* show_error(int err) {
    switch (err) {
    case WRITING_KEYS_TO_FILE:
        return "writing keys to file";
    case READING_KEYS_FROM_FILE:
        return "reading keys from file";
    }
    return "bad error code";
}

int create_signing_keys(hydro_sign_keypair* signing_keys) {
    hydro_sign_keypair key_pair;
    hydro_sign_keygen(&key_pair);

    FILE* f = fopen(keys_path, "wb");

    const size_t pk_count = fwrite(
        key_pair.pk,
        1,
        hydro_sign_PUBLICKEYBYTES,
        f);
    if (pk_count != hydro_sign_PUBLICKEYBYTES) {
        return WRITING_KEYS_TO_FILE;
    }

    const size_t sk_count = fwrite(
        key_pair.sk,
        1,
        hydro_sign_SECRETKEYBYTES,
        f);
    if (sk_count != hydro_sign_SECRETKEYBYTES) {
        return WRITING_KEYS_TO_FILE;
    }

    if (ferror(f)) {
        return WRITING_KEYS_TO_FILE;
    }
    return 0;
}


int get_signing_keys(hydro_sign_keypair* signing_keys) {
    FILE* f = fopen(keys_path, "rb");
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


int main_err() {
    hydro_sign_keypair signing_keys;
    int err = get_signing_keys(&signing_keys);
    if (err != 0) {
        return err;
    }
    return 0;
}

int main() {
    int err = main_err();
    if (err) {
        printf("internal error: %s", show_error(err));
    }
    return 0;
}
