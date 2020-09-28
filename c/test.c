#include <signal/signal_protocol.h>
#include <signal/key_helper.h>
#include <stdio.h>
#include "crypto_provider_openssl.h"
#include <pthread.h>


signal_crypto_provider provider = {
    .random_func = random_generator,
    .hmac_sha256_init_func = hmac_sha256_init,
    .hmac_sha256_update_func = hmac_sha256_update,
    .hmac_sha256_final_func = hmac_sha256_final,
    .hmac_sha256_cleanup_func = hmac_sha256_cleanup,
    .sha512_digest_init_func = sha512_digest_init,
    .sha512_digest_update_func = sha512_digest_update,
    .sha512_digest_final_func = sha512_digest_final,
    .sha512_digest_cleanup_func = sha512_digest_cleanup,
    .encrypt_func = encrypt,
    .decrypt_func = decrypt,
    .user_data = 0
};


pthread_mutex_t global_mutex;
pthread_mutexattr_t global_mutex_attr;


void lock_function(void *user_data) {
    pthread_mutex_lock(&global_mutex);
}


void unlock_function(void *user_data) {
    pthread_mutex_unlock(&global_mutex);
}


typedef struct {
    signal_buffer *identity_key_public;
    signal_buffer *identity_key_private;
    uint32_t local_registration_id;
} my_data_store


int identity_key_store_get_identity_key_pair(
    signal_buffer **public_data,
    signal_buffer **private_data,
    void *user_data) {

    my_data_store *data = user_data;
    *public_data = signal_buffer_copy(data->identity_key_public);
    *private_data = signal_buffer_copy(data->identity_key_private);
    return 0;
}


int identity_key_store_get_local_registration_id(
    void *user_data,
    uint32_t *registration_id) {

    my_data_store *data = user_data;
    *registration_id = data->local_registration_id;
    return 0
}


typedef struct


int identity_key_store_save_identity(
    const signal_protocol_address *address,
    uint8_t *key_data,
    size_t key_len,
    void *user_data) {

    return 0
}


typedef struct {
    bool fatal_error;
    state_type type;
} program_state


typedef enum {
} state_type


typedef struct {
    input_type type;
} io_input


int main() {
    state program_state = initState();
    input io_input = initInput();
    while (!state.fatalError) {
        output output = update(&state, input);
        input = io(output)
    }
}


int main() {
    signal_context *global_context;
    signal_context_create(&global_context, 0);
    signal_context_set_crypto_provider(global_context, &provider);

    pthread_mutexattr_init(&global_mutex_attr);
    pthread_mutexattr_settype(
        &global_mutex_attr,
        PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(
        &global_mutex,
        &global_mutex_attr);

    signal_context_set_locking_functions(
        global_context,
        lock_function,
        unlock_function);

    ratchet_identity_key_pair *identity_key_pair;
    uint32_t registration_id;
    signal_protocol_key_helper_pre_key_list_node *pre_keys_head;
    session_signed_pre_key *signed_pre_key;

    uint64_t timestamp = time(NULL);
    signal_protocol_key_helper_generate_identity_key_pair(
        &identity_key_pair,
        global_context);
    signal_protocol_key_helper_generate_registration_id(
        &registration_id,
        0,
        global_context);
    signal_protocol_key_helper_generate_pre_keys(
        &pre_keys_head,
        1,
        100,
        global_context);
    signal_protocol_key_helper_generate_signed_pre_key(
        &signed_pre_key,
        identity_key_pair,
        5,
        timestamp,
        global_context);

    signal_protocol_identity_key_store store = {
        .get_identity_key_pair =
            identity_key_store_get_identity_key_pair,
        .get_local_registration_id =
            identity_key_store_get_local_registration_id,
        .save_identity =
            identity_key_store_save_identity,
        .is_trusted_identity =
            test_identity_key_store_is_trusted_identity,
        .destroy_func =
            test_identity_key_store_destroy,
        .user_data =
            data
    };

    printf("Hello, World!");
    return 0;
}
