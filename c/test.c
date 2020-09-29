#include <signal/signal_protocol.h>
#include <signal/key_helper.h>
#include <stdio.h>
#include "crypto_provider_openssl.h"
#include <pthread.h>
#include <sqlite3.h>


typedef struct {
    state_type type;
    state_value value;
} program_state;


typedef enum {
    Empty;
} state_type


typedef union {
    void dont_care;
} state_value;


typedef struct {
    output_type type;
    output_value value;
} io_output;


typedef union {
    void dont_care;
} output_value;


typedef enum {
    DoNothing;
} output_type;


typedef struct {
    input_type type;
    input_value value;
} io_input;


typedef union {
    void dont_care;
} input_value;


typedef enum {
    Start;
} input_type;


program_state init_state() {
    program_state s;
    return s;
}


io_input init_input() {
    void dont_care;
    return {
        .type = Start;
        .value = dont_care;
    };
}


io_output init_output() {
    return {
        .type = 
    };
}


int main() {
    program_state state = init_state();
    io_input input = init_input();
    io_output output = init_output();

    while (!state.fatal) {
        update(&state, &input, &output);
        io(&input, &output);
    }
}


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
    signal_buffer* id_key_public;
    signal_buffer* id_key_private;
    uint32_t local_registration_id;
} my_data_store


int get_identity_key_pair(
    signal_buffer** public_data,
    signal_buffer** private_data,
    void* user_data) {

    my_data_store* const my_data = user_data;
    *public_data = signal_buffer_copy(my_data->id_key_public);
    *private_data = signal_buffer_copy(my_data->id_key_private);
    return 0;
}


int read_signal_buffer_from_file(
    char const* const path,
    signal_buffer** data) {

    FILE* const file_handle = fopen(path, "rbx");
    if (file_handle == NULL) {
        return -1;
    }

    int const fseekErr = fseek(file_handle, 0, SEEK_END);
    if (seekErr != 0) {
        return fseekErr;
    }
    int const seekErr = ferror(file_handle);
    if (fileErr != 0) {
        return seekErr;
    }
    uint32_t const file_size = ftell(file_handle);
    rewind(file_handle);

    uint8_t* const buffer = malloc(file_size);
    if (buffer == NULL) {
        return -1;
    }

    size_t const num_bytes_from_file = fread(
        buffer,
        1,
        file_size,
        file_handle);

    int const readErr = ferror(file_handle);
    if (readErr != 0) {
        return readErr;
    }
    int const closeErr = fclose(file_handle);
    if (closeErr != 0) {
        return closeErr;
    }
    if (num_bytes_from_file != file_size) {
        return -1;
    }

    signal_buffer const with_length = {
        .buffer = buffer,
        .len = file_size
    }

    *data = &signal_buffer;

    return 0;
}


int get_local_registration_id(
    void* user_data,
    uint32_t* registration_id) {

    my_data_store *my_data = user_data;
    *registration_id = data->local_registration_id;
    return 0
}

int is_trusted_identity(
    const signal_protocol_address* address,
    uint8_t* key_data,
    size_t key_len,
    void* user_data) {

    my_data_store const* const my_data = user_data;

    sqlite3* const db;

    int const openStatus = sqlite3_open(my_data->db_path, &db);
    if (openStatus != SQLITE_OK) {
        sqlite3_close(db);
        return -1;
    }

    sqlite3_stmt* const stmt
    char const* const sql =
        "SELECT FROM TABLE remote_ids (name, device_id, key) "
        "WHERE name=? AND device_id=? AND key=?;"
    if (sqlite3_prepare_v2(db, sql, -1, stmt, 0) != SQLITE_OK) {
        return -1;
    }

    int const name_bind =
        sqlite3_bind_text(stmt, 1, address->name, -1, 0);
    if (name_bind != SQLITE_OK) {
        return -1;
    }

    int const id_bind =
        sqlite3_bind_int(stmt, 2, address->device_id);
    if (id_bind != SQLITE_OK) {
        return -1;
    }

    int const key_bind =
        sqlite3_bind_blob(stmt, 3, key_data, key_len, SQLITE_STATIC);
    if (key_bind != SQLITE_OK) {
        return -1;
    }

    return sqlite3_step(stmt) == SQLITE_ROW;
}


int save_identity(
    const signal_protocol_address *address,
    uint8_t* key_data,
    size_t key_len,
    void* user_data) {

    my_data_store const* const my_data = user_data;

    sqlite3* const db;

    int const openStatus = sqlite3_open(my_data->db_path, &db);
    if (openStatus != SQLITE_OK) {
        sqlite3_close(db);
        return -1;
    }

    sqlite3_stmt* const stmt;
    char const* const sql =
        "REPLACE INTO TABLE remote_ids (name, device_id, key) "
        "VALUES (?, ?, ?);"
    if (sqlite3_prepare_v2(db, sql, -1, stmt, 0) != SQLITE_OK) {
        return -1;
    }
    
    int const name_bind =
        sqlite3_bind_text(stmt, 1, address->name, -1, 0);
    if (name_bind != SQLITE_OK) {
        return -1;
    }

    int const id_bind =
        sqlite3_bind_int(stmt, 2, address->device_id);
    if (id_bind != SQLITE_OK) {
        return -1;
    }

    int const key_bind =
        sqlite3_bind_blob(stmt, 3, key_data, key_len, SQLITE_STATIC);
    if (key_bind != SQLITE_OK) {
        return -1;
    }

    if (sqlite3_finalize(stmt) != SQLITE_OK) {
        return -1;
    }

    if (sqlite3_close(db) != SQLITE_OK) {
        return -1;
    }

    return 0
}


int destroy_func(void* user_data) {
    
}


int main() {
    signal_context *global_context;
    signal_context_create(&global_context, 0);
    signal_context_set_crypto_provider(global_context, &provider);

    my_data_store const my_data;

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

    int const pubErr =
        read_signal_buffer_from_file(
            "publicKey",
            my_data.public_data);
    if (pubErr != 0) {
        return pubErr;
    }

    int const privErr =
        read_signal_buffer_from_file(
            "privateKey",
            my_data.private_data);
    if (privErr != 0) {
        return privErr;
    }

    signal_protocol_identity_key_store store = {
        .get_identity_key_pair = get_identity_key_pair,
        .get_local_registration_id = get_local_registration_id,
        .save_identity = save_identity,
        .is_trusted_identity = is_trusted_identity,
        .destroy_func = destroy_func,
        .user_data =
            data
    };

    printf("Hello, World!");
    return 0;
}


int read_signal_buffer_from_file(
    char const* const path,
    signal_buffer** data) {

    FILE* const file_handle = fopen(path, "rbx");
    if (file_handle == NULL) {
        return -1;
    }

    int const fseekErr = fseek(file_handle, 0, SEEK_END);
    if (seekErr != 0) {
        return fseekErr;
    }
    int const seekErr = ferror(file_handle);
    if (fileErr != 0) {
        return seekErr;
    }
    uint32_t const file_size = ftell(file_handle);
    rewind(file_handle);

    uint8_t* const buffer = malloc(file_size);
    if (buffer == NULL) {
        return -1;
    }

    size_t const num_bytes_from_file = fread(
        buffer,
        1,
        file_size,
        file_handle);

    int const readErr = ferror(file_handle);
    if (readErr != 0) {
        return readErr;
    }
    int const closeErr = fclose(file_handle);
    if (closeErr != 0) {
        return closeErr;
    }
    if (num_bytes_from_file != file_size) {
        return -1;
    }

    signal_buffer const with_length = {
        .buffer = buffer,
        .len = file_size
    }

    *data = &signal_buffer;

    return 0;
}
