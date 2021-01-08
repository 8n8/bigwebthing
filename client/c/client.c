#include <stdio.h>
#include "hydrogen.h"
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#define KEYS_PATH "bigwebthingSECRET"
#define AUTH_CODE_LENGTH 32
#define FROM_SERVER_AUTH_LENGTH 33
#define TO_SERVER_AUTH_LENGTH 97
#define TO_SERVER_ENCRYPTED_LENGTH 162

#define FROM_SERVER_AUTH_CODE_INDICATOR 0
#define GET_REQUEST_LENGTH 25

// 36 bytes header
//   + 1 byte plain-text length
//   + 100 byte plain-text buffer
#define CIPHERTEXT_LENGTH 137

// 1 byte plain-text length + 100 bytes plain-text buffer
#define PLAINTEXT_LENGTH 101


typedef enum {
    WRITING_KEYS_TO_FILE = 1,
    READING_KEYS_FROM_FILE,
    BAD_ARGS,
    ENCRYPT_FAILED,
    COULDNT_CREATE_SOCKET,
    COULDNT_CONNECT_TO_SERVER,
    BAD_HYDRO_INIT,
    BAD_DECRYPT,
    BAD_MESSAGE_LENGTH,
    BAD_INDICATOR,
    GET_REQUEST_NO_INDICATOR,
    GET_REQUEST_NOT_SENT,
    BAD_SEND_ENCRYPTED,
    BAD_SEND_AUTH,
    BAD_SIGN_AUTHCODE,
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
        fclose(f);
        return WRITING_KEYS_TO_FILE;
    }

    const size_t sk_count = fwrite(
        key_pair.sk,
        1,
        hydro_sign_SECRETKEYBYTES,
        f);
    if (sk_count != hydro_sign_SECRETKEYBYTES) {
        fclose(f);
        return WRITING_KEYS_TO_FILE;
    }

    if (ferror(f)) {
        fclose(f);
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


#define MESSAGE_ID_LENGTH 24

// 2 * (MESSAGE_ID_LENGTH + hydro_secretbox_KEYBYTES) + 1
//     = 2 * (24 + 32) + 1 = 113
#define KEY_ID_HEX_LENGTH 113

// 24-byte message ID and 32-byte secret key is 56 bytes, which is
// 112 hex characters
#define KEY_ID_LENGTH 56
#define RAW_KEY_AND_ID_LENGTH 112

#define MESSAGE_LENGTH 100
#define ENCRYPT_CONTEXT "k2LCe3A3"
#define SIGN_CONTEXT "MjQhgLOl"
#define SERVER_CLIENT_CONTEXT "IYSfodrk"


int download_auth_code(int sock, uint8_t auth_code[AUTH_CODE_LENGTH]) {
    uint8_t auth_code_message[FROM_SERVER_AUTH_LENGTH];
    const int received =
        recv(sock, auth_code_message, from_server_AUTH_LENGTH, 0);
    if (received != FROM_SERVER_AUTH_LENGTH) {
        return BAD_SERVER_CONNECTION;
    }
    if (auth_code_message[0] != FROM_SERVER_AUTH_CODE_INDICATOR) {
        return BAD_SERVER_MESSAGE;
    }
    uint8_t auth_code[AUTH_CODE_LENGTH];
    for (int i = 0; i < AUTH_CODE_LENGTH; i++) {
        auth_code[i] = auth_code_message[i+1];
    }
    return 0;
}


int make_socket(int* sock) {
    struct sockaddr_in server_address;
    *sock = socket(AF_INET, SOCK_STREAM, 0);
    if (*sock < 0) {
        return COULDNT_CREATE_SOCKET;
    }

    memset(&server_address, 0, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_addr.s_addr = inet_addr(SERVER_ADDRESS);
    server_address.sin_port = htons(SERVER_PORT);

    const int conn_err = connect(
        *sock,
        &server_address,
        sizeof(server_address));
    if (conn_err < 0) {
        return COULDNT_CONNECT_TO_SERVER;
    }

    return 0;
}


int authenticate_socket(
    int sock, const hydro_sign_keypair* signing_keys) {

    uint8_t auth_code[AUTH_CODE_LENGTH];
    const int auth_down_err = download_auth_code(sock, auth_code);
    if (auth_down_err) {
        return auth_down_err;
    }

    uint8_t signature[hydro_sign_BYTES];
    const int sign_err = hydro_sign_create(
        signature,
        auth_code,
        AUTH_CODE_LENGTH, 
        SIGN_CONTEXT,
        signing_keys->sk);
    if (sign_err) {
        return BAD_SIGN_AUTHCODE;
    }

    uint8_t signed_auth[TO_SERVER_AUTH_LENGTH];
    signed_auth[0] = 0;
    for (int i = 0; i < hydro_sign_PUBLICKEYBYTES; i++) {
        signed_auth[i+1] = signing_keys->pk[i];
    }
    for (int i = 0; i < hydro_sign_BYTES; i++) {
        signed_auth[i+1+hydro_sign_PUBLICKEYBYTES] = signature[i];
    }
    
    const int send_auth_err =
        send(sock, signed_auth, TO_SERVER_AUTH_LENGTH, 0);
    if (send_auth_err != TO_SERVER_AUTH_LENGTH) {
        return BAD_SEND_AUTH;
    }

    return 0;
}


int encrypt(
    uint8_t ciphertext[CIPHERTEXT_LENGTH],
    const uint8_t plaintext[PLAINTEXT_LENGTH],
    const uint8_t secretkey[hydro_secretbox_KEYBYTES]) {

    return hydro_secretbox_encrypt(
        ciphertext,
        plaintext,
        PLAINTEXT_LENGTH,
        0,
        ENCRYPT_CONTEXT,
        secretkey);
}


// MESSAGE_ID_LENGTH + hydro_secretbox_KEYBYTES = 24 + 32 = 56
#define KEY_ID_LENGTH 56

void print_id(
    uint8_t message_id[MESSAGE_ID_LENGTH],
    uint8_t key[hydro_secretbox_KEYBYTES]) {

    uint8_t bin[KEY_ID_LENGTH];
    for (int i = 0; i < MESSAGE_ID_LENGTH; i++) {
        bin[i] = message_id[i];
    }
    for (int i = 0; i < hydro_secretbox_KEYBYTES; i++) {
        bin[i + KEY_ID_LENGTH] = key[i];
    }

    char hex[KEY_ID_HEX_LENGTH];
    hydro_bin2hex(hex, KEY_ID_HEX_LENGTH, bin, KEY_ID_LENGTH);
    printf("%s", hex);
}


int send_encrypted(
    int sock, 
    const uint8_t message_id[MESSAGE_ID_LENGTH],
    const uint8_t ciphertext[CIPHERTEXT_LENGTH]) {

    uint8_t to_server[TO_SERVER_ENCRYPTED_LENGTH];
    to_server[0] = 1;
    for (int i = 0; i < MESSAGE_ID_LENGTH; i++) {
        to_server[1 + i] = message_id[i];
    }
    for (int i = 0; i < CIPHERTEXT_LENGTH; i++) {
        to_server[1 + MESSAGE_ID_LENGTH + i] = ciphertext[i];
    }

    const int sent = send(
        sock, to_server, TO_SERVER_ENCRYPTED_LENGTH, 0);
    if (sent != TO_SERVER_ENCRYPTED_LENGTH) {
        return BAD_SEND_ENCRYPTED;
    }

    return 0;
}


int bwt_send_use_sock(
    const hydro_sign_keypair* signing_keys,
    int sock,
    const uint8_t plaintext[PLAINTEXT_LENGTH]) {

    const int auth_err = authenticate_socket(sock, signing_keys);
    if (auth_err) {
        return auth_err;
    }

    uint8_t key[hydro_secretbox_KEYBYTES];
    hydro_secretbox_keygen(key);
    uint8_t ciphertext[CIPHERTEXT_LENGTH];
    if (encrypt(ciphertext, plaintext, key)) {
        return ENCRYPT_FAILED;
    }

    uint8_t message_id[MESSAGE_ID_LENGTH];
    hydro_random_buf(message_id, MESSAGE_ID_LENGTH);

    const int send_err = send_encrypted(sock, message_id, ciphertext);
    if (send_err) {
        return send_err;
    }

    print_id(message_id, key);

    return 0;
}


int bwt_get() {
    int sock;
    const int sock_err = make_socket(&sock);
    if (sock_err) {
        return sock_err;
    }

    const int read_err = bwt_get_use_sock(sock);
    close(sock);
    if (read_err) {
        return read_err;
    }
    return 0;
}


int one_simple_arg(char** argv) {
    if (strcmp(argv[1], "help") == 0) {
        return bwt_help();
    }
    if (strcmp(argv[1], "myid") == 0) {
        return bwt_myid();
    }
    if (strcmp(argv[1], "get") == 0) {
        return bwt_get();
    }
    return BAD_ARGS;
}


const char* usage =
    "Get usage\n"
    "\n"
    "    $ bwt help\n"
    "\n"
    "Get my id\n"
    "    $ bwt myid\n"
    "\n"
    "Download a message to STDOUT\n"
    "\n"
    "    $ bwt get <message ID>\n"
    "\n"
    "Send a message from STDIN\n"
    "\n"
    "    $ bwt send\n";


int decrypt(
    uint8_t plaintext[PLAINTEXT_LENGTH],
    const uint8_t ciphertext[CIPHERTEXT_LENGTH],
    const uint8_t key[hydro_secretbox_KEYBYTES]) {

    return hydro_secretbox_decrypt(
            plaintext,
            ciphertext,
            CIPHERTEXT_LENGTH,
            0,
            ENCRYPT_CONTEXT,
            key);
}


void print_message(uint8_t plaintext[PLAINTEXT_LENGTH]) {
    for (int i = 0; i < plaintext[0]; i++) {
        putchar(plaintext[i+1]);
    }
}


int request_message(
    int sock,
    const uint8_t message_id[MESSAGE_ID_LENGTH],
    const uint8_t key[hydro_secretbox_KEYBYTES]) {

    uint8_t request[GET_REQUEST_LENGTH];
    request[0] = 2;
    for (int i = 0; i < MESSAGE_ID_LENGTH; i++) {
        request[i+1] = message_id[i];
    }

    const int bytes_sent = send(sock, request, GET_REQUEST_LENGTH, 0);
    if (bytes_sent != GET_REQUEST_LENGTH) {
        return GET_REQUEST_NOT_SENT;
    }

    uint8_t indicator[1];
    if (recv(sock, indicator, 1, 0) != 1) {
        return GET_REQUEST_NO_INDICATOR;
    }

    if (indicator[0] == 2) {
        printf("no such message");
        return 0;
    }

    if (indicator[0] != 1) {
        return BAD_INDICATOR;
    }

    uint8_t ciphertext[CIPHERTEXT_LENGTH];
    const int received = recv(sock, ciphertext, CIPHERTEXT_LENGTH, 0);
    if (received != CIPHERTEXT_LENGTH) {
        return BAD_MESSAGE_LENGTH;
    }

    uint8_t plaintext[PLAINTEXT_LENGTH];
    if (decrypt(plaintext, ciphertext, key)) {
        return BAD_DECRYPT;
    }
    print_message(plaintext);
    return 0;
}


int create_static_keys_help(*FILE f, hydro_kx_keypair* static_keys) {
    const size_t pk_count = fwrite(
        static_keys.pk,
        1,
        hydro_kx_PUBLICKEYBYTES,
        f);
    if (pk_count != hydro_kx_PUBLICKEYBYTES) {
        return BAD_WRITE_PK_TO_FILE;
    }

    const size_t sk_count = fwrite(
        static_keys.sk,
        1,
        hydro_kx_SECRETKEYBYTES,
        f);
    if (sk_count != hydro_sign_SECRETKEYBYTES) {
        return BAD_WRITE_SK_TO_FILE;
    }

    return 0
}


int create_static_keys(hydro_kx_keypair* static_keys) {
    hydro_kx_keygen(static_keys);

    FILE* f = fopen(keys_path, "wb");
    const int err = create_static_keys_help(f, static_keys);
    close(f);
    return err;
}


int get_static_keys(hydro_kx_keypair* static_keys) {
    FILE* f = fopen(keys_path, "rb");
    const int err = keys_file_help(static_Keys, f);
    close(f);
    return err;
}

int keys_file_help(hydro_kx_keypair* static_keys, FILE* f) {
    if (f == NULL) {
        return create_static_keys(static_keys);
    }

    const size_t pk_count = fread(
        static_keys->pk,
        1,
        hydro_kx_PUBLICKEYBYTES,
        f);
    if (pk_count != hydro_kx_PUBLICKEYBYTES) {
        return BAD_READ_PK_FROM_FILE;
    }

    const size_t sk_count = fread(
        static_keys->sk,
        1,
        hydro_kx_SECRETKEYBYTES,
        f);
    if (sk_count != hydro_kx_SECRETKEYBYTES) {
        return BAD_READ_SK_FROM_FILE;
    }

    return 0;
}


int get_session_keys(
    int sock,
    hydro_kx_session_keypair* session_kp,
    hydro_kx_keypair* static_keys) {

    uint8_t packet1[hydro_kx_XX_PACKET1BYTES];
    hydro_kx_state kx_state;
    if (hydro_kx_xx_1(&kx_state, packet1, NULL)) {
        return BAD_KX_XX1;
    }

    const int p1l = send(sock, packet1, hydro_kx_XX_PACKET1BYTES, 0);
    if (p1l != hydro_kx_XX_PACKET1BYTES) {
        return BAD_XX1_SEND;
    }

    uint8_t packet2[hydro_kx_XX_PACKET2BYTES];
    const int received = recv(sock, packet2, hydro_kx_XX_PACKET2BYTES);
    if (recieved != hydro_kx_XX_PACKET2BYTES) {
        return BAD_XX2_RECEIVE;
    }

    uint8_t packet3[hydro_kx_XX_PACKET3BYTES];
    uint8_t server_pk[hydro_kx_PUBLICKEYBYTES];

    const int xx3_err = hydro_kx_xx_3(
        &kx_state,
        &session_kp,
        packet3,
        server_pk,
        packet2,
        NULL,
        static_keys);
    if (xx3_err) {
        return BAD_MAKE_XX3;
    }

    for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; i++) {
        if (server_pk[i] != known_server_pk[i]) {
            return FAKE_SERVER;
        }
    }

    const int p3l = send(sock, packet3, hydro_kx_XX_PACKET3BYTES, 0);
    if (p3l != hydro_kx_XX_PACKET3BYTES) {
        return BAD_XX3_SEND;
    }
    return 0;
}


// An encrypted 24-byte blob ID: 24 + hydro_secretbox_HEADERBYTES
#define from_server_PING_SIZE 60

// An encrypted 'no such thing' message:
//
//     one byte + hydro_secretbox_HEADERBYTES
//
#define from_server_NO_SUCH_THING_SIZE 37

// An encrypted blob:
//
//    maximum blob size 16KB + hydro_secretbox_HEADERBYTES
//
#define MAX_MESSAGE_SIZE 16000


int process_blob_help(
    uint8_t* ciphertext,
    uint8_t* plaintext,
    hydro_kx_keypair* static_keys,
    uint8_t file_id[FILE_ID_SIZE],
    struct session_state* session_state) {

    const int decrypt_err =
        hydro_secretbox_decrypt(
            plaintext,
            ciphertext,
            ciphertext_size,
            session_state->counter,
            SERVER_CLIENT_CONTEXT,
            session_state->keys->rx);
    if (decrypt_err != 0) {
        return BAD_DECRYPT_SERVER_BLOB;
    }

    int initiator_err =
        process_blob_as_initiator(plaintext, file_id, static_keys);
    if (initiator_err != 0) {
        return initiator_err;
    }

    return process_blob_as_responder(plaintext, file_id, static_keys);
}


typedef struct {
    int cursor;
    hydro_kx_state state;
    hydro_kx_session_keypair session_keys;
} parser_state;
            

int process_blob_as_initiator(
    const uint8_t* plaintext,
    const int plantext_size,
    const uint8_t file_id[FILE_ID_SIZE],
    const hydro_kx_keypair* static_keys) {

    parser_state state;
    state.cursor = 0;

    const int xx1_err = process_XX_1(
        plaintext,
        plaintext_size,
        &state);
    if (xx1_err != 0) {
        return xx1_err;
    }

    if (state.cursor >= plaintext_size) {
        return 0;
    }

    const int xx2_err = process_XX_2(
        plaintext,
        plaintext_size,
        &state);
    if (xx2_err != 0) {
        return xx2_err;
    }

    if (state.cursor >= plaintext_size) {
        return 0;
    }

    const int xx3_err = process_XX_3(
        plaintext,
        plaintext_size,
        &state);
    if (xx3_err != 0) {
        return xx3_err;
    }

    if (state.cursor >= plaintext_size) {
        return 0;
    }

    while (cursor < plaintext_size) {

        const int transport_err = process_transport(
            plaintext,
            plaintext_size,
            &state);
        if (transport_err != 0) {
            return transport_err;
        }

        if (state.cursor >= plaintext_size) {
            return 0;
        }
    }
    return INITIATOR_PROCESS_NEVER_HAPPENS;
}


int process_blob(
    uint8_t* blob,
    struct session_state* session_state,
    hydro_kx_keypair* static_keys,
    uint8_t file_id[FILE_ID_SIZE],
    int sock) {

    const int message_length =
        recv(sock, blob, from_server_MAX_BLOB_SIZE, 0);
    if (message_length <= hydro_secretbox_HEADERBYTES) {
        return BAD_BLOB_BODY_RECV;
    }

    uint8_t* plaintext =
        malloc(message_length - hydro_secrebox_HEADERBYTES);
    const int helper_err = process_blob_help(
        blob,
        plaintext,
        static_keys,
        file_id,
        session_state);
    free(plaintext);
    return helper_err;
}


int process_ping(
    const uint8_t buf[PING_SIZE],
    struct session_state* session_state,
    hydro_kx_keypair* static_keys,
    int sock) {

    uint8_t file_id[FILE_ID_SIZE];
    const int decrypt_err =
        hydro_secretbox_decrypt(
            file_id,
            buf,
            PING_SIZE,
            session_state->counter,
            SERVER_CLIENT_CONTEXT,
            session_state->keys->rx);
    if (decrypt_err != 0) {
        return BAD_SERVER_DECRYPT;
    }
    session_state->counter++;

    uint8_t* blob = malloc(from_server_MAX_BLOB_SIZE);
    const int process_blob_err = process_blob(
        blob,
        session_state,
        static_keys,
        file_id,
        sock);
    free(blob);
    if (process_blob_err) {
        return process_blob_err;
    }
}


int get_message_help(
    int sock,
    const hydro_kx_keypair* static_keys,
    const hydro_kx_session_keypair* session_keys,
    int* xx_counter,
    uint8_t* buf) {

    const int n = recv(sock, buf, MAX_MESSAGE_SIZE, *xx_counter);
    if (n <= hydro_secretbox_HEADERBYTES) {
        return MESSAGE_FROM_SERVER_TOO_SHORT;
    }

    const int plain_size = n - hydro_secretbox_HEADERBYTES;
    uint8_t* plaintext = malloc(plain_size);

    *xx_counter += 1;

    const int decrypt_err =
        hydro_secretbox_decrypt(
            plaintext,
            buf,
            n,
            *xx_counter,
            ENCRYPT_CONTEXT,
            session_keys->rx);
    if (decrypt_err != 0) {
        return BAD_SECRETBOX_DECRYPT;
    }

    if (n == 1) {
        if (plaintext[0] != 0) {
            return BAD_MESSAGE_FROM_SERVER;
        }
        printf("no messages");
        return 0;
    }
    if (n < 34) {
        return BAD_MESSAGE_FROM_SERVER;
    }
    if (plaintext[0] != 1) {
        return BAD_MESSAGE_FROM_SERVER;
    }
    uint8_t sender_id[hydro_kx_PUBLICKEYBYTES];
    for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; i++) {
        sender_id[i] = plaintext[i + 1];
    }

    const int message_size = plain_size - 1 - hydro_kx_PUBLICKEYBYTES;

    uint8_t* message = malloc(message_size);
    for (int i = 0; i < message_size; i++) {
        message[i] = plaintext[1 + hydro_kx_PUBLICKEYBYTES + i];
    }
    const int c2c_err = process_client_to_client(
        static_keys,
        sock,
        xx_counter,
        session_keys,
        sender_id,
        message,
        message_size);
    free(message);
    return c2c_err;
}


int process_client_to_client(
    const hydro_kx_keypair* static_keys,
    int sock,
    int* xx_counter,
    const hydro_kx_session_keypair* session_keys
    uint8_t sender_id[hydro_kx_PUBLICKEYBYTES],
    uint8_t* message,
    int message_size) {
}



int bwt_get_use_sock(int sock) {
    hydro_kx_keypair* static_keys;
    int static_key_err = get_static_keys(&static_keys);
    if (static_key_err) {
        return static_key_err;
    }

    hydro_kx_session_keypair session_keys;
    const int key_err =
        get_session_keys(sock, &session_keys, static_keys);
    if (key_err) {
        return key_err;
    }

    int xx_counter = 0;
    uint8_t get_messages[1] = {1};
    const int xx_err =
        to_server(&session_keys, &xx_counter, get_messages);
    if xx_err != 0 {
        return xx_err;
    }

    uint8_t* buf = malloc(MAX_MESSAGE_SIZE);
    const int get_err =
        get_message_help(sock, static_keys, &session_keys, &xx_counter, buf);
    free(buf);
    return get_err;
}


int main_err(int argc, char** argv) {
    if (argc == 2) {
        return one_simple_arg(argv);
    }
    if (argc == 3) {
        return two_args(argv);
    }
    return BAD_ARGS;
}


int main(int argc, char** argv) {
    int err = main_err(argc, argv);
    if (err) {
        printf("internal error: %s", show_error(err));
    }
    return 0;
}
