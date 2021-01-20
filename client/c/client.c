#include <stdio.h>
#include "hydrogen.h"
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <sqlite3.h>

const char* KEYS_PATH = "bigwebthingSECRET";
const int TO_SERVER_ENCRYPTED_LENGTH = 162;

const int FROM_SERVER_AUTH_CODE_INDICATOR = 0;
const int GET_REQUEST_LENGTH = 25;

// 36 bytes header
//   + 1 byte plain-text length
//   + 100 byte plain-text buffer
const int CIPHERTEXT_SIZE =
	hydro_secretbox_HEADERBYTES + PLAINTEXT_SIZE;

// 1 byte plain-text length + 100 bytes plain-text buffer
const int PLAINTEXT_SIZE = 101;
const int NUM_KKs = 100;

const char* DB_PATH = "db.sqlite";

enum error {
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
    BAD_DB_CLOSE,
    BAD_DB_FINALIZE,
    BAD_DB_OPEN,
    BAD_MAKE_TX_KEY_TABLE,
    BAD_MAKE_TX_STATE_TABLE,
    BAD_MAKE_RX_TABLE,
    BAD_MAKE_CONTACT_TABLE,
    BAD_DB_STEP_CONTACT,
    BAD_DB_BIND,
    BAD_DB_PREPARE_CONTACT,
    BAD_DB_STEP,
    BAD_DATABASE_PREPARE_KK1,
    BAD_STDIN_READ,
    BAD_CHAR,
    BAD_DATABASE_PREPARE,
};

const char* SAVE_CONTACT =
	"INSERT INTO contacts (theirid) VALUES (?);";

const int SAVE_CONTACT_SIZE = 42;

const char* SAVE_KK1 =                 // char count
	"INSERT INTO txstate "         // 20
	"(sessionid, theirid, state) " // 28
	"VALUES (?, ?, ?);";           // 17
	                               // --
				       // 65
				       // ==

const int SAVE_KK1_SIZE = 65;

const char* TX_KEY_TABLE =                    // char count
	"CREATE TABLE IF NOT EXISTS txkeys (" //  35
	"sessionid blob UNIQUE NOT NULL, "    //  32
	"theirid blob NOT NULL, "             //  23
	"key blob NOT NULL);";                //  19
	                                      // ---
					      // 109
					      // ===

const int TX_KEY_TABLE_SIZE = 109;

const char* TX_STATE_TABLE =                   // char count
	"CREATE TABLE IF NOT EXISTS txstate (" //  36
	"sessionid blob UNIQUE NOT NULL, "     //  32
	"theirid blob NOT NULL, "              //  23
	"state blob NOT NULL);";                 //  19
	                                       // ---
					       // 110
					       // ===

const int TX_STATE_TABLE_SIZE = 110;

const char* RX_KEY_TABLE =                    // char count
	"CREATE TABLE IF NOT EXISTS rxkeys (" //  35
	"sessionid blob UNIQUE NOT NULL, "    //  32
	"theirid blob NOT NULL, "             //  23
	"key blob NOT NULL);";                //  19
	                                      // ---
					      // 109
					      // ===

const int RX_KEY_TABLE_SIZE = 109;

const char* CONTACT_TABLE =                     // char count
	"CREATE TABLE IF NOT EXISTS contacts (" // 37
	"theirid blob UNIQUE NOT NULL);";       // 30
	                                        // --
						// 67
						// ==

const int CONTACT_TABLE_SIZE = 67;

struct noise_xx_state {
	hydro_secretbox_keypair kp;
	int counter;
}

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

const int MESSAGE_ID_SIZE = 24;

const int KEY_ID_HEX_LENGTH =
	2 * (MESSAGE_ID_SIZE + hydro_secretbox_KEYBYTES) + 1;

const MESSAGE_SIZE = 100;
const char* ENCRYPT_CONTEXT = "k2LCe3A3";
const char* SIGN_CONTEXT = "MjQhgLOl";
const char* SERVER_CLIENT_CONTEXT = "IYSfodrk";

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

struct Secrets {
	hydro_kx_keypair static_keys;
	uint8_t* contacts[hydro_kx_PUBLICKEYBYTES];
}

int bwt_read_help(FILE* fsecret, FILE* fpublic) {
	struct Secrets secrets;
	if (fsecret == NULL) {
		int err = make_secrets(&secrets);
		if (err) {
			return err;
		}
	}
}

int bwt_read() {
	FILE* fsecret = fopen(secret_path, "rb");
	FILE* fpublic = fopen(public_path, "rb");
	int err = bwt_read_help(fsecret, fpublic)
	if (fsecret != NULL) {
		fclose(fsecret);
	}
	if (fpublic != NULL) {
		fclose(fpublic);
	}
	return err;
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

int bwt_help() {
	puts(usage);
	return 0;
}

const hex_size = (hydro_kx_PUBLICKEYBYTES * 2) + 1;

int bwt_myid() {
	hydro_kx_keypair static_keys;
	int err = get_static_keys(&static_keys);
	if (err) {
		return err;
	}

	char hex[hex_size];
	char* hex =
		hydro_bin2hex(
			hex,
			hex_size,
			static_keys.pk,
			hydro_kx_PUBLICKEYBYTES);
	if (hex == NULL) {
		return HEX_OVERFLOW;
	}
	puts(hex);
}

int one_simple_arg(char** argv) {
    if (strcmp(argv[1], "help") == 0) {
        return bwt_help();
    }
    if (strcmp(argv[1], "myid") == 0) {
        return bwt_myid();
    }
    if (strcmp(argv[1], "read") == 0) {
        return bwt_read();
    }
    return BAD_ARGS;
}

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

const int MAX_MESSAGE_SIZE = 16000;

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

int process_message_from_client(
	struct noise_xx_state* xx_state,
	struct message_from_client* msg_from,
	hydro_kx_keypair* static_keys) {

	int err = check_sender_in_contacts(msg_from->sender);
	if (err) {
		return err;
	}

	switch msg_from->type {
	case KK1s:
		return process_kk1s(
			msg_from->msg,
			static_keys,
			xx_state);
	case KK2s:
		return process_kk2s(msg_from->msg, static_keys);

	case KKTransport:
		return process_transport(
			msg_from->msg,
			static_keys,
			xx_state);
	}

	return BAD_CLIENT_TO_CLIENT_INDICATOR;
}

struct from_client {
	from_client_type type;
	uint8_t sender[hydro_kx_PUBLICKEYBYTES];
	message_from from;
}

const int KK1_INDICATOR = 0;
const int KK2_INDICATOR = 1;
const int KKTRANSPORT_INDICATOR = 2;

const int NO_MESSAGES_INDICATOR = 0;
const int MESSAGE_INDICATOR = 1;

const int TRANSPORT_SIZE =
	1 +
	SESSION_ID_SIZE +
	hydro_secretbox_HEADERBYTES +
	PLAINTEXT_SIZE;

const int KK_SIZE = hydro_kx_KK_PACKET1 + SESSION_ID_SIZE;

// 1 + 32 + 7201 = 7234
// (indicator + recipient + client to client)
const int KKS_SIZE = 1 + 32 + 7201;

int kk_start(int i) {
	return 1 + hydro_kx_PUBLICKEYBYTES + 1 + (i * KK_SIZE);
}

int process_kk1(
	int i,
	uint8_t their_pk[hydro_kx_PUBLICKEYBYTES],
	uint8_t* kk2s,
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	uint8_t kk1[KK_SIZE];
	memcpy(kk1, message + kk1_start(i), KK_SIZE);

	hydro_kx_session_keypair session_kp;
	uint8_t kk2[hydro_kx_KK_PACKET2BYTES];
	int err = hydro_kx_kk_2(
		&session_kp,
		kk2,
		kk1,
		their_pk,
		static_keys);
	if (err) {
		return COULDNT_DECRYPT_KK1;
	}

	memcpy(kk2s + kk2_start(i), kk2, hydro_kx_KK_PACKET2BYTES);

	uint8_t session_id[SESSION_ID_SIZE];
	memcpy(
		session_id,
		message + kk_start(i) + hydro_kx_PACKET1BYTES,
		SESSION_ID_SIZE);
	memcpy(
		kk2s + kk_start(i) + hydro_kx_PACKET2BYTES,
		session_id,
		SESSION_ID_SIZE);

	return save_receiving(their_pk, session_id, session_kp.rx);
}

int kk1s_help(
	uint8_t* kk2s,
	struct noise_xx_state* xx_state,
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	kk2s[0] = 0;
	memcpy(kk2s + 1, message + 1, hydro_kx_PUBLICKEYBYTES);

	uint8_t their_pk[hydro_kx_PUBLICKEYBYTES];
	memcpy(their_pk, message + 1, hydro_kx_PUBLICKEYBYTES);

	kk2s[33] = KK2_INDICATOR;
	for (int i = 0; i < NUM_KKs; i++) {
		int err = process_kk1(
			i,
			their_pk,
			kk2s,
			message,
			static_keys);
		if (err) {
			return err;
		}
	}
	return to_server(xx_state, kk2s);
}

int process_kk1s(
	struct noise_xx_state* xx_state,
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	uint8_t* kk2s = malloc(KKS_SIZE);
	int err = kk1s_help(kk2s, xx_state, message, static_keys);
	free(kk2s);
	return err;
}

int process_kk2(
	int i,
	uint8_t their_pk[hydro_kx_PUBLICKEYBYTES],
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	uint8_t session_id[SESSION_ID_SIZE];
	memcpy(
		session_id,
		message + kk_start(i) + hydro_kx_PACKET2BYTES,
		SESSION_ID_SIZE);

	hydro_kx_state kx_state;
	int err = get_kx_state(session_id, &kx_state);
	if (err) {
		return err;
	}

	uint8_t kk2[hydro_kx_PACKET2BYTES];
	memcpy(kk2, message + kk_start(i), hydro_kx_PACKET2BYTES);

	hydro_kx_session_keypair session_kp;
	err = hydro_kx_kk_3(
		&kx_state,
		&session_kp,
		kk2,
		static_keys);
	if (err) {
		return err;
	}

	err = delete_sending_handshake(session_id)
	if (err) {
		return err;
	}

	return save_sending_secret(
		session_id,
		&session_kp.tx,
		their_pk);
}

int process_kk2s(uint8_t* message, hydro_kx_keypair* static_keys) {
	uint8_t their_pk[hydro_kx_PUBLICKEYBYTES];
	memcpy(their_pk, message + 1, hydro_kx_PUBLICKEYBYTES);

	for (int i = 0; i < NUM_KKs; i++) {
		int err = process_kk2(
			i,
			their_pk,
			message,
			static_keys);
		if (err) {
			return err;
		}
	}
	return 0;
}

int process_transport(
	struct noise_xx_state* xx_state,
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	uint8_t session_id[SESSION_ID_SIZE];
	memcpy(
		session_id,
		message + 1 + hydro_kx_PUBLICKEYBYTES + 1,
		SESSION_ID_SIZE);

	uint8_t their_pk[hydro_kx_PUBLICKEYBYTES];
	memcpy(their_pk, message + 1, hydro_kx_PUBLICKEYBYTES);

	uint8_t secret[hydro_secretbox_KEYBYTES];
	int err = get_receive_secret(session_id, their_pk);
	if (err) {
		return err;
	}

	uint8_t plaintext[PLAINTEXT_SIZE];
	uint8_t ciphertext[CIPHERTEXT_SIZE];
	err = hydro_secretbox_decrypt(
		plaintext,
		ciphertext,
		CIPHERTEXT_SIZE,
		0,
		ENCRYPT_CONTEXT,
		secret);
	if (err) {
		return BAD_SECRETBOX_DECRYPT;
	}

	err = delete_receiving(session_id);
	if (err) {
		return err;
	}

	pretty_message(plaintext);
	return 0;
}

int process_c2c(
	struct noise_xx_state* xx_state,
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	switch message[33] {
	case KK1_INDICATOR:
		return process_kk1s(xx_state, message, static_keys);

	case KK2_INDICATOR:
		return process_kk2s(message, static_keys);

	case KKTRANSPORT_INDICATOR:
		return process_transport(
			xx_state,
			message,
			static_keys);
	}
	return BAD_C2C_INDICATOR;
}

int from_server_help(
	struct noise_xx_state* xx_state,
	uint8_t* message,
	hydro_kx_keypair* static_keys) {

	int err = from_server(xx_state, message);
	if (err) {
		return err;
	}

	switch message[0] {
	case NO_MESSAGES_INDICATOR:
		printf("no messages");
		return 0;
	case MESSAGE_INDICATOR:
		return process_c2c(xx_state, message, static_keys);
	}

	return BAD_FROM_SERVER_INDICATOR;
}

enum from_server_type {
	NO_MESSAGES;
	MESSAGE;
}

enum from_client_type {
	KK1s;
	KK2s;
	KKTransport;
}

struct kk_packet {
	uint8_t* packet;
	uint8_t* session_id;
}

union from_client_value {
	struct kk_packet* kk1;
	struct kk_packet* kk2;
	struct kk_transport transport;
}

struct from_client {
	enum from_client_type type;
	union from_client_value value; 
}

const int MAX_FROM_SERVER_SIZE = 7201;

int parse_hex_userid(
	char* raw,
	uint8_t userid[hydro_kx_PUBLICKEYBYTES]) {

	if (strlen(raw) != hex_size) {
		return BAD_RECIPIENT_SIZE;
	}

	int n = hydro_hex2bin(
		userid,
		hydro_kx_PUBLICKEYBYTES,
		raw,
		2 * hydro_kx_PUBLICKEYBYTES,
		NULL,
		NULL);
	if (n != hydro_kx_PUBLICKEYBYTES) {
		return BAD_HEX_PARSE;
	}
	return 0;
}

int make_transport(
	uint8_t message[MAX_MESSAGE_SIZE],
	uint8_t transport[TRANSPORT_SIZE]) {

	uint8_t shared_secret[hydro_secretbox_KEYBYTES];
	uint8_t session_id[SESSION_ID_SIZE];
	err = get_shared_secret(shared_secret);
	if (err) {
		return err;
	}

	uint8_t ciphertext[CIPHERTEXT_SIZE];
	err = hydro_secretbox_encrypt(
		ciphertext,
		message,
		MAX_MESSAGE_SIZE,
		0,
		ENCRYPT_CONTEXT,
		shared_secret);
	if (err) {
		return err;
	}

	transport[0] = KKTRANSPORT_INDICATOR;
	memcpy(transport + 1, session_id, SESSION_ID_SIZE);
	memcpy(transport + 1 + SESSION_ID_SIZE, ciphertext);

	return 0;
}

int top_up_kks(
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES],
	struct noise_xx_state* xx_state,
	hydro_kx_keypair* static_keys) {

	uint8_t* message = malloc(KKS_SIZE);
	int err = top_up_kks_help(
		recipient,
		xx_state,
		static_keys,
		message);
	free(message);
	return err;
}

int get_num_keys(
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES],
	int* num_keys) {

}

int get_num_sending_states_help(
	int* n,
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES],
	sqlite3* db,
	sqlite3_stmt* stmt) {

	int err = sqlite3_prepare_v2(
		db,
		GET_NUM_SENDING_STATES,
		GET_NUM_SENDING_SIZE,
		&stmt,
		NULL);
	if (err != SQLITE_OK) {
		return BAD_DATABASE_PREPARE;
	}

	err = sqlite3_bind_blob(
		stmt,
		1,
		recipient,
		hydro_kx_PUBLICKEYBYTES,
		SQLITE_STATIC);
	if (err != SQLITE_OK) {
		return BAD_DB_BIND;
	}

	if (sqlite3_step(stmt) != SQLITE_DONE) {
		return BAD_DB_STEP;
	}

	*n = sqlite3_column_int(stmt, 0);

	return 0;
}

int get_num_sending_states(
	int* n,
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES]) {

	sqlite3* db;
	if (sqlite3_open(DB_PATH, &db) != SQLITE_OK) {
		return BAD_DB_OPEN;
	}

	sqlite3_stmt* stmt;
	int err = get_num_sending_states_help(n, recipient, db, stmt);
	if (sqlite3_finalize(stmt) != SQLITE_OK) {
		return BAD_DB_FINALIZE;
	}
	if (sqlite3_close(db) != SQLITE_OK) {
		return BAD_DB_CLOSE;
	}
	return err;
}

int get_num_sending_keys(
	int* num_sending_keys,
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES]) {

}

int check_key_levels(
	int* running_low,
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES]) {

	int num_sending_states;
	int err =
		get_num_sending_states(&num_sending_states, recipient);
	if (err) {
		return err;
	}

	int num_sending_keys;
	err = get_num_sending_keys(&num_sending_keys, recipient);
	if (err) {
		return err;
	}

	*running_low =
		(num_sending_states + num_sending_keys) > NUM_KKs;

	return 0;
}

int try_to_send_to(
	uint8_t message[MAX_MESSAGE_SIZE],
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES],
	hydro_kx_keypair* static_keys) {

	uint8_t transport[TRANSPORT_SIZE];
	int err = make_transport(message, transport);
	if (err) {
		return err;
	}

	struct noise_xx_state xx_state;
	err = init_xx_session(&xx_state);
	if (err) {
		return err;
	}

	err = to_server(&xx_state, transport);
	if (err) {
		return err;
	}

	int keys_running_low;
	err = check_key_levels(&keys_running_low, recipient);
	if (err) {
		return err;
	}
	if (keys_running_low) {
		return top_up_kks(recipient, &xx_state, static_keys);
	}

	return 0;
}

int read_message_from_stdin(uint8_t message[PLAINTEXT_SIZE]) {
	int i;
	for (i = 0; i <= MESSAGE_SIZE; i++) {
		int ch = getchar();
		if (ch == EOF) {
			break; 
		}
		if (ch < 32 || ch > 126) {
			return BAD_CHAR;
		}
		message[i + 1] = ch;
	}
	if (feof(stdin)) {
		return BAD_STDIN_READ;
	}
	message[0] = i;
	return 0;
}

int bwt_send(char** argv) {
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES];
	int err = parse_hex_userid(argv[2], recipient);
	if (err) {
		return err;
	}

	uint8_t message[PLAINTEXT_SIZE];
	err = read_message_from_stdin(message);
	if (err) {
		return err;
	}

	hydro_kx_keypair static_keys;
	err = get_static_keys(&static_keys);
	if (err) {
		return err;
	}

	return try_to_send_to(message, recipient, &static_keys);
}


// hydro_kx_PUBLICKEYBYTES + hydro_kx_SECRETKEYBYTES +
// hydro_hash_state.state + hydro_hash_state.buf_off +
// hydro_hash_state.align
const int ENCODED_KX_STATE_SIZE =
	hydro_kx_PUBLICKEYBYTES +
	hydro_kx_SECRETKEYBYTES +
	12 +
	1 +
	3;

void encode_kx_state(
	hydro_kx_state* state,
	uint8_t encoded[ENCODED_KX_STATE_SIZE]) {

	memcpy(encoded, state->eph_kp.pk, hydro_kx_PUBLICKEYBYTES);
	int offset = hydro_kx_PUBLICKEYBYTES;
	memcpy(
		encoded + offset,
		state->eph_kp.sk,
		hydro_kx_SECRETKEYBYTES);
	offset += hydro_kx_SECRETKEYBYTES;
	memcpy(encoded + offset, state->h_st.state, 12);
	offset += 12;
	encoded[offset] = state->h_st.buf_off;
	offset += 1;
	memcpy(encoded + offset, state->h_st.align, 3);
}

int save_kk1_help(
	uint8_t session_id[SESSION_ID_SIZE],
	uint8_t their_id[hydro_kx_PUBLICKEYBYTES],
	hydro_kx_state* state,
	sqlite3* db,
	sqlite3_stmt* stmt) {

	uint8_t encoded[ENCODED_KX_STATE_SIZE];
	encode_kx_state(state, encoded);

	int err = sqlite3_prepare_v2(
		db,
		SAVE_KK1,
		SAVE_KK1_SIZE,
		&stmt,
		NULL);
	if (err != SQLITE_OK) {
		return BAD_DATABASE_PREPARE_KK1;
	}

	err = sqlite3_bind_blob(
		stmt,
		1,
		session_id,
		SESSION_ID_SIZE,
		SQLITE_STATIC);
	if (err != SQLITE_OK) {
		return BAD_DB_BIND;
	}

	err = sqlite3_bind_blob(
		stmt,
		2,
		their_id,
		hydro_kx_PUBLICKEYBYTES,
		SQLITE_STATIC);
	if (err != SQLITE_OK) {
		return BAD_DB_BIND;
	}

	err = sqlite3_bind_blob(
		stmt,
		3,
		encoded,
		ENCODED_KX_STATE_SIZE,
		SQLITE_STATIC);
	if (err != SQLITE_OK) {
		return BAD_DB_BIND;
	}

	if (sqlite3_step(stmt) != SQLITE_DONE) {
		return BAD_DB_STEP;
	}

	return 0;
}

int save_kk1(
	hydro_kx_state* state,
	uint8_t their_id[hydro_kx_PUBLICKEYBYTES],
	uint8_t kk1[hydro_kx_KK_PACKET1BYTES],
	uint8_t session_id[SESSION_ID_SIZE]) {

	sqlite3* db;
	if (sqlite3_open(DB_PATH, &db) != SQLITE_OK) {
		return BAD_DB_OPEN;
	}

	sqlite3_stmt* stmt;
	int err = save_kk1_help(session_id, their_id, state, db, stmt);
	if (sqlite3_finalize(stmt) != SQLITE_OK) {
		return BAD_DB_FINALIZE;
	}
	if (sqlite3_close(db) != SQLITE_OK) {
		return BAD_DB_CLOSE;
	}
	return err;
}

const uint8_t SERVER_PK[hydro_kx_PUBLICKEYBYTES] =
	{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32};

int make_kk1(
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES],
	hydro_kx_keypair* static_keys,
	int i,
	uint8_t* message) {

	uint8_t kk1[hydro_kx_KK_PACKET1BYTES];
	hydro_kx_state state;

	int err = hydro_kx_kk_1(&state, kk1, SERVER_PK, static_keys);
	if (err) {
		return err;
	}

	uint8_t session_id[SESSION_ID_SIZE];
	hydro_random_buf(session_id, SESSION_ID_SIZE);

	uint8_t* kk_start =
		message +
		1 +
		hydro_kx_PUBLICKEYBYTES +
		(i * KK_SIZE);
	memcpy(kk_start, kk1, hydro_kx_KK_PACKET1BYTES);

	uint8_t* id_start = kk_start + SESSION_ID_SIZE;
	memcpy(id_start, session_id, SESSION_ID_SIZE);

	return save_kk1(&state, recipient, kk1, session_id);
}

int top_up_kks_help(
	uint8_t recipient[hydro_kx_PUBLICKEYBYTES],
	struct noise_xx_state* xx_state,
	hydro_kx_keypair* static_keys,
	uint8_t* message) {

	message[0] = 1;
	memcpy(message + 1, recipient, hydro_kx_PUBLICKEYBYTES);

	for (int i = 0; i < NUM_KKs; i++) {
		int err = make_kk1(recipient, static_keys, i, message);
		if (err) {
			return err;
		}
	}
	return to_server(xx_state, message);
}

int save_new_contact_help(
	uint8_t user_id[hydro_kx_PUBLICKEYBYTES],
	sqlite3* db,
	sqlite3_stmt* stmt) {

	int err = sqlite3_prepare_v2(
		db,
		SAVE_CONTACT,
		SAVE_CONTACT_SIZE,
		&stmt,
		NULL);
	if (err != SQLITE_OK) {
		return BAD_DB_PREPARE_CONTACT;
	}

	err = sqlite3_bind_blob(
		stmt,
		1,
		user_id,
		hydro_kx_PUBLICKEYBYTES,
		SQLITE_STATIC);
	if (err != SQLITE_OK) {
		return BAD_DB_BIND;
	}

	if (sqlite3_step(stmt) != SQLITE_DONE) {
		return BAD_DB_STEP_CONTACT;
	}

	return 0;
}

int save_new_contact(uint8_t user_id[hydro_kx_PUBLICKEYBYTES]) {
	sqlite3* db;
	if (sqlite3_open(DB_PATH, &db) != SQLITE_OK) {
		return BAD_DB_OPEN;
	}

	sqlite3_stmt* stmt;
	int err = save_new_contact_help(user_id, db, stmt);
	if (sqlite3_finalize(stmt) != SQLITE_OK) {
		return BAD_DB_FINALIZE;
	}
	if (sqlite3_close(db) != SQLITE_OK) {
		return BAD_DB_CLOSE;
	}
	return err;
}

int bwt_addcontact(char** argv) {
	uint8_t new_contact[hydro_kx_PUBLICKEYBYTES];
	int err = parse_hex_userid(argv[2], new_contact);
	if (err) {
		return err;
	}

	err = save_new_contact(new_contact);
	if (err) {
		return err;
	}

	hydro_kx_keypair static_keys;
	err = get_static_keys(&static_keys);
	if (err) {
		return err;
	}

	struct noise_xx_state xx_state;
	err = init_xx_session(&xx_state, &static_keys);
	if (err) {
		return err;
	}

	return top_up_kks(new_contact, &xx_state, &static_keys);
}

int two_args(char** argv) {
	if (strcmp(argv[1], "send") == 0) {
		return bwt_send(argv);
	}
	if (strcmp(argv[1], "addcontact") == 0) {
		return bwt_addcontact(argv);
	}
	return BAD_ARGS;
}

int setup_db_help(sqlite3* db, sqlite3_stmt* stmt) {
	int err = sqlite3_prepare_v2(
		db, CONTACT_TABLE, CONTACT_TABLE_SIZE, &stmt, NULL);
	if (err != SQLITE_OK) {
		return BAD_MAKE_CONTACT_TABLE;
	}

	err = sqlite3_prepare_v2(
		db, RX_KEY_TABLE, RX_KEY_TABLE_SIZE, &stmt, NULL);
	if (err != SQLITE_OK) {
		return BAD_MAKE_RX_TABLE;
	}

	err = sqlite3_prepare_v2(
		db, TX_STATE_TABLE, TX_STATE_TABLE_SIZE, &stmt, NULL);
	if (err != SQLITE_OK) {
		return BAD_MAKE_TX_STATE_TABLE;
	}

	err = sqlite3_prepare_v2(
		db, TX_KEY_TABLE, TX_KEY_TABLE_SIZE, &stmt, NULL);
	if (err != SQLITE_OK) {
		return BAD_MAKE_TX_KEY_TABLE;
	}

	return 0;
}

int setup_db() {
	sqlite3* db;
	if (sqlite3_open(DB_PATH, &db) != SQLITE_OK) {
		return BAD_DB_OPEN;
	}

	sqlite3_stmt* stmt;
	int err = setup_db_help(db, stmt);
	if (sqlite3_finalize(stmt) != SQLITE_OK) {
		return BAD_DB_FINALIZE;
	}
	if (sqlite3_close(db) != SQLITE_OK) {
		return BAD_DB_CLOSE;
	}
	return err;
}

int main_err(int argc, char** argv) {
	if (hydro_init() != 0) {
		return BAD_HYDRO_INIT;
	}

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
