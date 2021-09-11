#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "hydrogen.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <unistd.h>
#include <time.h>

#define MAX_NICKNAME 20
#define FINGERPRINT_LEN 10
#define SECRET_FILE "secret"
#define FRIEND_FILE "friend"


struct route_char {
	char ch;
	uint_fast8_t route_id;
}

const struct route_char route_chars[] = {
	{'n', 0},
	{'e', 0},
	{'w', 0},
	{'f', 0},
	{'r', 0},
	{'i', 0},
	{'e', 0},
	{'n', 0},
	{'d', 0},
};

struct arg_char {
	uint_fast8_t char_set_id;
	uint_fast8_t route_id;
	uint_fast8_t arg_pos;
};

enum char_set {
	DECIMAL,
	NO_ZERO,
	ALPHA_NUM,
}

enum route_id {
	NEW_FRIEND,
}

const struct arg_char optional_arg_chars[] = {
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 0},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
	{ALPHA_NUM, NEW_FRIEND, 2},
};

const struct arg_char[] required_arg_chars = {
	{NO_ZERO, NEW_FRIEND, 0},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{DECIMAL, NEW_FRIEND, 1},
	{ALPHA_NUM, NEW_FRIEND, 2},
};

struct char_set {
	uint_fast8_t id;
	char ch;
};

const struct char_set[] char_set = {
	{DECIMAL, '0'},
	{DECIMAL, '1'},
	{DECIMAL, '2'},
	{DECIMAL, '3'},
	{DECIMAL, '4'},
	{DECIMAL, '5'},
	{DECIMAL, '6'},
	{DECIMAL, '7'},
	{DECIMAL, '8'},
	{DECIMAL, '9'},
	{NO_ZERO, '1'},
	{NO_ZERO, '2'},
	{NO_ZERO, '3'},
	{NO_ZERO, '4'},
	{NO_ZERO, '5'},
	{NO_ZERO, '6'},
	{NO_ZERO, '7'},
	{NO_ZERO, '8'},
	{NO_ZERO, '9'},
	{ALPHA_NUM, 'a'},
	{ALPHA_NUM, 'b'},
	{ALPHA_NUM, 'c'},
	{ALPHA_NUM, 'd'},
	{ALPHA_NUM, 'e'},
	{ALPHA_NUM, 'f'},
	{ALPHA_NUM, 'g'},
	{ALPHA_NUM, 'h'},
	{ALPHA_NUM, 'i'},
	{ALPHA_NUM, 'j'},
	{ALPHA_NUM, 'k'},
	{ALPHA_NUM, 'l'},
	{ALPHA_NUM, 'm'},
	{ALPHA_NUM, 'n'},
	{ALPHA_NUM, 'o'},
	{ALPHA_NUM, 'p'},
	{ALPHA_NUM, 'q'},
	{ALPHA_NUM, 'r'},
	{ALPHA_NUM, 's'},
	{ALPHA_NUM, 't'},
	{ALPHA_NUM, 'u'},
	{ALPHA_NUM, 'v'},
	{ALPHA_NUM, 'w'},
	{ALPHA_NUM, 'x'},
	{ALPHA_NUM, 'y'},
	{ALPHA_NUM, 'z'},
	{ALPHA_NUM, '0'},
	{ALPHA_NUM, '1'},
	{ALPHA_NUM, '2'},
	{ALPHA_NUM, '3'},
	{ALPHA_NUM, '4'},
	{ALPHA_NUM, '5'},
	{ALPHA_NUM, '6'},
	{ALPHA_NUM, '7'},
	{ALPHA_NUM, '8'},
	{ALPHA_NUM, '9'},
};

// - hydro_secretbox_HEADERBYTES (36) bytes: auth tag
// - 8 bytes: Little-Endian Posix seconds of expiry date
// - hydro_sign_BYTES (64) bytes: signature of expiry and public
//	Noise key, signed by long-term hard-coded server public key
//
// total = 36 + 8 + 64 = 108
#define ENCRYPTED_CERTIFICATE_BYTES 108
// This is the decrypted certificate: 8 + 64 = 72
#define RAW_CERTIFICATE_BYTES 72
// This is the expiry plus the Noise public key = 8 + 32 = 40
#define SERVER_SIGNED_BYTES 40

// - hydro_secrebox_HEADERBYTES (36) bytes: auth tag
// - 8 bytes: their user ID
//
// total = 36 + 8 = 44
#define ENCRYPTED_PUBLIC_KEY_REQUEST 44

// Indicator length = 1
// User ID length = 8
#define PUBLIC_KEY_REQUEST 9

// 36 + 32 = 68
#define ENCRYPTED_PUBLIC_KEY 68

#define KEY_AND_USER_ID_LEN 40

struct new_friend {
	char* nickname;
	uint8_t fingerprint[FINGERPRINT_LEN];
	uint64_t user_id;
};

enum msg_id_server {
	SERVER_CERTIFICATE_ID,
	THEIR_PUBLIC_KEY_ID,
	REQUEST_THEIR_PUBLIC_KEY_ID,
};

enum result {
	OK,
	NO_ARGS,
	BAD_FIRST_ARG,
	NEW_FRIEND_NOT_FIVE_ARGS,
	ODD_LENGTH_USERID,
	USERID_TOO_LONG,
	BAD_USER_ID_HEX,
	BAD_FINGERPRINT_LENGTH,
	BAD_FINGERPRINT_HEX,
	BAD_LIBHYDROGEN_INIT,
	EMPTY_NICKNAME,
	NICKNAME_TOO_LONG,
	BAD_NICKNAME_CHAR,
	BAD_CLOSE_NEW_SECRET_FILE,
	BAD_WRITE_SECRET_KEY,
	BAD_WRITE_PUBLIC_KEY,
	BAD_MAKE_KX_KEY,
	BAD_MAKE_SECRET_FILE,
	BAD_CLOSE_OLD_SECRET_FILE,
	BAD_READ_SECRET_KEY,
	BAD_READ_PUBLIC_KEY,
	BAD_SOCKET,
	BAD_SOCKET_CONNECT,
	BAD_GET_ADDR_INFO,
	BAD_CERTIFICATE_SIGNATURE,
	EXPIRED_CERTIFICATE,
	BAD_CERTIFICATE_DECRYPT,
	BAD_RECV_CERTIFICATE,
	BAD_SEND_XX3,
	BAD_HYDRO_KX_XX3,
	BAD_RECV_XX2,
	BAD_SEND_XX1,
	BAD_HYDRO_KX_XX1,
	BAD_RECEIVE_PUBLIC_KEY,
	BAD_SEND_KEY_REQUEST,
	BAD_ENCRYPT_GET_KEY,
	BAD_FINGERPRINT,
	BAD_WRITE_FRIENDS_FILE,
	BAD_MAKE_FRIEND_FILE,
};

int encrypt_server(
	uint8_t *c,
	const void *m_,
	size_t mlen,
	uint64_t msg_id,
	const uint8_t key[hydro_secretbox_KEYBYTES]) {

	return hydro_secretbox_encrypt(
		c,
		m_,
		mlen,
		msg_id,
		"bwttoser",
		key);
}

int decrypt_server(
	void *m_,
	const uint8_t *c,
	size_t clen,
	uint64_t msg_id,
	const uint8_t key[hydro_secretbox_KEYBYTES]) {

	return hydro_secretbox_decrypt(
		m_,
		c,
		clen,
		msg_id,
		"bwtfroms",
		key);
}

char* pretty_result(const enum result error) {
	return "something went wrong";
}

int parse_userid(uint64_t* user_id, const char* raw) {
	size_t len = strlen(raw);
	if (len > 8) {
		return USERID_TOO_LONG;
	}
	if (len % 2 == 1) {
		return ODD_LENGTH_USERID;
	}

	uint8_t buf[8];

	if (hydro_hex2bin(buf, 8, raw, len, NULL, NULL)) {
		return BAD_USER_ID_HEX;
	}

	for (int i = 0; i < 8; ++i) {
		*user_id |= (((uint64_t)buf[i]) << i);
	}

	return 0;
}

int parse_fingerprint(
	uint8_t fingerprint[FINGERPRINT_LEN],
	const char* const raw) {

	size_t len = strlen(raw);
	if (len != FINGERPRINT_LEN) {
		return BAD_FINGERPRINT_LENGTH;
	}

	if (hydro_hex2bin(
		fingerprint,
		FINGERPRINT_LEN,
		raw,
		FINGERPRINT_LEN,
		NULL,
		NULL)) {

		return BAD_FINGERPRINT_HEX;
	}

	return 0;
}

int good_nick_char(const char c) {
	return ((c > 47) && (c < 58)) || ((c > 96) && (c < 123));
}

int check_nickname(const char* const nickname) {
	int len = strlen(nickname);
	if (len == 0) {
		return EMPTY_NICKNAME;
	}

	if (len > MAX_NICKNAME) {
		return NICKNAME_TOO_LONG;
	}

	for (int i = 0; i < len; ++i) {
		if (!good_nick_char(nickname[i])) {
			return BAD_NICKNAME_CHAR;
		}
	}

	return 0;
}

int parse_new_friend(
	struct new_friend* args,
	int argc,
	char* argv[]) {

	// To add a new friend, the command is like:
	//
	// addfriend b447 0421838936522a1b3202 jake
	//           ^~~~                      ^~~~
	//        user ID                      a nickname
	//                ^~~~~~~~~~~~~~~~~~~~
	//            20 character user fingerprint

	if (argc != 5) {
		return NEW_FRIEND_NOT_FIVE_ARGS;
	}

	int error = parse_userid(&(args->user_id), argv[2]);
	if (error) {
		return error;
	}

	error = parse_fingerprint(args->fingerprint, argv[3]);
	if (error) {
		return error;
	}

	error = check_nickname(argv[4]);
	if (error) {
		return error;
	}

	args->nickname = argv[4];

	return 0;
}

int make_new_key(hydro_kx_keypair* my_static) {
	FILE* f = fopen(SECRET_FILE, "wb");
	if (!f) {
		return BAD_MAKE_SECRET_FILE;
	}

	hydro_kx_keygen(my_static);

	size_t n = fwrite(
		my_static->pk,
		1,
		hydro_kx_PUBLICKEYBYTES,
		f);
	if (n != hydro_kx_PUBLICKEYBYTES) {
		return BAD_WRITE_PUBLIC_KEY;
	}
	
	n = fwrite(
		my_static->sk,
		1,
		hydro_kx_SECRETKEYBYTES,
		f);
	if (n != hydro_kx_SECRETKEYBYTES) {
		return BAD_WRITE_SECRET_KEY;
	}

	if (fclose(f)) {
		return BAD_CLOSE_NEW_SECRET_FILE;
	}

	return OK;
}

int get_my_static(hydro_kx_keypair* my_static) {
	FILE* f = fopen(SECRET_FILE, "rb");
	if (!f) {
		return make_new_key(my_static);
	}

	size_t n = fread(my_static->pk, 1, hydro_kx_PUBLICKEYBYTES, f);
	if (n != hydro_kx_PUBLICKEYBYTES) {
		return BAD_READ_PUBLIC_KEY;
	}

	n = fread(my_static->sk, 1, hydro_kx_SECRETKEYBYTES, f);
	if (n != hydro_kx_SECRETKEYBYTES) {
		return BAD_READ_SECRET_KEY;
	}

	if (fclose(f)) {
		return BAD_CLOSE_OLD_SECRET_FILE;
	}

	return OK;
}

int get_server_sock(int* const sock) {
	struct addrinfo hints, *server_info;
	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;

	int result = getaddrinfo(
		"127.0.0.1",
		"8080",
		&hints,
		&server_info);
	if (result) {
		return BAD_GET_ADDR_INFO;
	}

	*sock = socket(
		server_info->ai_family,
		server_info->ai_socktype,
		server_info->ai_protocol);
	if (*sock == -1) {
		return BAD_SOCKET;
	}

	result = connect(
		*sock,
		server_info->ai_addr,
		server_info->ai_addrlen);
	if (result == -1) {
		close(*sock);
		return BAD_SOCKET_CONNECT;
	}

	return OK;
}

const uint8_t SERVER_PUBLIC_SIGN[32] = {129, 184, 154, 153, 228, 28, 158, 110, 212, 224, 32, 12, 50, 143, 83, 109, 194, 224, 57, 30, 138, 160, 223, 189, 199, 196, 81, 146, 22, 159, 86, 44};


int check_certificate(
	const uint8_t raw[RAW_CERTIFICATE_BYTES],
	const uint8_t untrusted[hydro_kx_PUBLICKEYBYTES]) {

	uint8_t sign_msg[SERVER_SIGNED_BYTES];
	memcpy(sign_msg, raw, 8); // the expiry date
	memcpy(sign_msg + 8, untrusted, 32); // the static Noise key

	int result = hydro_sign_verify(
		raw + 8,
		sign_msg,
		SERVER_SIGNED_BYTES,
		"cert_sig",
 		SERVER_PUBLIC_SIGN); 
	if (result) {
		return BAD_CERTIFICATE_SIGNATURE;
	}

	uint8_t expiry = 0;
	for (int i = 0; i < 8; i++) {
		expiry |= ((uint8_t)raw[i]) << i;
	}

	uint8_t now = time(NULL);

	if (now > expiry) {
		return EXPIRED_CERTIFICATE;
	}

	return OK;
}

int send_packet_1(const int sock, hydro_kx_state* kx_state) {
	uint8_t packet1[hydro_kx_XX_PACKET1BYTES];
	int result = hydro_kx_xx_1(kx_state, packet1, NULL);
	if (result) {
		return BAD_HYDRO_KX_XX1;
	}

	int n = send(sock, packet1, hydro_kx_XX_PACKET1BYTES, 0);
	if (n != hydro_kx_XX_PACKET1BYTES) {
		return BAD_SEND_XX1;
	}

	return OK;
}

int send_packet_3(
	const int sock,
	hydro_kx_state* kx_state,
	hydro_kx_session_keypair* session_keys,
	uint8_t server_public_key[hydro_kx_PUBLICKEYBYTES]) {

	uint8_t packet2[hydro_kx_XX_PACKET2BYTES];
	int n = recv(sock, packet2, hydro_kx_XX_PACKET2BYTES, 0);
	if (n != hydro_kx_XX_PACKET2BYTES) {
		return BAD_RECV_XX2;
	}

	hydro_kx_keypair my_static;
	int result = get_my_static(&my_static);
	if (result) {
		return result;
	}

	uint8_t packet3[hydro_kx_XX_PACKET3BYTES];
	result = hydro_kx_xx_3(
		kx_state,
		session_keys,
		packet3,
		server_public_key,
		packet2,
		NULL,
		&my_static);
	if (result) {
		return BAD_HYDRO_KX_XX3;
	}

	n = send(sock, packet3, hydro_kx_XX_PACKET3BYTES, 0);
	if (n != hydro_kx_XX_PACKET3BYTES) {
		return BAD_SEND_XX3;
	}
	
	return OK;
}

int get_certificate(
	const int sock,
	const hydro_kx_session_keypair* const session_keys,
	const uint8_t server_public_key[hydro_kx_PUBLICKEYBYTES]) {

	uint8_t encrypted_certificate[ENCRYPTED_CERTIFICATE_BYTES];
	int n = recv(
		sock,
		encrypted_certificate,
		ENCRYPTED_CERTIFICATE_BYTES,
		0);
	if (n != ENCRYPTED_CERTIFICATE_BYTES) {
		return BAD_RECV_CERTIFICATE;
	}

	uint8_t raw_certificate[RAW_CERTIFICATE_BYTES];
	int result = decrypt_server(
		raw_certificate,
		encrypted_certificate,
		ENCRYPTED_CERTIFICATE_BYTES,
		SERVER_CERTIFICATE_ID,
		session_keys->rx);
	if (result) {
		return BAD_CERTIFICATE_DECRYPT;
	}

	return check_certificate(raw_certificate, server_public_key);
}

int authenticate(
	const int sock,
	hydro_kx_session_keypair* session_keys) {

	hydro_kx_state kx_state;
	int result = send_packet_1(sock, &kx_state);
	if (result) {
		return result;
	}

	uint8_t server_public_key[hydro_kx_PUBLICKEYBYTES];
	result = send_packet_3(
		sock,
		&kx_state,
		session_keys,
		server_public_key);
	if (result) {
		return result;
	}

	return get_certificate(
		sock,
		session_keys,
		server_public_key);
}

int make_public_key_request(
	uint8_t request[ENCRYPTED_PUBLIC_KEY_REQUEST],
	const uint64_t their_id,
	const uint8_t key[hydro_kx_SESSIONKEYBYTES]) {

	uint8_t plain[PUBLIC_KEY_REQUEST];
	plain[0] = 0;
	for (int i = 0; i < 8; ++i) {
		plain[i+1] = (uint8_t)((their_id >> i) & 0xFF);
	}

	int result = encrypt_server(
		request,
		plain,
		PUBLIC_KEY_REQUEST,
		REQUEST_THEIR_PUBLIC_KEY_ID,
		key);
	if (result) {
		return BAD_ENCRYPT_GET_KEY;
	}

	return OK;
}

int request_public_key(
	const int sock,
	const uint64_t user_id,
	const uint8_t key[hydro_kx_SESSIONKEYBYTES]) {

	uint8_t request[ENCRYPTED_PUBLIC_KEY_REQUEST];
	int result = make_public_key_request(
		request,
		user_id,
		key);
	if (result) {
		return result;
	}

	int n = send(sock, request, ENCRYPTED_PUBLIC_KEY_REQUEST, 0);
	if (n != ENCRYPTED_PUBLIC_KEY_REQUEST) {
		return BAD_SEND_KEY_REQUEST;
	}

	return OK;
}

int fetch_public_key(
	const int sock,
	uint8_t public_key[hydro_kx_PUBLICKEYBYTES],
	const uint8_t key[hydro_kx_SESSIONKEYBYTES]) {

	uint8_t encrypted[ENCRYPTED_PUBLIC_KEY];
	int n = recv(sock, encrypted, ENCRYPTED_PUBLIC_KEY, 0);
	if (n != ENCRYPTED_PUBLIC_KEY) {
		return BAD_RECEIVE_PUBLIC_KEY;
	}
	
	uint8_t plain[hydro_kx_PUBLICKEYBYTES];
	int result = decrypt_server(
		plain,
		encrypted,
		ENCRYPTED_PUBLIC_KEY,
		THEIR_PUBLIC_KEY_ID,
		key);
	if (result) {
		return result;
	}

	return OK;
}

const uint8_t finger_key[hydro_pwhash_MASTERKEYBYTES] = {94, 54, 200, 248, 175, 85, 145, 186, 246, 162, 9, 109, 240, 136, 137, 204, 82, 43, 223, 239, 253, 247, 67, 163, 193, 23, 149, 240, 249, 183, 241, 10};	

int make_fingerprint(
	uint8_t fingerprint[FINGERPRINT_LEN],
	const uint8_t public_key[hydro_kx_PUBLICKEYBYTES],
	const uint64_t user_id) {

	char input [KEY_AND_USER_ID_LEN];
	memcpy(input, public_key, hydro_kx_PUBLICKEYBYTES);
	for (int i = 0; i < 8; i++) {
		input[hydro_kx_PUBLICKEYBYTES + i] =
			(user_id >> i) & 0xFF;
	}

	return hydro_pwhash_deterministic(
		fingerprint,
		FINGERPRINT_LEN,
		input,
		KEY_AND_USER_ID_LEN,
		"makefing",
		finger_key,
		10000,
		0,
		1);
}

void encode_uint32(uint8_t* buf, uint32_t u) {
	for (int i = 0; i < 4; ++i) {
		buf[i] = (u >> i) & 0xFF;
	}
}

int store_my_first_friend(
	const uint8_t public_key [hydro_kx_PUBLICKEYBYTES],
	const char * nickname) {

	FILE* f = fopen(FRIEND_FILE, "wb");
	if (!f) {
		return BAD_MAKE_FRIEND_FILE;
	}

	size_t nick_len = strlen(nickname);
	uint32_t encoded_len =
		4 + 4 + 1 + nick_len + hydro_kx_PUBLICKEYBYTES;
	uint8_t encoded[encoded_len];
	encode_uint32(encoded, encoded_len);
	encode_uint32(encoded+4, 1);

	encoded[4+4] = nick_len;
	for (int i = 0; i < nick_len; ++i) {
		encoded[4+4+1+i] = nickname[i];
	}
	for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; ++i) {
		encoded[4+4+1+nick_len+i] = public_key[i];
	}

	size_t n = fwrite(encoded, 1, encoded_len, f);
	if (n != encoded_len) {
		return BAD_WRITE_FRIENDS_FILE;
	}

	return OK;
}

uint32_t decode_uint32(uint8_t raw[]) {
	uint32_t u = 0;
	for (int i = 0; i < 4; ++i) {
		u |= raw[i] << i;
	}
	return u;
}

struct friend {
	uint8_t public_key[hydro_kx_PUBLICKEYBYTES];
	char* nickname;
};

int store_new_friend(
	const uint8_t public_key[hydro_kx_PUBLICKEYBYTES],
	const char* nickname) {

	FILE* f = fopen(FRIEND_FILE, "rb");
	if (!f) {
		return store_my_first_friend(public_key, nickname);
	}

	uint8_t raw_size[4];
	size_t n = fread(raw_size, 1, 4, f);
	if (n != 4) {
		return NO_SIZE_IN_FRIEND_FILE;
	}

	size_t buf_size = decode_uint32(raw_size);
	uint8_t* buf = malloc(buf_size);
	n = fread(buf, 1, buf_size, f);
	if (n != buf_size) {
		return COULDNT_READ_FRIENDS_FILE;
	}
	close(f);

	int num_friends = decode_uint32(buf);

	struct friends* parsed = malloc(
		num_friends * sizeof struct friend);
	


	return result;
}

int new_friend(int argc, char* argv[]) {
	struct new_friend cmd;
	int result = parse_new_friend(&cmd, argc, argv);
	if (result) {
		return result;
	}

	int sock;
	result = get_server_sock(&sock);
	if (result) {
		return result;
	}

	hydro_kx_session_keypair session_keys;
	result = authenticate(sock, &session_keys);
	if (result) {
		return result;
	}

	result = request_public_key(
		sock,
		cmd.user_id,
		session_keys.tx);
	if (result) {
		return result;
	}

	uint8_t public_key[hydro_kx_PUBLICKEYBYTES];
	result = fetch_public_key(sock, public_key, session_keys.rx);
	if (result) {
		return result;
	}

	uint8_t fingerprint[FINGERPRINT_LEN];
	result = make_fingerprint(
		fingerprint,
		public_key,
		cmd.user_id);
	if (result) {
		return result;
	}
	for (int i = 0; i < FINGERPRINT_LEN; ++i) {
		if (fingerprint[i] != cmd.fingerprint[i]) {
			return BAD_FINGERPRINT;
		}
	}

	return store_new_friend(public_key, cmd.nickname);
}

int main_error(int argc, char* argv[]) {
	if (hydro_init()) {
		return BAD_LIBHYDROGEN_INIT;
	}

	if (argc < 2) {
		return NO_ARGS;
	}

	if (!strcmp("newfriend", argv[1])) {
		return new_friend(argc, argv);
	}

	return BAD_FIRST_ARG;
}

struct model {
	int argc;
	char* argv[];
}

int step(struct model* model) {
	
}

struct struct_field {
	
}

struct branch {
	uint_fast32_t value_id;
	uint_fast32_t from;
	uint_fast32_t to;
}

const struct branch branches[] = {
	
}


int main(int argc, char *argv[]) {
	
}
