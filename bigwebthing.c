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
// - 1 byte: 0
// - 8 bytes: their user ID
//
// total = 36 + 1 + 8 = 45
#define ENCRYPTED_PUBLIC_KEY_REQUEST 45
// 1 + 8 = 9
#define PUBLIC_KEY_REQUEST 9

// 36 + 1 + 32 = 68
#define ENCRYPTED_PUBLIC_KEY 68
#define PLAIN_PUBLIC_KEY 33

struct new_friend {
	char* nickname;
	uint8_t fingerprint[FINGERPRINT_LEN];
	uint64_t user_id;
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
};

char* pretty_result(enum result error) {
	return "something went wrong";
}

int parse_userid(uint64_t* user_id, char* raw) {
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
	char* raw) {

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

int good_nick_char(char c) {
	return ((c > 47) && (c < 58)) || ((c > 96) && (c < 123));
}

int check_nickname(char* nickname) {
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
	char *argv[]) {

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

int get_server_sock(int* sock) {
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
	uint8_t raw[RAW_CERTIFICATE_BYTES],
	uint8_t untrusted[hydro_kx_PUBLICKEYBYTES]) {

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

	uint8_t expiry;
	for (int i = 0; i < 8; i++) {
		expiry |= ((uint8_t)raw[i]) << i;
	}

	uint8_t now = time(NULL);

	if (now > expiry) {
		return EXPIRED_CERTIFICATE;
	}

	return OK;
}

int send_packet_1(int sock, hydro_kx_state* kx_state) {
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
	int sock,
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
	int sock,
	hydro_kx_session_keypair* session_keys,
	uint8_t server_public_key[hydro_kx_PUBLICKEYBYTES]) {
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
	int result = hydro_secretbox_decrypt(
		raw_certificate,
		encrypted_certificate,
		ENCRYPTED_CERTIFICATE_BYTES,
		0,
		"cert_msg",
		session_keys->rx);
	if (result) {
		return BAD_CERTIFICATE_DECRYPT;
	}

	return check_certificate(raw_certificate, server_public_key);
}

int authenticate(int sock, hydro_kx_keypair* session_keys) {
	hydro_kx_state kx_state;
	int result = send_packet_1(sock, &kx_state);
	if (result) {
		return result;
	}

	uint8_t server_public_key[hydro_kx_PUBLICKEYBYTES];
	result = send_packet_3(
		sock,
		&kx_state,
		&session_keys,
		server_public_key);
	if (result) {
		return result;
	}

	return get_certificate(
		sock,
		&session_keys,
		server_public_key);
}

int make_public_key_request(
	uint8_t request[ENCRYPTED_PUBLIC_KEY_REQUEST],
	uint64_t their_id,
	uint8_t key[hydro_kx_SESSION_KEY_BYTES]) {

	uint8_t plain[PUBLIC_KEY_REQUEST];
	plain[0] = 0;
	for (int i = 0; i < 8; ++i) {
		plain[i+1] = (uint8_t)((their_id >> i) & 0xFF)
	}

	int result = hydro_secretbox_encrypt(
		request,
		plain,
		PUBLIC_KEY_REQUEST,
		1,
		"get_key_",
		key);
	if (result) {
		return BAD_ENCRYPT_GET_KEY;
	}

	return OK;
}

int request_public_key(
	const int sock,
	const uint64_t user_id,
	const uint8_t key[hydro_kx_SESSION_KEY_BYTES]) {

	uint8_t request[ENCRYPTED_PUBLIC_KEY_REQUEST];
	result = make_public_key_request(
		request,
		cmd.user_id,
		session_keys.tx);
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
	
	uint8_t plain[PLAIN_PUBLIC_KEY];
	int result = hydro_secretbox_decrypt(
		plain,
		encrypted,
		ENCRYPTED_PUBLIC_KEY,
		

}

int new_friend(int argc, char *argv[]) {
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
}

int main_error(int argc, char *argv[]) {
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

int main(int argc, char *argv[]) {
	puts(pretty_result(main_error(argc, argv)));
}
