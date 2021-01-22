#include <stdio.h>
#include "hydrogen.h"
#include <string.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <sqlite3.h>

enum error {
    BAD_ARGS = 1,
    ENCRYPT_FAILED,
    BAD_HYDRO_INIT,
    BAD_DECRYPT,
    BAD_MESSAGE_LENGTH,
    BAD_INDICATOR,
    BAD_STDIN_READ,
    BAD_CHAR,
    BAD_HEX_PARSE,
    BAD_RECIPIENT_SIZE,
};

char* show_error(int err) {
    switch (err) {
    }
    return "bad error code";
}

const int PLAINTEXT_SIZE = 100;
const char* ENCRYPT_CONTEXT = "k2LCe3A3";
#define SECRET_SIZE 40
#define KK1_SIZE 48

struct Session {
	uint8_t kk1[KK1_SIZE];
	uint8_t theirid[hydro_kx_PUBLICKEYBYTES];
	uint8_t secret[SECRET_SIZE];
};

struct Secrets {
	hydro_kx_keypair static_keys;
	uint8_t** contacts;
	struct Session* sending;
	struct Session* receiving;
};

int read_help(FILE* fsecret, FILE* fpublic) {
	struct Secrets secrets;
	if (fsecret == NULL) {
		int err = make_secrets(&secrets);
		if (err) {
			return err;
		}
	}
	return 0;
}

const char* public_path = "public";

const char* secret_path = "secret";

int read_() {
	FILE* fsecret = fopen(secret_path, "rb");
	FILE* fpublic = fopen(public_path, "rb");
	int err = read_help(fsecret, fpublic);
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
    "    $ bwt read <message ID>\n"
    "\n"
    "Send a message from STDIN\n"
    "\n"
    "    $ bwt write <recipient ID>\n";

int help() {
	puts(usage);
	return 0;
}

const int hex_size = (hydro_kx_PUBLICKEYBYTES * 2) + 1;

int myid() {
	puts("MYID NOT IMPLEMENTED YET");
	return 0;
}

int one_simple_arg(char** argv) {
    if (strcmp(argv[1], "help") == 0) {
        return help();
    }
    if (strcmp(argv[1], "myid") == 0) {
        return myid();
    }
    if (strcmp(argv[1], "read") == 0) {
        return read_();
    }
    return BAD_ARGS;
}

const int MAX_MESSAGE_SIZE = 16000;

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

int read_message_from_stdin(uint8_t message[PLAINTEXT_SIZE]) {
	int i;
	for (i = 0; i <= PLAINTEXT_SIZE; i++) {
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

int addcontact(char** argv) {
	puts("ADDCONTACT NOT IMPLEMENTED YET");
	return 0;
}

int write_(char** argv) {
	puts("WRITE_ NOT IMPLEMENTED YET");
	return 0;
}

int two_args(char** argv) {
	if (strcmp(argv[1], "write") == 0) {
		return write_(argv);
	}
	if (strcmp(argv[1], "addcontact") == 0) {
		return addcontact(argv);
	}
	return BAD_ARGS;
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
