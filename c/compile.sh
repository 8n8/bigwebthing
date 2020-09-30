gcc test.c crypto_provider_openssl.c -lsignal-protocol-c -lssl -lcrypto -lpthread -lsqlite3 -Wall -std=c11 -Werror -Wpedantic

echo "compilation finished"
