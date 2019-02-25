#include <sodium.h>
#include <stdio.h>

#define MESSAGE (const unsigned char *) "test it here"
#define MESSAGE_LEN 12 


void printarr(unsigned char a[], int l) {
    int i;
    for (i=0; i<l; i++)
    {
        printf("%02X", a[i]);
    }
    printf("\n");
}

int main (void)
{
    printf("%d", crypto_sign_PUBLICKEYBYTES); 
    // unsigned char pk[crypto_sign_PUBLICKEYBYTES];
    // unsigned char sk[crypto_sign_SECRETKEYBYTES];
    // unsigned char seed[32] = {0x34, 0xeb, 0xa3, 0x9f, 0xc5, 0xa1, 0xb4, 0x1d, 0x64, 0x12, 0xce, 0xc3, 0xd2, 0x0a, 0x7f, 0xa8, 0x24, 0x24, 0x2a, 0xdc, 0x1e, 0x6c, 0x04, 0x48, 0xce, 0x91, 0xb3, 0xc4, 0x84, 0xcc, 0x7a, 0xc6};
    // crypto_sign_seed_keypair(pk, sk, seed);
    // 
    // unsigned char sig[crypto_sign_BYTES];
    // 
    // crypto_sign_detached(sig, NULL, MESSAGE, MESSAGE_LEN, sk);

    // printf("The secret key is:\n");
    // printarr(sk, 32);
    // printf("The public key is:\n");
    // printarr(pk, 32);
    // printf("The signature is:\n");
    // printarr(sig, 64);
    // 
    // if (crypto_sign_verify_detached(sig, MESSAGE, MESSAGE_LEN, pk) != 0) {
    //     printf("Verification failed.");
    // }
}
