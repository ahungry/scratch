// reference: https://stackoverflow.com/questions/62458175/hmac-sha256-in-c-with-openssl-how-to-get-a-correct-output
// compile via: gcc -Wall -lcrypto -lssl ./hmac.c -o hmac && ./hmac
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <string.h>

unsigned char *mx_hmac_sha256(const void *key, int keylen,
                              const unsigned char *data, int datalen,
                              unsigned char *result, unsigned int *resultlen) {
  return HMAC(EVP_sha256(), key, keylen, data, datalen, result, resultlen);
}

// The program is the same (output etc.) as this one liner on CLI
// echo -n 'this is highly sensitive user data' | openssl dgst -sha256 -hmac 'security is awesome'
// (stdin)= ccdbe6f787d5b96dba796b27ea484811ea54e52c7cf271f1acbec75fcdc91012

int
main (int argc, char *argv[]) {
  char *key = strdup("security is awesome");
  int keylen = strlen(key);
  const unsigned char *data = (const unsigned char *)strdup("this is highly sensitive user data");
  int datalen = strlen((char *)data);
  unsigned char *result = NULL;
  unsigned int resultlen = -1;

  result = mx_hmac_sha256((const void *)key, keylen, data, datalen, result, &resultlen);

  for (unsigned int i = 0; i < resultlen; i++)
    printf("%c", result[i]);

  printf("\n");
  for (unsigned int i = 0; i < resultlen; i++)
    printf("%u ", result[i]);

  printf("\nencrypted: %s   len = %d\n", result, resultlen);

  for (unsigned int i = 0; i < resultlen; i++){
    printf("%02x", result[i]); // or just "%02X" if you are not using C11 or later
  }

  // TODO: Come up with the equivalent of an echo to sha256sum and base64
  // https://stackoverflow.com/questions/5288076/base64-encoding-and-decoding-with-openssl
}
