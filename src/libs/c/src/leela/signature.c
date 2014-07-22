// Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "signature.h"
#include <poly1305aes/poly1305aes.h>

struct leela_signature_t
{
  unsigned char seed[LEELA_SIGNATURE_SEED_SIZE];
};

static inline
char __henc(unsigned char i)
{ return(i < 10 ? (i + 48) : (i+55)); }

static inline
unsigned char __hdec(unsigned char i)
{ return(i >= 65 ? (i - 55) : (i - 48)); }

void leela_signature_hexencode (char *dst, const unsigned char *src, size_t size)
{
  size_t i;
  for (i=0; i<size; i+=1)
  {
    dst[2 * i]     = __henc(src[i] >> 4);
    dst[2 * i + 1] = __henc(src[i] & 0x0F);
  }
  dst[2 * size] = '\0';
}

void leela_signature_hexdecode (unsigned char *dst, size_t size, const char *src)
{
  size_t i;
  for (i=0; i<size; i+=1)
  { dst[i] = (__hdec(src[2 * i]) << 4) | __hdec(src[2 * i + 1]); }
}

leela_signature_t *leela_signature_init (const unsigned char seed[LEELA_SIGNATURE_SEED_SIZE])
{
  leela_signature_t *sig = (leela_signature_t *) malloc(sizeof(leela_signature_t));
  if (sig != NULL)
  {
    memcpy(sig->seed, seed, LEELA_SIGNATURE_SEED_SIZE);
    poly1305aes_clamp(sig->seed);
  }
  return(sig);
}

void leela_signature_nonce_next (unsigned char dst[LEELA_SIGNATURE_NONCE_SIZE], const unsigned char src[LEELA_SIGNATURE_NONCE_SIZE])
{
  uint64_t nonce[2] = {0, 0};
  memcpy(nonce, src, LEELA_SIGNATURE_NONCE_SIZE);

  nonce[0] += 1;
  if (nonce[0] == 0)
  { nonce[1] += 1; }

  memcpy(dst, nonce, LEELA_SIGNATURE_NONCE_SIZE);
}

void leela_signature_sign (leela_signature_t *sig, unsigned char mac[LEELA_SIGNATURE_SIZE], const unsigned char nonce[LEELA_SIGNATURE_SIZE], const void *msg, unsigned int msglen)
{ poly1305aes_authenticate(mac, sig->seed, nonce, (const unsigned char *) msg, msglen); }

int leela_signature_check (leela_signature_t *s, const unsigned char mac[LEELA_SIGNATURE_SIZE], const unsigned char nonce[LEELA_SIGNATURE_SIZE], const void *msg, unsigned int msglen)
{ return(poly1305aes_verify(mac, s->seed, nonce, (const unsigned char *) msg, msglen) != 0 ? 0 : -1); }

void leela_signature_destroy (leela_signature_t *sig)
{ free(sig); }
