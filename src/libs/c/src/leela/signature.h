/* Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
 *  
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef __leela_signature_h__
#define __leela_signature_h__

#include <stdlib.h>
#include "base.h"

LEELA_CPLUSPLUS_OPEN

#define LEELA_SIGNATURE_SIZE 16
#define LEELA_SIGNATURE_HEX_SIZE 33
#define LEELA_SIGNATURE_SEED_SIZE 32
#define LEELA_SIGNATURE_NONCE_SIZE 16

typedef struct leela_signature_t leela_signature_t;

/*! Encodes the signature or nonce using base16 into a valid c-string
 *  (null terminated).
 *
 *  \param dst A variable that can take at least size*2+1 bytes;
 *  
 *  \param src The buffer to encode;
 *  
 *  \param size The size of src;
 */
void leela_signature_hexencode (char *dst, const unsigned char *src, size_t size);

/*! Decodes a hex-encoded string.
 *
 *  \param dst A variable that can take size bytes;
 *  
 *  \param src The buffer to decode (this must be at least size*2+1 bytes);
 *  
 *  \param size The size of dst;
 */
void leela_signature_hexdecode (unsigned char *dst, size_t size, const char *src);

/*! Initializes the signature framework. The seed must be exactly
 *  LEELA_SIGNATURE_SEED_SIZE long.
 *
 *  \return NULL: something went wrong;
 *          _   : a valid pointer;
 */
leela_signature_t *leela_signature_init (const unsigned char seed[LEELA_SIGNATURE_SEED_SIZE]);

/*! Generates a new nonce.
 */
void leela_signature_nonce_next (unsigned char dst[LEELA_SIGNATURE_NONCE_SIZE], const unsigned char src[LEELA_SIGNATURE_NONCE_SIZE]);

/*! Signs a new message.
 *
 *  \param sig The pointer created with leela_signature_init;
 *  
 *  \param mac The variable that takes the signature;
 *  
 *  \param nonce The nonce to use. Make sure you use the
 *  leela_signature_nonce to generate a new nonce (extremely
 *  important!);
 *  
 *  \param msg The message to sign;
 *  
 *  \param msglen The size of this message;
 */
void leela_signature_sign (leela_signature_t *sig, unsigned char mac[LEELA_SIGNATURE_SIZE], const unsigned char nonce[LEELA_SIGNATURE_NONCE_SIZE], const void *msg, unsigned int msglen);

/*! Checks wheter or not the signature is valid.
 *
 *  \param s The pointer created with leela_signature_init;
 *  
 *  \param sig The signature to check;
 *  
 *  \param nonce The nonce used to sign this message;
 *  
 *  \param msg The message to check;
 *  
 *  \param msglen The size of this message;
 *  
 *  \return 0: Ok;
 *          x: An error has ocurred;
 */
int leela_signature_check (leela_signature_t *s, const unsigned char sig[LEELA_SIGNATURE_SIZE], const unsigned char nonce[LEELA_SIGNATURE_NONCE_SIZE], const void *msg, unsigned int msglen);

/*! Frees all memory.
 */
void leela_signature_destroy (leela_signature_t *sig);

LEELA_CPLUSPLUS_CLOSE

#endif
