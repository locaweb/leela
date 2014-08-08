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

#include "base64.h"

const char leela_b64_default_alphabet[65] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
                                             'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                                             'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                                             'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
                                             'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
                                             'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                                             'w', 'x', 'y', 'z', '0', '1', '2', '3',
                                             '4', '5', '6', '7', '8', '9', '+', '/',
                                             '='
                                            };

static
int leela_b64_encode_ (const char *src, size_t srclen, size_t offset)
{
  int v = 0;
  if (offset++ < srclen)
  { v |= (src[offset-1] << 16); }
  if (offset++ < srclen)
  { v |= (src[offset-1] << 8); }
  if (offset++ < srclen)
  { v |= src[offset-1]; }
  return(v);
}

size_t leela_b64encode (const char alphabet[65], char *dst, size_t dstlen, const char *src, size_t srclen)
{
  int code;
  size_t at, tmp, len = 0;
  for (at=0; at<srclen; at+=3)
  {
    code = leela_b64_encode_(src, srclen, at);
    len += 4;
    for (tmp=1; tmp<=4; tmp+=1)
    {
      if (len-tmp < dstlen)
      { dst[len-tmp] = alphabet[code & 0x3f]; }
      code = code >> 6;
    }
  }

  switch (srclen % 3)
  {
  case 1:
    if (len-1 < dstlen)
    { dst[len-1] = alphabet[64]; }
    if (len-2 < dstlen)
    { dst[len-2] = alphabet[64]; }
    break;
  case 2:
    if (len-1 < dstlen)
    { dst[len-1] = alphabet[64]; }
    break;
  }

  if (len++ < dstlen)
  { dst[len-1] = '\0'; }
  else if (dstlen > 0)
  { dst[dstlen] = '\0'; }
  return(len);
}
