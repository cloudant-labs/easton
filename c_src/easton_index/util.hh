
#ifndef EASTON_UTIL_HH
#define EASTON_UTIL_HH


#include <stdbool.h>


bool easton_is_dir(const int8_t* path);
bool easton_read_uint16(const uint8_t** cmd, uint32_t* cmdlen, uint16_t* ret);
bool easton_read_binary(const uint8_t** cmd, uint32_t* cmdlen,
        const void** buf, uint32_t* buflen);

#endif