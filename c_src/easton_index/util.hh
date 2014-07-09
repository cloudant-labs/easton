
#ifndef EASTON_UTIL_HH
#define EASTON_UTIL_HH


#include <stdbool.h>

bool easton_is_dir(const int8_t* path);
bool easton_read_uint16(const uint8_t** cmd, uint32_t* cmdlen, uint16_t* ret);
bool easton_read_uint32(const uint8_t** cmd, uint32_t* cmdlen, uint32_t* ret);
bool easton_read_uint64(const uint8_t** cmd, uint32_t* cmdlen, uint64_t* ret);
bool easton_read_double(const uint8_t** cmd, uint32_t* cmdlen, double* ret);
bool easton_read_binary(const uint8_t** cmd, uint32_t* cmdlen,
        const void** buf, uint32_t* buflen);

void easton_write_uint32(uint8_t* buf, uint32_t val);
void easton_write_uint64(uint8_t* buf, uint64_t val);
void easton_write_double(uint8_t* buf, double val);

#endif
