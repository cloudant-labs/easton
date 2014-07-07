
#ifndef EASTON_UTIL_HH
#define EASTON_UTIL_HH


#include <stdbool.h>


bool easton_is_dir(const char* path);
bool easton_read_binary(const unsigned char** cmd, size_t* cmdlen,
        const void** buf, size_t* buflen);

#endif