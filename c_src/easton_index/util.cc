
#include <string.h>

#include <tcutil.h>

#include "util.h"


bool
easton_is_dir(const char* path)
{
    bool is_dir;
    
    if(!tcstatfile(path, &is_dir, NULL, NULL)) {
        return false;
    }
    
    return is_dir;
}


bool
easton_read_binary(const unsigned char** cmd, size_t* cmdlen,
    const void** buf, size_t* buflen)
{
    unsigned int len;
    
    if(*cmdlen < sizeof(unsigned int)) {
        return false;
    }

    memcpy(&len, *cmd, sizeof(unsigned int));
    len = ntohl(len);

    *buf = *cmd + sizeof(unsigned int);    
    *buflen = (size_t) len;

    if(*buflen > *cmdlen) {
        return false;
    }

    *cmd += *buflen + sizeof(unsigned int);
    *cmdlen -= *buflen + sizeof(unsigned int);

    return true;
}