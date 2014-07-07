
#include <string.h>

#include <tcutil.h>

#include "util.h"


bool
easton_is_dir(const int8_t* path)
{
    bool is_dir;

    if(!tcstatfile((const char*) path, &is_dir, NULL, NULL)) {
        return false;
    }

    return is_dir;
}


bool
easton_read_uint16(const uint8_t** cmd, uint32_t* cmdlen, uint16_t* ret)
{
    uint16_t op;

    if(*cmdlen < sizeof(op)) {
        return false;
    }

    memcpy(&op, *cmd, sizeof(op));
    op = ntohs(op);

    *cmd += sizeof(op);
    *cmdlen -= sizeof(op);
    *ret = op;

    return true;
}

bool
easton_read_binary(const uint8_t** cmd, uint32_t* cmdlen,
    const void** buf, uint32_t* buflen)
{
    uint32_t len;

    if(*cmdlen < sizeof(uint32_t)) {
        return false;
    }

    memcpy(&len, *cmd, sizeof(uint32_t));
    len = ntohl(len);

    *buf = *cmd + sizeof(uint32_t);
    *buflen = len;

    if(*buflen > *cmdlen) {
        return false;
    }

    *cmd += *buflen + sizeof(uint32_t);
    *cmdlen -= *buflen + sizeof(uint32_t);

    return true;
}
