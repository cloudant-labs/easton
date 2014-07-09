
#include <string.h>

#include <tcutil.h>

#include "cowboys.hh"
#include "util.hh"


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
easton_read_uint32(const uint8_t** cmd, uint32_t* cmdlen, uint32_t* ret)
{
    uint32_t val;

    if(*cmdlen < sizeof(val)) {
        return false;
    }

    memcpy(&val, *cmd, sizeof(val));
    val = ntohl(val);

    *cmd += sizeof(val);
    *cmdlen -= sizeof(val);
    *ret = val;

    return true;
}


bool
easton_read_uint64(const uint8_t** cmd, uint32_t* cmdlen, uint64_t* ret)
{
    uint64_t val;

    if(*cmdlen < sizeof(val)) {
        return false;
    }

    memcpy(&val, *cmd, sizeof(val));
    val = be64toh(val);

    *cmd += sizeof(val);
    *cmdlen -= sizeof(val);
    *ret = val;

    return true;
}


bool
easton_read_double(const uint8_t** cmd, uint32_t* cmdlen, double* ret)
{
    uint64_t tmp;

    if(!easton_read_uint64(cmd, cmdlen, &tmp)) {
        return false;
    }

    memcpy(ret, &tmp, sizeof(double));

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


void
easton_write_uint32(uint8_t* buf, uint32_t val)
{
    val = htobe32(val);
    memcpy(buf, &val, sizeof(val));
}


void
easton_write_uint64(uint8_t* buf, uint64_t val)
{
    val = htobe64(val);
    memcpy(buf, &val, sizeof(val));
}


void
easton_write_double(uint8_t* buf, double val)
{
    uint64_t tmp;

    memcpy(&tmp, &val, sizeof(double)); // Assumes double is 64 bits
    easton_write_uint64(buf, tmp);
}
