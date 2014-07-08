
#include <stdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "config.hh"


bool
easton_read_data(uint8_t** data, uint32_t* len)
{
    uint32_t packet_len;
    uint8_t* buf;
    uint32_t ret;

    ret = read(EASTON_STREAM_IN, &packet_len, sizeof(uint32_t));
    if(ret == 0) {
        return false;
    } else if(ret != sizeof(uint32_t)) {
        exit(EASTON_ERROR_BAD_READ);
    }

    packet_len = ntohl(packet_len);

    buf = (uint8_t*) malloc(packet_len);
    if(buf == NULL) {
        exit(EASTON_ERROR_BAD_ALLOC);
    }

    ret = read(EASTON_STREAM_IN, buf, packet_len);
    if(ret != packet_len) {
        exit(EASTON_ERROR_BAD_READ);
    }

    *data = buf;
    *len = packet_len;

    return true;
}


void
easton_send_data(uint8_t* data, uint32_t len)
{
    uint32_t packet_len = htonl((uint32_t) len);
    uint32_t ret;

    ret = write(EASTON_STREAM_OUT, &packet_len, sizeof(uint32_t));
    if(ret != sizeof(uint32_t)) {
        exit(EASTON_ERROR_BAD_WRITE);
    }

    ret = write(EASTON_STREAM_OUT, data, len);
    if(ret != len) {
        exit(EASTON_ERROR_BAD_WRITE);
    }
}


void
easton_send_resp(uint8_t code, uint8_t* data, uint32_t len)
{
    uint32_t packet_len = htonl((uint32_t) (len + 1));
    ssize_t ret;

    ret = write(EASTON_STREAM_OUT, &packet_len, sizeof(uint32_t));
    if(ret != sizeof(uint32_t)) {
        exit(EASTON_ERROR_BAD_WRITE);
    }

    ret = write(EASTON_STREAM_OUT, &code, 1);
    if(ret != 1) {
        exit(EASTON_ERROR_BAD_WRITE);
    }

    if(len ==  0) {
        return;
    }

    ret = write(EASTON_STREAM_OUT, data, len);
    if(ret != len) {
        exit(EASTON_ERROR_BAD_WRITE);
    }
}


void
easton_send_ok(uint8_t* data, uint32_t len)
{
    easton_send_resp(0, data, len);
}


void
easton_send_ok_uint64_t(uint64_t value)
{
    uint8_t msg[8];
    uint32_t i;

    // big-endian
    for(i = 0; i < 8; i++) {
        msg[8 - (i+1)] = (uint8_t) (((value) >> (i*8)) & 0xFF);
    }

    easton_send_ok(msg, 8);
}


void
easton_send_error(uint8_t* data, uint32_t len)
{
    easton_send_resp(1, data, len);
}
