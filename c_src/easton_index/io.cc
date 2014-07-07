
#include <stdio.h>

#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>

#include "config.hh"


bool
easton_read_data(unsigned char** data, size_t* len)
{
    unsigned int packet_len;
    unsigned char* buf;
    ssize_t ret;

    ret = read(EASTON_STREAM_IN, &packet_len, sizeof(unsigned int));
    if(ret == 0) {
        return false;
    } else if(ret != sizeof(unsigned int)) {
        exit(EASTON_ERROR_BAD_READ);
    }

    packet_len = ntohl(packet_len);

    buf = (unsigned char*) malloc(packet_len);
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
easton_send_data(unsigned char* data, size_t len)
{
    unsigned int packet_len = htonl((unsigned int) len);
    ssize_t ret;

    ret = write(EASTON_STREAM_OUT, &packet_len, sizeof(unsigned int));
    if(ret != sizeof(unsigned int)) {
        exit(EASTON_ERROR_BAD_WRITE);
    }

    ret = write(EASTON_STREAM_OUT, data, len);
    if(ret != len) {
        exit(EASTON_ERROR_BAD_WRITE);
    }
}


void
easton_send_resp(unsigned char code, unsigned char* data, size_t len)
{
    unsigned int packet_len = htonl((unsigned int) (len + 1));
    ssize_t ret;

    ret = write(EASTON_STREAM_OUT, &packet_len, sizeof(unsigned int));
    if(ret != sizeof(unsigned int)) {
        fprintf(stderr, "1\r\n");
        exit(EASTON_ERROR_BAD_WRITE);
    }

    ret = write(EASTON_STREAM_OUT, &code, 1);
    if(ret != 1) {
        fprintf(stderr, "2\r\n");
        exit(EASTON_ERROR_BAD_WRITE);
    }

    if(len ==  0) {
        return;
    }

    ret = write(EASTON_STREAM_OUT, data, len);
    if(ret != len) {
        fprintf(stderr, "3\r\n");
        exit(EASTON_ERROR_BAD_WRITE);
    }
}


void
easton_send_ok(unsigned char* data, size_t len)
{
    easton_send_resp(0, data, len);
}


void
easton_send_error(unsigned char* data, size_t len)
{
    easton_send_resp(1, data, len);
}
