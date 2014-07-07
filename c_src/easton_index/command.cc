
#include <stdlib.h>

#include "command.hh"
#include "config.hh"
#include "io.hh"


static void
close_idx(easton_idx_t* idx, const unsigned char* cmd, size_t cmdlen)
{
    if(cmdlen != 0) {
        exit(EASTON_ERROR_BAD_CLOSE);
    }

    if(!easton_index_close(idx)) {
        exit(EASTON_ERROR_CLOSE_FAIL);
    }

    easton_send_ok(NULL, 0);

    exit(EASTON_OK);
}


static void
flush_idx(easton_idx_t* idx, const unsigned char* cmd, size_t cmdlen)
{
    if(cmdlen != 0) {
        exit(EASTON_ERROR_BAD_FLUSH);
    }

    if(!easton_index_flush(idx)) {
        exit(EASTON_ERROR_FLUSH_FAIL);
    }
    
    easton_send_ok(NULL, 0);
}




void
easton_handle_command(easton_idx_t* idx,
        const unsigned char* cmd, size_t cmdlen)
{
    int op;

    if(cmdlen < 4) {
        exit(EASTON_ERROR_BAD_COMMAND);
    }

    memcpy(&op, cmd, 4);
    op = ntohl(op);

    // Bump to after the op code
    cmd += 4;
    cmdlen -= 4;

    switch(op) {
        case EASTON_COMMAND_CLOSE:
            close_idx(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_FLUSH:
            flush_idx(idx, cmd, cmdlen);
            break;
        default:
            exit(EASTON_ERROR_BAD_COMMAND);
    }
}
