
#include <stdio.h>

#include "command.hh"
#include "index.hh"
#include "init.hh"
#include "io.hh"


int
main(int argc, const char* argv[])
{
    unsigned char* cmd_buf;
    size_t cmd_len;

    easton_init();
    easton_idx_t* idx = easton_index_init(argc, argv);

    while(easton_read_data(&cmd_buf, &cmd_len)) {
        easton_handle_command(idx, cmd_buf, cmd_len);
        free(cmd_buf);
    }

    return 0;
}
