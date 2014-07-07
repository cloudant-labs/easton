
#include <stdio.h>

#include "command.hh"
#include "index.hh"
#include "init.hh"
#include "io.hh"


int
main(int argc, const char* argv[])
{
    uint8_t* cmd;
    uint32_t cmdlen;

    easton_init();
    easton_idx_t* idx = easton_index_init(argc, (const int8_t**) argv);

    while(easton_read_data(&cmd, &cmdlen)) {
        easton_handle_command(idx, cmd, cmdlen);
        free(cmd);
    }

    return 0;
}
