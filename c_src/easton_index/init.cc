

#include <arpa/inet.h>

#include <CsMap/cs_map.h>

#include "config.hh"
#include "init.hh"
#include "io.hh"


static void
report_pid()
{
    uint32_t p = htonl((uint32_t) getpid());
    easton_send_data((uint8_t*) &p, sizeof(p));
}


static void
init_csmap()
{
    const int8_t* map_dir = (int8_t*) getenv("EASTON_CS_MAP_DIR");

	if(map_dir != NULL) {
        CS_altdr((char*) map_dir);
    } else {
        CS_altdr((char*) EASTON_DEFAULT_CS_MAP_DIR);
    }

	CS_init(0);
}


void
easton_init()
{
    report_pid();
    init_csmap();
}
