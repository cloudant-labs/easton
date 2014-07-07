

#include <arpa/inet.h>

#include <CsMap/cs_map.h>

#include "config.hh"
#include "init.hh"
#include "io.hh"


static void
report_pid()
{
    int p = htonl((unsigned int) getpid());
    easton_send_data((unsigned char*) &p, sizeof(p));
}


static void
init_csmap()
{
    const char* map_dir = getenv("EASTON_CS_MAP_DIR");

	if(map_dir != NULL) {
        CS_altdr(map_dir);
    } else {
        CS_altdr(EASTON_DEFAULT_CS_MAP_DIR);
    }

	CS_init(0);
}


void
easton_init()
{
    report_pid();
    init_csmap();
}
