

#include <unistd.h>
#include <arpa/inet.h>

#include <CsMap/cs_map.h>

#include "config.hh"
#include "init.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_BEGIN


static void
report_pid()
{
    uint32_t p = htonl((uint32_t) getpid());
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write("ok");
    writer->write((uint64_t) p);
    writer->send();
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
init()
{
    report_pid();
    init_csmap();
}


NS_EASTON_END
