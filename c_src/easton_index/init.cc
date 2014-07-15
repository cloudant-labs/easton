
#include <execinfo.h>
#include <signal.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <CsMap/cs_map.h>

#include "config.hh"
#include "init.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_BEGIN


void
show_stack(int sig)
{
    void* frames[64];
    size_t size;

    size = backtrace(frames, 64);

    fprintf(stderr, "Error: Signal %d:\n", sig);
    backtrace_symbols_fd(frames, size, STDERR_FILENO);
    exit(255);
}


static void
init_signals()
{
    signal(SIGINT, show_stack);
    signal(SIGQUIT, show_stack);
    signal(SIGABRT, show_stack);
    signal(SIGKILL, show_stack);
    signal(SIGBUS, show_stack);
    signal(SIGSEGV, show_stack);
}


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
    init_signals();
    report_pid();
    init_csmap();
}


NS_EASTON_END
