
#include <execinfo.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <CsMap/cs_map.h>
#include <CsMap/csNameMapperSupport.h>

#include "config.hh"
#include "epsg.hh"
#include "init.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_BEGIN


void
show_stack(int sig)
{
    void* frames[64];
    size_t size;
    const int8_t* dbg_info = (int8_t*) getenv("EASTON_DEBUG_INFO");

    if(dbg_info != NULL) {
        fprintf(stderr, "Error: Signal %d\n", sig);
    } else {
        fprintf(stderr, "Error: Signal %d :: %s\n", sig, dbg_info);
    }

    size = backtrace(frames, 64);
    backtrace_symbols_fd(frames, size, STDERR_FILENO);

    if(sig == 0) {
        return;
    }

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
    uint32_t p = (uint32_t) getpid();
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
maybe_check_epsg_table()
{
    const char* do_check = getenv("EASTON_CHECK_EPSG_TABLE");
    if(do_check == NULL) {
        return;
    }

    std::string check(do_check);
    if(check == "" || check == "0" || check == "false") {
        return;
    }

    fprintf(stderr, "Checking Easton EPSG tables.\r\n");

    bool error = false;

    for(uint64_t i = EASTON_EPSG_MIN; i <= EASTON_EPSG_MAX; i++) {
        const char* easton_name = easton_epsg_lookup(i);
        if(easton_name == NULL) {
            continue;
        }

        const char* csmap_name = CSepsg2adskCS((long) i);
        if(csmap_name == NULL) {
            continue;
        }

        if(strcmp(easton_name, csmap_name) != 0) {
            fprintf(stderr, "INVALID EPSG NAME: %llu %s != %s\r\n",
                   (long long unsigned int) i, easton_name, csmap_name);
            error = true;
        }
    }

    if(error) {
        exit(254);
    }
}


void
init()
{
    init_signals();
    report_pid();
    init_csmap();
    maybe_check_epsg_table();
}


NS_EASTON_END
