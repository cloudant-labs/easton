
#include <stdio.h>

#include "command.hh"
#include "exceptions.hh"
#include "index.hh"
#include "init.hh"
#include "io.hh"


using namespace easton;


int
main(int argc, const char* argv[])
{
    try {
        init();

        io::Reader::Ptr r;
        io::Writer::Ptr w;

        easton::Index::Ptr idx = easton::Index::create(argc, argv);

        while((r = io::Reader::recv())) {
            try {
                w = cmd::handle(idx, r);
                w->send();
            } catch(EastonExit& e) {
                exit(e.code);
            } catch(std::exception& e) {
                w = io::Writer::create();
                w->start_tuple(2);
                w->write("error");
                w->write(io::Bytes::proxy(e.what()));
                w->send();
            }
        }

        exit(0);

    } catch(std::exception& e) {
        fprintf(stderr, "ERROR: %s\r\n", e.what());
        show_stack(255);
    } catch(...) {
        fprintf(stderr, "UNKNOWN ERROR\r\n");
        show_stack(255);
    }
}
