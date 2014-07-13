
#include <stdio.h>

#include <leveldb/db.h>

#include "command.hh"
#include "config.hh"
#include "exceptions.hh"
#include "index.hh"
#include "init.hh"
#include "io.hh"


using namespace easton;


void
run(int argc, const char* argv[])
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


void
destroy(int argc, const char* argv[])
{
    if(argc != 1) {
        fprintf(stderr, "No index directory specified.\r\n");
        exit(-1);
    }

    init();

    leveldb::Options opts;
    leveldb::Status s = leveldb::DestroyDB(argv[0], opts);
    if(!s.ok()) {
        fprintf(stderr, "Error destroying index: %s", s.ToString().c_str());
        exit(-1);
    }

    exit(EASTON_OK);
}


int
main(int argc, const char* argv[])
{
    if(argc < 2) {
        fprintf(stderr, "usage: %s action [args...]\r\n", argv[0]);
        exit(-1);
    }

    std::string action = argv[1];
    if(action == "run") {
        run(argc - 2, argv + 2);
    } else if(action == "destroy") {
        destroy(argc - 2, argv + 2);
    } else {
        fprintf(stderr, "Invalid action: %s\r\n", action.c_str());
    }
}
