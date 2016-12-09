
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
run(io::Reader::Ptr opts)
{
    io::Reader::Ptr r;
    io::Writer::Ptr w;

    easton::Index::Ptr idx = easton::Index::create(opts);

    w = io::Writer::create();
    w->write("ok");
    w->send();

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
}


void
destroy(io::Reader::Ptr reader)
{
    std::string dir;
    if(!reader->read(dir)) {
        throw EastonException("Invalid index directory for destroy.");
    }

    io::Writer::Ptr w = io::Writer::create();
    w->write("ok");
    w->send();

    leveldb::Options opts;
    leveldb::Status s = leveldb::DestroyDB(dir, opts);
    if(!s.ok()) {
        throw EastonException("Error destroying index: " + s.ToString());
    }
}


int
main(int argc, const char* argv[])
{
    try {

        init();

        io::Reader::Ptr reader = io::Reader::recv();

        if(reader == NULL) {
            throw EastonException("Error getting data from reader.");
        }

        if(!reader->read_tuple_n(2)) {
            throw EastonException("Invalid command tuple.");
        }

        std::string cmd;
        if(!reader->read(cmd)) {
            throw EastonException("Invalid index command.");
        }

        if(cmd == "run") {
            run(reader);
        } else if(cmd == "destroy") {
            destroy(reader);
        } else {
            throw EastonException("Unknown index command: " + cmd);
        }
    } catch(EastonExit& e) {
        exit(e.code);
    } catch(std::exception& e) {
        fprintf(stderr, "ERROR: %s\r\n", e.what());
        show_stack(255);
    } catch(...) {
        fprintf(stderr, "UNKNOWN ERROR\r\n");
        show_stack(255);
    }
}
