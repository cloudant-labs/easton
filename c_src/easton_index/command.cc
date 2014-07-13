
#include <stdlib.h>

#include "easton.hh"
#include "command.hh"
#include "config.hh"
#include "exceptions.hh"


using namespace easton;


NS_EASTON_CMD_BEGIN


io::Writer::Ptr
close_idx(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    // These simple functions that don't really take any
    // arguments are taking a boolean just to fit the
    // two-tuple protocol RPC style.w
    bool v;
    if(!reader->read(v)) {
        throw EastonException("Invalid argument for close_idx.");
    }

    if(!v) {
        throw EastonException("Invalid boolean for close_idx.");
    }

    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write("ok");
    writer->write(v);

    // Send here since we'll skip the normal send
    // with the exception handling.
    writer->send();
    throw EastonExit(EASTON_OK);
}


io::Writer::Ptr
sync_idx(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    bool v;
    if(!reader->read(v)) {
        throw EastonException("Invalid argument for sync_idx.");
    }

    if(!v) {
        throw EastonException("Invalid boolean for sync_idx.");
    }

    idx->sync();

    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write("ok");
    writer->write(true);

    return writer;
}


io::Writer::Ptr
get_doc_id_num(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    uint64_t doc_id_num;
    bool v;

    if(!reader->read(v)) {
        throw EastonException("Invalid argument for get_doc_id_num.");
    }

    if(!v) {
        throw EastonException("Invalid boolean for get_doc_id_num.");
    }

    doc_id_num = idx->curr_docid_num();

    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write("ok");
    writer->write(doc_id_num);

    return writer;
}


io::Writer::Ptr
get_doc_count(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    uint64_t count;
    bool v;

    if(!reader->read(v)) {
        throw EastonException("Invalid argument for get_doc_count.");
    }

    if(!v) {
        throw EastonException("Invalid boolean for get_doc_count.");
    }

    count = idx->doc_count();

    io::Writer::Ptr writer = io::Writer::create();

    writer->start_tuple(2);
    writer->write("ok");
    writer->write(count);

    return writer;
}


io::Writer::Ptr
put_user_kv(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Invalid argument for put_user_kv.");
    }

    io::Bytes::Ptr key = reader->read_bytes();
    if(!key) {
        throw EastonException("Invalid key argument for put_user_kv.");
    }

    io::Bytes::Ptr val = reader->read_bytes();
    if(!val) {
        throw EastonException("Invalid value argument for put_user_kv.");
    }

    idx->put_kv(key, val);

    io::Writer::Ptr writer = io::Writer::create();
    writer->write("ok");

    return writer;
}


io::Writer::Ptr
get_user_kv(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    io::Bytes::Ptr key = reader->read_bytes();
    if(!key) {
        throw EastonException("Invalid argument for get_user_kv.");
    }

    io::Bytes::Ptr val = idx->get_kv(key);

    io::Writer::Ptr writer = io::Writer::create();

    if(!val) {
        writer->write(false);
    } else {
        writer->start_tuple(2);
        writer->write("ok");
        writer->write(val);
    }

    return writer;
}


io::Writer::Ptr
del_user_kv(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    io::Bytes::Ptr key = reader->read_bytes();
    if(!key) {
        throw EastonException("Invalid argument for del_user_kv.");
    }

    idx->del_kv(key);

    io::Writer::Ptr writer = io::Writer::create();
    writer->write("ok");

    return writer;
}


io::Writer::Ptr
update_entry(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Invalid argument for update_entry.");
    }

    io::Bytes::Ptr docid = reader->read_bytes();
    if(!docid) {
        throw EastonException("Invalid docid for update_entry.");
    }

    int32_t num_wkbs;
    if(!reader->read_list(num_wkbs)) {
        throw EastonException("Invalid WKB list for update_entry.");
    }

    io::Bytes::Vector wkbs;
    for(int32_t i = 0; i < num_wkbs; i++) {
        io::Bytes::Ptr wkb = reader->read_bytes();
        if(!wkb) {
            throw EastonException("Invalid WKB in update_entry.");
        }
        wkbs.push_back(wkb);
    }

    idx->update(docid, wkbs);

    io::Writer::Ptr writer = io::Writer::create();
    writer->write("ok");

    return writer;
}


io::Writer::Ptr
remove_entry(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    io::Bytes::Ptr docid = reader->read_bytes();
    if(!docid) {
        throw EastonException("Invalid docid argument for remove_entry.");
    }

    idx->remove(docid);

    io::Writer::Ptr writer = io::Writer::create();
    writer->write("ok");

    return writer;
}


io::Writer::Ptr
query(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    geo::Ctx::Ptr ctx = idx->get_geo_ctx();
    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Invalid WKB argument for query.");
    }

    uint64_t filter;
    bool nearest;
    uint64_t limit;
    uint64_t offset;

    if(!reader->read(filter)) {
        throw EastonException("Invalid filter argument for query.");
    }

    if(!reader->read(nearest)) {
        throw EastonException("Invalid nearest argument for query.");
    }

    if(!reader->read(limit)) {
        throw EastonException("Invalid limit argument for query.");
    }

    if(!reader->read(offset)) {
        throw EastonException("Invalid offset argument for query.");
    }

    geo::Geom::Ptr query = ctx->from_wkb(wkb);
    if(!query) {
        throw EastonException("Unable to parse WKB geometry for query.");
    }

    geo::Bounds::Ptr bounds = query->get_bounds();
    std::vector<easton::Index::Result> results = idx->query(bounds, nearest);

    geo::GeomFilter filtfun = ctx->make_filter(query, filter);

    // Discard skip results and only keep up to
    // limit hits.
    std::vector<easton::Index::Result> hits;
    while(results.size() && hits.size() < limit) {
        easton::Index::Result r = results.at(0);
        results.erase(results.begin());

        geo::Geom::Ptr hit = ctx->from_wkb(r.second);

        // Anything that doesn't pass our filter was
        // a false positive so ignore it.
        if(!filtfun(hit)) {
            continue;
        }

        // Skip past the requested number of hits
        if(offset > 0) {
            offset--;
            continue;
        }

        hits.push_back(r);
    }

    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write("ok");
    writer->start_list(hits.size());
    for(uint32_t i = 0; i < hits.size(); i++) {
        easton::Index::Result r = hits.at(i);
        writer->start_tuple(2);
        writer->write(r.first);
        writer->write(r.second);
    }
    writer->write_empty_list();
    return writer;
}


io::Writer::Ptr
handle(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    uint64_t op;

    if(!reader->read_tuple_n(2)) {
        throw EastonException("Invalid command tuple.");
    }

    if(!reader->read(op)) {
        throw EastonException("Invalid command op.");
    }

    switch(op) {
        case EASTON_COMMAND_CLOSE:
            return close_idx(idx, reader);
        case EASTON_COMMAND_SYNC:
            return sync_idx(idx, reader);
        case EASTON_COMMAND_GET_DOC_ID_NUM:
            return get_doc_id_num(idx, reader);
        case EASTON_COMMAND_GET_DOC_COUNT:
            return get_doc_count(idx, reader);
        case EASTON_COMMAND_PUT_USER_KV:
            return put_user_kv(idx, reader);
        case EASTON_COMMAND_GET_USER_KV:
            return get_user_kv(idx, reader);
        case EASTON_COMMAND_DEL_USER_KV:
            return del_user_kv(idx, reader);
        case EASTON_COMMAND_UPDATE_ENTRIES:
            return update_entry(idx, reader);
        case EASTON_COMMAND_REMOVE_ENTRIES:
            return remove_entry(idx, reader);
        case EASTON_COMMAND_QUERY:
            return query(idx, reader);
        default:
            throw EastonException("Unknown command op.");
    }
}


NS_EASTON_CMD_END
