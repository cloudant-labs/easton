
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
get_index_info(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    bool v;

    if(!reader->read(v)) {
        throw EastonException("Invalid argument for get_doc_count.");
    }

    if(!v) {
        throw EastonException("Invalid boolean for get_doc_count.");
    }

    uint64_t doc_id_num = idx->curr_docid_num();
    uint64_t doc_count = idx->get_doc_count();
    uint64_t geom_count = idx->get_geom_count();
    uint64_t data_size = idx->data_size();

    io::Writer::Ptr writer = io::Writer::create();

    // This encodes {ok, [{tag, value} | ...]}
    writer->start_tuple(2);
    writer->write("ok");

    writer->start_list(4);

    writer->start_tuple(2);
    writer->write("data_size");
    writer->write(data_size);

    writer->start_tuple(2);
    writer->write("doc_count");
    writer->write(doc_count);

    writer->start_tuple(2);
    writer->write("geom_count");
    writer->write(geom_count);

    writer->start_tuple(2);
    writer->write("doc_id_num");
    writer->write(doc_id_num);

    writer->write_empty_list();

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

    int32_t num_entries;
    if(!reader->read_list(num_entries)) {
        throw EastonException("Invalid WKB list for update_entry.");
    }

    Entry::Vector entries;
    for(int32_t i = 0; i < num_entries; i++) {
        Entry::Ptr e = idx->get_reader()->read_update(reader);
        entries.push_back(e);
    }

    if(!reader->read_empty_list()) {
        throw EastonException("Improper entry list for update.");
    }

    idx->update(docid, entries);

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
search_entries(easton::Index::Ptr idx, io::Reader::Ptr reader)
{
    geo::Ctx::Ptr ctx = idx->get_geo_ctx();

    int32_t arity;
    if(!reader->read_list(arity)) {
        throw EastonException("Invalid argument for search.");
    }

    if(arity < 7 || arity > 8) {
        throw EastonException("Invalid argument arity for search.");
    }

    geo::SRID::Ptr ctx_srid = ctx->get_srid();
    geo::SRID::Ptr req_srid = geo::SRID::from_reader(reader);
    geo::SRID::Ptr resp_srid = geo::SRID::from_reader(reader);

    if(!req_srid) {
        req_srid = ctx_srid;
    }

    if(!resp_srid) {
        resp_srid = ctx_srid;
    }

    Entry::Ptr entry = idx->get_reader()->read_query(reader, req_srid);
    if(!entry) {
        throw EastonException("Invalid query argument for search.");
    }

    uint64_t filter;
    bool nearest;
    uint64_t limit;
    bool include_geom;

    if(!reader->read(filter)) {
        throw EastonException("Invalid filter argument for search.");
    }

    if(!reader->read(nearest)) {
        throw EastonException("Invalid nearest argument for search.");
    }

    if(!reader->read(limit)) {
        throw EastonException("Invalid limit argument for search.");
    }

    if(!reader->read(include_geom)) {
        throw EastonException("Invalid include_geom argument for search.");
    }

    Hit bookmark;
    if(arity == 8) {
        if(!reader->read_tuple_n(2)) {
            throw EastonException("Invalid bookmark argument for search.");
        }
        bookmark.docid = reader->read_bytes();
        if(!reader->read(bookmark.distance)) {
            throw EastonException("Invalid bookmark distance for search.");
        }
    }

    if(!reader->read_empty_list()) {
        throw EastonException("Invalid list for search argument.");
    }

    geo::GeomFilter filtobj = entry->make_filter(ctx, filter);
    TopHits collector(bookmark, filtobj, limit);

    idx->search(collector, entry, nearest);

    // Reverse and maybe reproject the hits
    std::unique_ptr<Hit[]> results(new Hit[collector.size()]);
    uint32_t count = collector.size();
    for(uint32_t i = 0; i < count; i++) {
        Hit h = collector.pop();

        if(include_geom) {
            if(resp_srid != ctx_srid) {
                h.wkb = h.geom->reproject(ctx_srid, resp_srid)->to_wkb();
            } else {
                h.wkb = h.geom->to_wkb();
            }
        }

        results[count - i - 1] = h;
    }

    // Send the results back to Erlang
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write("ok");
    writer->start_list(count);
    for(uint32_t i = 0; i < count; i++) {
        if(include_geom) {
            writer->start_tuple(3);
            writer->write(results[i].docid);
            writer->write(results[i].distance);
            writer->write(results[i].wkb);
        } else {
            writer->start_tuple(2);
            writer->write(results[i].docid);
            writer->write(results[i].distance);
        }
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

    idx->reset_index();

    switch(op) {
        case EASTON_COMMAND_CLOSE:
            return close_idx(idx, reader);
        case EASTON_COMMAND_SYNC:
            return sync_idx(idx, reader);
        case EASTON_COMMAND_GET_INDEX_INFO:
            return get_index_info(idx, reader);
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
        case EASTON_COMMAND_SEARCH:
            return search_entries(idx, reader);
        default:
            throw EastonException("Unknown command op.");
    }
}


NS_EASTON_CMD_END
