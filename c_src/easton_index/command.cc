
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

    bool ret = idx->del_kv(key);

    io::Writer::Ptr writer = io::Writer::create();
    writer->write(ret);

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


// static void
// do_query(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
// {
//     GEOSGeometry* query = NULL;
//     easton_geom_filt_t* filtfun;
//
//     uint8_t* wkb;
//     uint32_t wkblen;
//     uint8_t filter;
//     uint8_t nearest;
//     uint64_t lim;
//     uint64_t off;
//     std::vector<bytes> docids;
//     std::vector<bytes> wkbs;
//     uint8_t* buf = NULL;
//     uint8_t* pos = NULL;
//     uint64_t buflen;
//     uint64_t i;
//
//     if(!easton_read_binary(&cmd, &cmdlen, (const void**) &wkb, &wkblen)) {
//         exit(EASTON_ERROR_BAD_WKB);
//     }
//
//     if(!easton_read_uint8(&cmd, &cmdlen, &filter)) {
//         exit(EASTON_ERROR_BAD_FILTER);
//     }
//
//     if(!easton_read_uint8(&cmd, &cmdlen, &nearest)) {
//         exit(EASTON_ERROR_BAD_NEAREST);
//     }
//
//     if(!easton_read_uint64(&cmd, &cmdlen, &lim)) {
//         exit(EASTON_ERROR_BAD_LIMIT);
//     }
//
//     if(!easton_read_uint64(&cmd, &cmdlen, &off)) {
//         exit(EASTON_ERROR_BAD_OFFSET);
//     }
//
//     query = easton_geom_from_wkb(idx, wkb, wkblen);
//     if(query == NULL) {
//         easton_send_error(NULL, 0);
//         goto done;
//     }
//
//     filtfun = easton_geom_get_filter(idx, filter);
//     if(filtfun == NULL) {
//         easton_send_error(NULL, 0);
//         goto done;
//     }
//
//     if(!easton_index_query(idx, query, filt, nearest, lim, off, docids, wkbs)) {
//         goto done;
//     }
//
//     buflen = sizeof(uint64_t);
//     for(i = 0; i < num_results; i++) {
//         buflen += sizeof(uint32_t) + docidlens[i];
//         buflen += sizeof(uint32_t) + wkblens[i];
//     }
//
//     buf = (uint8_t*) malloc(buflen);
//     pos = buf;
//
//     easton_write_uint64(pos, num_results);
//     pos += sizeof(uint64_t);
//
//     for(i = 0; i < num_results; i++) {
//         easton_write_binary(buf, &(docids[i][0]), docids[i].size());
//         easton_write_binary(buf, &(wkbs[i][0]), wkbs[i].size());
//     }
//
//     easton_send_ok(buf, buflen);
//
// done:
//
//     if(query != NULL) {
//         GEOSGeom_destroy_r(idx->geos_ctx, query);
//     }
//
//     if(docids != NULL) {
//         free(docids);
//     }
//
//     if(docidlens != NULL) {
//         free(docidlens);
//     }
//
//     if(wkbs != NULL) {
//         free(wkbs);
//     }
//
//     if(wkblens != NULL) {
//         free(wkblens);
//     }
//
//     if(buf != NULL) {
//         free(buff);
//     }
//
//     // Do *not* free the pos pointer.
// }


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
        // case EASTON_COMMAND_QUERY:
        //     return do_query(idx, reader);
        default:
            throw EastonException("Unknown command op.");
    }
}


NS_EASTON_CMD_END
