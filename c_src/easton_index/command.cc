
#include <stdlib.h>

#include "command.hh"
#include "config.hh"
#include "geom.hh"
#include "io.hh"
#include "util.hh"


static void
close_idx(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    if(!easton_index_close(idx)) {
        exit(EASTON_ERROR_CLOSE_FAIL);
    }

    easton_send_ok(NULL, 0);

    exit(EASTON_OK);
}


static void
flush_idx(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    if(!easton_index_flush(idx)) {
        exit(EASTON_ERROR_FLUSH_FAIL);
    }

    easton_send_ok(NULL, 0);
}


static void
get_doc_id_num(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint64_t doc_id_num;

    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    doc_id_num = easton_index_get_doc_id_num(idx);
    easton_send_ok_uint64_t(doc_id_num);
}


static void
get_doc_count(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint64_t count;

    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    count = easton_index_get_doc_count(idx);

    if(count != UINT64_MAX) {
        easton_send_ok_uint64_t(count);
    } else {
        easton_send_error(NULL, 0);
    }
}


static void
put_user_kv(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint8_t* key = NULL;
    uint8_t* val = NULL;
    uint32_t klen;
    uint32_t vlen;

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &key, &klen)) {
        exit(EASTON_ERROR_BAD_USER_KEY);
    }

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &val, &vlen)) {
        exit(EASTON_ERROR_BAD_USER_VAL);
    }

    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    if(!easton_index_put_kv(idx, key, klen, val, vlen)) {
        exit(EASTON_ERROR_BAD_PUT_USER_KV);
    }

    easton_send_ok(NULL, 0);
}


static void
get_user_kv(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint8_t* key;
    uint8_t* val;
    uint32_t klen;
    uint32_t vlen;

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &key, &klen)) {
        exit(EASTON_ERROR_BAD_USER_KEY);
    }

    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    val = (uint8_t*) easton_index_get_kv(idx, key, klen, &vlen);
    if(val != NULL) {
        easton_send_ok(val, vlen);
        free(val);
    } else {
        easton_send_error(NULL, 0);
    }
}


static void
del_user_kv(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint8_t* key;
    uint32_t klen;

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &key, &klen)) {
        exit(EASTON_ERROR_BAD_USER_KEY);
    }

    if(cmdlen != 0) {
        exit(EASTON_ERROR_TRAILING_DATA);
    }

    if(easton_index_del_kv(idx, key, klen)) {
        easton_send_ok(NULL, 0);
    } else {
        easton_send_error(NULL, 0);
    }
}


static void
update_rtree(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint8_t* docid;
    uint32_t docidlen;
    uint32_t numwkbs;
    uint8_t** wkbs = NULL;
    uint32_t* wkblens = NULL;
    uint8_t* wkb = NULL;
    uint32_t wkblen;
    uint32_t i;
    int code = EASTON_OK;

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &docid, &docidlen)) {
        code = EASTON_ERROR_BAD_DOC_ID;
        goto done;
    }

    if(!easton_read_uint32(&cmd, &cmdlen, &numwkbs)) {
        code = EASTON_ERROR_BAD_NUM_WKBS;
        goto done;
    }

    wkbs = (uint8_t**) malloc(numwkbs * sizeof(uint8_t*));
    if(wkbs == NULL) {
        code = EASTON_ERROR_BAD_ALLOC;
        goto done;
    }

    wkblens = (uint32_t*) malloc(numwkbs * sizeof(uint32_t));
    if(wkblens == NULL) {
        code = EASTON_ERROR_BAD_ALLOC;
        goto done;
    }

    for(i = 0; i < numwkbs; i++) {
        if(!easton_read_binary(&cmd, &cmdlen, (const void**) wkb, &wkblen)) {
            code = EASTON_ERROR_BAD_WKB;
            goto done;
        }
        wkbs[i] = wkb;
        wkblens[i] = wkblen;
    }

    if(easton_index_update(idx, docid, docidlen, numwkbs, wkbs, wkblens)) {
        easton_send_ok(NULL, 0);
    } else {
        easton_send_error(NULL, 0);
    }

done:
    if(wkbs != NULL) {
        free(wkbs);
    }

    if(wkblens != NULL) {
        free(wkblens);
    }

    if(code != EASTON_OK) {
        exit(code);
    }
}


static void
delete_rtree(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint8_t* docid;
    uint32_t docidlen;

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &docid, &docidlen)) {
        exit(EASTON_ERROR_BAD_DOC_ID);
    }

    if(easton_index_delete(idx, docid, docidlen)) {
        easton_send_ok(NULL, 0);
    } else {
        easton_send_error(NULL, 0);
    }
}


static void
query_rtree(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    GEOSGeometry* query = NULL;
    easton_geom_filt_t* filtfun;

    uint8_t* wkb;
    uint32_t wkblen;
    uint8_t filter;
    uint8_t nearest;
    uint64_t lim;
    uint64_t off;
    std::vector<bytes> docids;
    std::vector<bytes> wkbs;
    uint8_t* buf = NULL;
    uint8_t* pos = NULL;
    uint64_t buflen;
    uint64_t i;

    if(!easton_read_binary(&cmd, &cmdlen, (const void**) &wkb, &wkblen)) {
        exit(EASTON_ERROR_BAD_WKB);
    }

    if(!easton_read_uint8(&cmd, &cmdlen, &filter)) {
        exit(EASTON_ERROR_BAD_FILTER);
    }

    if(!easton_read_uint8(&cmd, &cmdlen, &nearest)) {
        exit(EASTON_ERROR_BAD_NEAREST);
    }

    if(!easton_read_uint64(&cmd, &cmdlen, &lim)) {
        exit(EASTON_ERROR_BAD_LIMIT);
    }

    if(!easton_read_uint64(&cmd, &cmdlen, &off)) {
        exit(EASTON_ERROR_BAD_OFFSET);
    }

    query = easton_geom_from_wkb(idx, wkb, wkblen);
    if(query == NULL) {
        easton_send_error(NULL, 0);
        goto done;
    }

    filtfun = easton_geom_get_filter(idx, filter);
    if(filtfun == NULL) {
        easton_send_error(NULL, 0);
        goto done;
    }

    if(!easton_index_query(idx, query, filt, nearest, lim, off, docids, wkbs)) {
        goto done;
    }

    buflen = sizeof(uint64_t);
    for(i = 0; i < num_results; i++) {
        buflen += sizeof(uint32_t) + docidlens[i];
        buflen += sizeof(uint32_t) + wkblens[i];
    }

    buf = (uint8_t*) malloc(buflen);
    pos = buf;

    easton_write_uint64(pos, num_results);
    pos += sizeof(uint64_t);

    for(i = 0; i < num_results; i++) {
        easton_write_binary(buf, &(docids[i][0]), docids[i].size());
        easton_write_binary(buf, &(wkbs[i][0]), wkbs[i].size());
    }

    easton_send_ok(buf, buflen);

done:

    if(query != NULL) {
        GEOSGeom_destroy_r(idx->geos_ctx, query);
    }

    if(docids != NULL) {
        free(docids);
    }

    if(docidlens != NULL) {
        free(docidlens);
    }

    if(wkbs != NULL) {
        free(wkbs);
    }

    if(wkblens != NULL) {
        free(wkblens);
    }

    if(buf != NULL) {
        free(buff);
    }

    // Do *not* free the pos pointer.
}


void
easton_handle_command(easton_idx_t* idx, const uint8_t* cmd, uint32_t cmdlen)
{
    uint16_t op;

    if(!easton_read_uint16(&cmd, &cmdlen, &op)) {
        exit(EASTON_ERROR_BAD_COMMAND);
    }

    switch(op) {
        case EASTON_COMMAND_CLOSE:
            close_idx(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_FLUSH:
            flush_idx(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_GET_DOC_ID_NUM:
            get_doc_id_num(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_GET_DOC_COUNT:
            get_doc_count(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_PUT_USER_KV:
            put_user_kv(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_GET_USER_KV:
            get_user_kv(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_DEL_USER_KV:
            del_user_kv(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_UPDATE_ENTRIES:
            update_rtree(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_DELETE_ENTRIES:
            delete_rtree(idx, cmd, cmdlen);
            break;
        case EASTON_COMMAND_QUERY:
            query_rtree(idx, cmd, cmdlen);
            break;
        default:
            exit(EASTON_ERROR_BAD_COMMAND);
    }
}
