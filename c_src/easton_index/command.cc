
#include <stdlib.h>

#include "command.hh"
#include "config.hh"
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
        default:
            exit(EASTON_ERROR_BAD_COMMAND);
    }
}
