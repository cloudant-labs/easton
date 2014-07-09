
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "config.hh"
#include "index.hh"
#include "util.hh"


#define DOC_ID_NUM_KEY "meta:doc_id_num"


static int8_t*
get_base_dir(const int8_t* dirname)
{
    char* ret = tcstrdup(dirname);
    TCXSTR* tmp;

    ret = tcstrtrim(ret);

    if(!tcstrbwm(ret, "/")) {
        tmp = tcxstrnew2(ret);
        tcxstrcat2(tmp, "/");
        free(ret);
        ret = tcstrdup((char*) tcxstrptr(tmp));
        tcxstrdel(tmp);
    }

    return (int8_t*) ret;
}


static int8_t*
get_file_name(const int8_t* base_dir, const int8_t* name)
{
    TCXSTR* tmp;
    char* ret;

    tmp = tcxstrnew2((char*) base_dir);
    tcxstrcat2(tmp, "/");
    tcxstrcat2(tmp, (char*) name);
    ret = tcstrdup((char*) tcxstrptr(tmp));

    tcxstrdel(tmp);
    return (int8_t*) ret;
}


static void
init_id_idx(easton_idx_t* idx)
{
    int32_t flags = HDBOWRITER | HDBOCREAT;
    int doc_id_num;

    idx->id_idx = tchdbnew();
    if(!tchdbopen(idx->id_idx, (char*) idx->id_idx_file, flags)) {
        exit(EASTON_ERROR_BAD_ID_IDX_INIT);
    }

    doc_id_num = tchdbaddint(idx->id_idx,
            DOC_ID_NUM_KEY, strlen(DOC_ID_NUM_KEY), 0);

    if(doc_id_num < 0) {
        exit(EASTON_ERROR_BAD_ID_IDX_INIT);
    }

    idx->doc_id_num = (uint64_t) doc_id_num;
}


static void
init_geo_idx(easton_idx_t* idx, int32_t argc, const int8_t* argv[])
{
    IndexPropertyH props = IndexProperty_Create();

    int64_t idx_type = tcatoi((const char*) argv[2]);
    int64_t dims = tcatoi((const char*) argv[3]);
    int64_t limit = tcatoi((const char*) argv[4]);

    RTIndexType it;

    idx->dimensions = dims;

    if(IndexProperty_SetFileName(props, (char*) idx->geo_idx_file) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(IndexProperty_SetIndexStorage(props, RT_Disk) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(IndexProperty_SetOverwrite(props, 0) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(idx_type == EASTON_INDEX_TYPE_RTREE) {
        it = RT_RTree;
    } else if(idx_type == EASTON_INDEX_TYPE_TPRTREE) {
        it = RT_TPRTree;
    } else {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(IndexProperty_SetIndexType(props, it) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(IndexProperty_SetDimension(props, (uint32_t) dims) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(IndexProperty_SetResultSetLimit(props, limit) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    idx->geo_idx = Index_Create(props);
    if(idx->geo_idx == NULL) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(!Index_IsValid(idx->geo_idx)) {
        exit(EASTON_ERROR_CORRUPT_GEO_IDX);
    }

    IndexProperty_Destroy(props);

    // Verify the properties after open are the
    // same as requested.

    props = Index_GetProperties(idx->geo_idx);

    // For some reason the properties returned from the
    // tree don't set the index type.
    //
    // if(IndexProperty_GetIndexType(props) != it) {
    //     exit(EASTON_ERROR_BAD_GEO_IDX_CFG);
    // }

    if(IndexProperty_GetDimension(props) != idx->dimensions) {
        exit(EASTON_ERROR_BAD_GEO_IDX_CFG);
    }

    IndexProperty_Destroy(props);
}


static void
geos_notice(const char* fmt, ...) {
    return;
}


static void
geos_error(const char* fmt, ...) {
    exit(EASTON_ERROR_GEOS_EXCEPTION);
}


static void
init_geos(easton_idx_t* idx)
{
    idx->geos_ctx = initGEOS_r(geos_notice, geos_error);
    GEOS_setWKBByteOrder_r(idx->geos_ctx, GEOS_WKB_XDR);
}


easton_idx_t*
easton_index_init(int32_t argc, const int8_t* argv[])
{
    easton_idx_t* idx = (easton_idx_t*) malloc(sizeof(easton_idx_t));

    if(idx == NULL) {
        exit(EASTON_ERROR_BAD_ALLOC);
    }

    if(argc < 5) {
        exit(EASTON_ERROR_BAD_ARGS);
    }

    if(!easton_is_dir(argv[1])) {
        exit(EASTON_ERROR_BAD_DIRECTORY);
    }

    idx->base_dir = get_base_dir(argv[1]);
    idx->id_idx_file = get_file_name(idx->base_dir,
            (int8_t*) EASTON_FILE_ID_IDX);
    idx->geo_idx_file = get_file_name(idx->base_dir,
            (int8_t*) EASTON_FILE_GEO_IDX);

    init_id_idx(idx);
    init_geo_idx(idx, argc, argv);
    init_geos(idx);

    return idx;
}


bool
easton_index_close(easton_idx_t* idx)
{
    Index_Destroy(idx->geo_idx);
    finishGEOS_r(idx->geos_ctx);

    if(!tchdbclose(idx->id_idx)) {
        return false;
    }

    tchdbdel(idx->id_idx);
    free(idx->base_dir);
    free(idx->id_idx_file);
    free(idx->geo_idx_file);
    free(idx);

    return true;
}


bool
easton_index_flush(easton_idx_t* idx)
{
    Index_Flush(idx->geo_idx);

    if(!tchdbsync(idx->id_idx)) {
        return false;
    }

    return true;
}


uint64_t
easton_index_get_doc_id_num(easton_idx_t* idx)
{
    return idx->doc_id_num;
}


uint64_t
easton_index_get_doc_count(easton_idx_t* idx)
{
    double* mins = NULL;
    double* maxs = NULL;
    uint32_t dims;
    uint64_t ret = UINT64_MAX;
    uint64_t n;

    if(Index_GetBounds(idx->geo_idx, &mins, &maxs, &dims) != RT_None) {
        goto done;
    }

    if(Index_Intersects_count(idx->geo_idx, mins, maxs, dims, &n) != RT_None) {
        goto done;
    }

    ret = n;

done:

    if(mins != NULL) {
        free(mins);
    }

    if(maxs != NULL) {
        free(maxs);
    }

    return ret;
}


bool
easton_index_put_kv(easton_idx_t* idx,
        void* key, uint32_t klen, void* val, uint32_t vlen)
{
    return tchdbput(idx->id_idx, key, (int32_t) klen, val, (int32_t) vlen);
}


uint8_t*
easton_index_get_kv(easton_idx_t* idx, void* key, uint32_t klen, uint32_t* vlen)
{
    uint8_t* val;
    int32_t len;

    len = tchdbvsiz(idx->id_idx, key, (int32_t) klen);
    if(len < 0) {
        return NULL;
    }

    *vlen = (uint32_t) len;
    val = (uint8_t*) malloc((uint32_t) *vlen);

    if(tchdbget3(idx->id_idx, key, (int32_t) klen, val, *vlen) < 0) {
        free(val);
        return NULL;
    }

    return val;
}


bool
easton_index_del_kv(easton_idx_t* idx, void* key, uint32_t klen)
{
    return tchdbout(idx->id_idx, key, (int32_t) klen);
}
