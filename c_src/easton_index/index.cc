
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "config.hh"
#include "index.hh"
#include "util.hh"


static char*
get_base_dir(const char* dirname)
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

    return ret;
}


static char*
get_file_name(const char* base_dir, const char* name)
{
    TCXSTR* tmp;
    char* ret;

    tmp = tcxstrnew2(base_dir);
    tcxstrcat2(tmp, "/");
    tcxstrcat2(tmp, name);
    ret = tcstrdup((char*) tcxstrptr(tmp));

    tcxstrdel(tmp);
    return ret;
}


static void
init_id_idx(easton_idx_t* idx)
{
    int flags = HDBOWRITER | HDBOCREAT;

    idx->id_idx = tchdbnew();
    if(!tchdbopen(idx->id_idx, idx->id_idx_file, flags)) {
        exit(EASTON_ERROR_BAD_ID_IDX_INIT);
    }

    // Get current doc id number from hash or
    // set it to 0 in both places.
}


static void
init_geo_idx(easton_idx_t* idx, int argc, const char* argv[])
{
    IndexPropertyH props = IndexProperty_Create();

    int64_t idx_type = tcatoi(argv[2]);
    int64_t dims = tcatoi(argv[3]);
    int64_t limit = tcatoi(argv[4]);

    RTIndexType it;

    idx->dimensions = dims;

    if(IndexProperty_SetFileName(props, idx->geo_idx_file) != RT_None) {
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

    if(IndexProperty_SetDimension(props, (unsigned int) dims) != RT_None) {
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
}


static void
geos_notice(const char *fmt, ...) {
    return;
}


static void
geos_error(const char *fmt, ...) {
    exit(EASTON_ERROR_GEOS_EXCEPTION);
}


static void
init_geos(easton_idx_t* idx)
{
    idx->geos_ctx = initGEOS_r(geos_notice, geos_error);
    GEOS_setWKBByteOrder_r(idx->geos_ctx, GEOS_WKB_XDR);
}


easton_idx_t*
easton_index_init(int argc, const char* argv[])
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
    idx->id_idx_file = get_file_name(idx->base_dir, EASTON_FILE_ID_IDX);
    idx->geo_idx_file = get_file_name(idx->base_dir, EASTON_FILE_GEO_IDX);

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
