
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "config.hh"
#include "geom.hh"
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


static
uint8_t*
get_docid_key(const uint8_t* docid, uint32_t docidlen, uint32_t* dockeylen)
{
    TCXSTR* tmp;
    uint8_t* ret;

    tmp = tcxstrnew2("docid:");
    tcxstrcat(tmp, docid, (int) docidlen);
    *dockeylen = (uint32_t) tcxstrsize(tmp);
    ret = (uint8_t*) tcmemdup(tcxstrptr(tmp), *dockeylen);

    tcxstrdel(tmp);
    return ret;
}


static void
init_id_idx(easton_idx_t* idx)
{
    int32_t flags = HDBOWRITER | HDBOCREAT;
    uint8_t* val;
    const uint8_t* vcopy;
    uint32_t len;

    idx->id_idx = tchdbnew();
    if(!tchdbopen(idx->id_idx, (char*) idx->id_idx_file, flags)) {
        exit(EASTON_ERROR_BAD_ID_IDX_INIT);
    }

    val = (uint8_t*) tchdbget(idx->id_idx,
            DOC_ID_NUM_KEY, strlen(DOC_ID_NUM_KEY), (int*) &len);

    if(val == NULL) {
        idx->doc_id_num = 0;
        return;
    }

    vcopy = val;
    if(!easton_read_uint64(&vcopy, &len, &(idx->doc_id_num))) {
        exit(EASTON_ERROR_BAD_ID_IDX_INIT);
    }

    free(val);
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

    // Setting the limit and offset below to UINT64_MAX is so
    // that we can do our own paging. Underneat the covers
    // libspatialindex is loading all results into RAM and then
    // has a post-processing step. Rather than futz around trying
    // to page things propery we'll just disable its paging
    // and use our own.

    if(IndexProperty_SetResultSetLimit(props, UINT64_MAX) != RT_None) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    idx->geo_idx = Index_Create(props);
    if(idx->geo_idx == NULL) {
        exit(EASTON_ERROR_BAD_GEO_IDX_INIT);
    }

    if(!Index_IsValid(idx->geo_idx)) {
        exit(EASTON_ERROR_CORRUPT_GEO_IDX);
    }
    
    Index_SetResultSetOffset(idx->geo_idx, UINT64_MAX);

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


static uint64_t
get_doc_num(easton_idx_t* idx, uint8_t* dockey, uint32_t dockeylen)
{
    uint8_t* val;
    const uint8_t* vcopy;
    uint32_t len;
    uint64_t ret;

    val = (uint8_t*) tchdbget(idx->id_idx, dockey, dockeylen, (int*) &len);
    if(val == NULL) {
        ret = idx->doc_id_num++;
    } else {
        vcopy = val;
        if(!easton_read_uint64(&vcopy, &len, &ret)) {
            exit(EASTON_ERROR_BAD_DOC_ID_VAL);
        }
        free(val);
    }

    // Write the update doc_id_num back to id_idx
    val = (uint8_t*) malloc(sizeof(uint64_t));
    if(val == NULL) {
        exit(EASTON_ERROR_BAD_ALLOC);
    }

    easton_write_uint64(val, ret);

    if(!tchdbput(idx->id_idx, DOC_ID_NUM_KEY, strlen(DOC_ID_NUM_KEY),
            val, sizeof(uint64_t))) {
        exit(EASTON_ERROR_BAD_DOC_NUM_INC);
    }

    free(val);

    return ret;
}


static double***
allocate_bounds(easton_idx_t* idx, uint32_t num)
{
    // This allocation strategy is a bit fancy to avoid
    // the need to perform a possibly large number of
    // allocations. The basic idea is to allocate the
    // number of each value and then arrange our pointers
    // after the fact.

    double* values = NULL;
    double** arrays = NULL;
    double*** bounds = NULL;
    uint32_t i;

    values = (double*) malloc(num * 2 * idx->dimensions * sizeof(double));
    if(values == NULL) {
        goto error;
    }
    memset(values, 0, num * 2 * idx->dimensions * sizeof(double));

    arrays = (double**) malloc(num * 2 * sizeof(double**));
    if(arrays == NULL) {
        goto error;
    }

    bounds = (double***) malloc(num * sizeof(double***));
    if(bounds == NULL) {
        goto error;
    }

    // Attach the arrays to our values
    for(i = 0; i < (num*2); i++) {
        arrays[i] = values + (i * idx->dimensions);
    }

    // Attach arrays into the bounds
    for(i = 0; i < num; i++) {
        bounds[i] = arrays + (i * 2);
    }

    return bounds;

error:
    if(values != NULL) {
        free(values);
    }

    if(arrays != NULL) {
        free(arrays);
    }

    return NULL;
}


static void
free_bounds(double*** bounds)
{
    // See allocate_bounds for the fancy.

    free(bounds[0][0]);
    free(bounds[0]);
    free(bounds);
}


static uint8_t*
make_id_idx_value(uint64_t docnum, uint64_t dims, uint32_t numwkbs,
        double*** bounds, uint32_t* retlen)
{
    uint32_t total_size = 8 + 4 + (numwkbs * 2 * dims * sizeof(double));
    uint8_t* val = (uint8_t*) malloc(total_size);
    uint32_t offset;
    uint32_t i;
    uint32_t j;
    uint32_t k;

    if(val == NULL) {
        return NULL;
    }

    easton_write_uint64(val, docnum);
    easton_write_uint32(val + 8, numwkbs);

    for(i = 0; i < numwkbs; i++) {
        for(j = 0; j < 2; j++) {
            for(k = 0; k < dims; k++) {
                offset = 12 + i * j * k * sizeof(double);
                easton_write_double(val + offset, bounds[i][j][k]);
            }
        }
    }

    *retlen = total_size;
    return val;
}


static uint8_t*
make_geo_idx_value(uint8_t* docid, uint32_t docidlen,
        uint8_t* wkb, uint32_t wkblen, uint32_t* retlen)
{
    uint32_t total_size = 4 + 4 + docidlen + wkblen;
    uint8_t* val = (uint8_t*) malloc(total_size);

    if(val == NULL) {
        return NULL;
    }

    easton_write_uint32(val, docidlen);
    memcpy(val + 4, docid, docidlen);

    easton_write_uint32(val + 4 + docidlen, wkblen);
    memcpy(val + 4 + docidlen + 4, wkb, wkblen);

    *retlen = total_size;
    return val;
}


bool
item_to_docid_and_wkb(ItemH* item, bytes& docid, bytes& wkb)
{
    uint8_t* data;
    uint32_t len;
    
    if(IndexItem_GetData(item, (uint8_t **) &data, &len) != RT_None) {
        return false;
    }
    
    if(!easton_read_binary(&data, &len, docid)) {
        return false;
    }
    
    if(!easton_read_binary(&data, &len, bytes)) {
        return false;
    }
    
    return true;
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


bool
easton_index_update(easton_idx_t* idx,
        uint8_t* docid, uint32_t docidlen,
        uint32_t numwkbs, uint8_t** wkbs, uint32_t* wkblens)
{
    uint8_t* dockey = NULL;
    uint32_t dockeylen;
    uint64_t docnum;
    double*** bounds = NULL;
    uint8_t* val = NULL;
    uint32_t vallen;
    uint32_t i;
    bool ret = false;

    dockey = get_docid_key(docid, docidlen, &dockeylen);
    if(dockey == NULL) {
        goto done;
    }

    docnum = get_doc_num(idx, dockey, dockeylen);

    bounds = allocate_bounds(idx, numwkbs);
    if(bounds == NULL) {
        goto done;
    }

    for(i = 0; i < numwkbs; i++) {
        if(!easton_geom_get_bounds(idx, wkbs[i], wkblens[i], bounds[i])) {
            goto done;
        }
    }

    val = make_id_idx_value(docnum, idx->dimensions, numwkbs, bounds, &vallen);
    if(!tchdbput(idx->id_idx, dockey, dockeylen, val, vallen)) {
        goto done;
    }
    free(val);
    val = NULL;

    for(i = 0; i < numwkbs; i++) {
        val = make_geo_idx_value(docid, docidlen, wkbs[i], wkblens[i], &vallen);
        if(val == NULL) {
            goto done;
        }

        if(Index_InsertData(idx->geo_idx, docnum, bounds[i][0], bounds[i][1],
                idx->dimensions, val, vallen) != RT_None) {
            goto done;
        }

        free(val);
        val = NULL;
    }

    ret = true;

done:
    if(dockey != NULL) {
        free(dockey);
    }

    if(bounds != NULL) {
        free_bounds(bounds);
    }

    if(val != NULL) {
        free(val);
    }

    return ret;
}


bool
easton_index_delete(easton_idx_t* idx, uint8_t* docid, uint32_t docidlen)
{
    uint8_t* dockey = NULL;
    uint32_t dockeylen;
    uint8_t* val = NULL;
    uint32_t vallen;
    const uint8_t* vcopy;
    uint64_t docnum;
    uint32_t numwkbs;
    double*** bounds = NULL;
    uint32_t i;
    uint32_t j;
    uint32_t k;
    bool ret = false;

    dockey = get_docid_key(docid, docidlen, &dockeylen);
    if(dockey == NULL) {
        goto done;
    }

    val = (uint8_t*) tchdbget(idx->id_idx, dockey, dockeylen, (int*) &vallen);
    if(val == NULL) {
        // Should this be a no-op or a failure? Currently
        // its a failure.
        goto done;
    }

    // Read functions mutate the pointer so use a copy
    vcopy = val;

    if(!easton_read_uint64(&vcopy, &vallen, &docnum)) {
        goto done;
    }

    if(!easton_read_uint32(&vcopy, &vallen, &numwkbs)) {
        goto done;
    }

    bounds = allocate_bounds(idx, numwkbs);
    if(bounds == NULL) {
        goto done;
    }

    for(i = 0; i < numwkbs; i++) {
        for(j = 0; j < 2; j++) {
            for(k = 0; k < idx->dimensions; k++) {
                if(!easton_read_double(&vcopy, &vallen, &(bounds[i][j][k]))) {
                    goto done;
                }
            }
        }
    }

    for(i = 0; i < numwkbs; i++) {
        if(Index_DeleteData(idx->geo_idx, docnum, bounds[i][0], bounds[i][1],
                idx->dimensions) != RT_None) {
            goto done;
        }
    }

    if(!tchdbout(idx->id_idx, dockey, dockeylen)) {
        goto done;
    }

    ret = true;

done:
    if(dockey != NULL) {
        free(dockey);
    }

    if(val != NULL) {
        free(val);
    }

    // Don't free vcopy since its a copy of val

    if(bounds != NULL) {
        free_bounds(bounds);
    }

    return ret;
}


bool
easton_index_intersects(easton_idx_t* idx,
        GEOSGeometry* geom,
        easton_geom_filt_t* filt,
        bool nearest,
        uint64_t limit,
        uint64_t offset,
        std::vector<bytes>& docids,
        std::vector<bytes>& wkbs)
{
    GEOSPreparedGeometry* pg = NULL;
    GEOSGeometry* item_geom = NULL;
    double*** bounds = NULL;
    IndexItemH* items = NULL;
    uint64_t num_items;
    bytes docid;
    bytes wkb;
    bool ret = false;
    
    docids.clear();
    wkbs.clear();
    
    pg = GEOSPrepare_r(geom);
    if(pg == NULL) {
        goto done;
    }
    
    bounds = allocate_bounds(idx, 1);
    if(!easton_geom_get_bounds(idx, geom, bounds[0])) {
        goto done;
    }
    
    if(Index_Intersects_obj(idx->geo_idx, bounds[0][0], bounds[0][1],
            idx->dimensions, &items, &num_items) != RT_None) {
        goto done;
    }
    
    // Filter out entries 
    count = 0;
    for(i = 0; i < num_items && count < offset; i++) {
        if(!item_to_docid_and_wkb(items[i], docid, wkb)) {
            goto done;
        }
        
        item_geom = easton_geom_from_wkb(idx, &(wkb[0]), wkb.size());
        if(item_geom == NULL) {
            goto done;
        }
        
        if((*filt)(idx->geos_ctx, pq, item_geom) == 1) {
            count += 1;
        }
        
        GEOSGeometry_destroy_r(idx->geos_ctx, item_geom);
        item_geom = NULL;
    }

    // Notice we're not resetting i to 0
    // before iterating of items.
    for(; i < num_items && docids.size() < limit; i++) {
        if(!item_to_docid_and_wkb(items[i], docid, wkb)) {
            goto done;
        }
        
        item_geom = easton_geom_from_wkb(idx, &(wkb[0]), wkb.size());
        if(item_geom == NULL) {
            goto done;
        }
        
        if((*filt)(idx->geos_ctx, pq, item_geom) == 1) {
            docids.push_back(docid);
            wkbs.push_back(wkb);
        }
        
        GEOSGeometry_destroy_r(idx->geos_ctx, item_geom);
        item_geom = NULL;
    }

    return true;
    
done:
    if(pg != NULL) {
        GEOSPreparedGeometry_destroy_r(pg);
    }

    if(item_geom != NULL) {
        GEOSGeometry_destroy_r(item_geom);
    }

    if(bounds != NULL) {
        free_bounds(bounds);
    }
    
    if(items != NULL) {
        Index_DestroyObjResults(items, num_results);
    }
    
    return ret;
}
