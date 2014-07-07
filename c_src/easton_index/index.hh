
#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


// Prevent the misuse of non thread-safe GEOS functions
#define GEOS_USE_ONLY_R_API


#include <tchdb.h>
#include <spatialindex/capi/sidx_api.h>
#include <geos_c.h>


typedef struct {
    int8_t*                 base_dir;
    int8_t*                 id_idx_file;
    int8_t*                 geo_idx_file;

    TCHDB*                  id_idx;
    IndexH                  geo_idx;
    GEOSContextHandle_t     geos_ctx;

    uint64_t                 dimensions;
    uint32_t                 id_num;
} easton_idx_t;


easton_idx_t* easton_index_init(int32_t argc, const int8_t* argv[]);
bool easton_index_close(easton_idx_t* idx);
bool easton_index_flush(easton_idx_t* idx);

bool easton_index_put_kv(easton_idx_t* idx,
        void* key, uint32_t klen, void* val, uint32_t vlen);
uint8_t* easton_index_get_kv(easton_idx_t* idx,
        void* key, uint32_t klen, uint32_t* vlen);
bool easton_index_del_kv(easton_idx_t* idx, void* key, uint32_t klen);



#endif
