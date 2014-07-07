
#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


// Prevent the misuse of non thread-safe GEOS functions
#define GEOS_USE_ONLY_R_API


#include <tchdb.h>
#include <spatialindex/capi/sidx_api.h>
#include <geos_c.h>


typedef struct {
    char*                   base_dir;
    char*                   id_idx_file;
    char*                   geo_idx_file;

    TCHDB*                  id_idx;
    IndexH                  geo_idx;
    GEOSContextHandle_t     geos_ctx;

    int64_t                 dimensions;
    int64_t                 id_num;
} easton_idx_t;


easton_idx_t* easton_index_init(int argc, const char* argv[]);
bool easton_index_close(easton_idx_t* idx);
bool easton_index_flush(easton_idx_t* idx);

bool easton_index_put_kv(easton_idx_t* idx,
        void* key, size_t klen, void* val, size_t vlen);
char* easton_index_get_kv(easton_idx_t* idx,
        void* key, size_t klen, size_t* vlen);
bool easton_index_del_kv(easton_idx_t* idx, void* key, size_t klen);



#endif
