
#ifndef EASTON_GEOM_HH
#define EASTON_GEOM_HH

#include <geos_c.h>

#include "index.hh"

typedef char (*easton_geom_filter_t)
        (GEOSContextHandle_t, const GEOSPreparedGeometry*, const GEOSGeometry*);


GEOSGeometry* easton_geom_from_wkb(easton_idx_t* idx,
        uint8_t* wkb, uint32_t wkb);

GEOSGeometry* easton_geom_from_item(easton_idx_t* idx, ItemH* item);

bool easton_geom_get_bounds(easton_idx_t* idx,
        uint8_t* wkb, uint32_t wkblen, double** bounds);

easton_geom_filter_t* easton_gem_get_filter(uint8_t filter);

#endif
