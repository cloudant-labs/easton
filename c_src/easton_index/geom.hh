
#ifndef EASTON_GEOM_HH
#define EASTON_GEOM_HH

#include "index.hh"

bool easton_geom_get_bounds(easton_idx_t* idx,
        uint8_t* wkb, uint32_t wkblen, double** bounds);


#endif
