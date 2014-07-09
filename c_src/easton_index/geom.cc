
#include <geos_c.h>

#include "geom.hh"

bool
easton_geom_get_bounds(easton_idx_t* idx,
        uint8_t* wkb, uint32_t wkblen, double** bounds)
{
    GEOSGeometry* geom = NULL;
	GEOSGeometry* env = NULL;
	const GEOSGeometry* ring;
	const GEOSCoordSequence* coords = NULL;
    int type;
    uint32_t ncoords;
    uint32_t dims;
    uint32_t i;
    uint32_t j;
    double v;
    bool ret = false;

    geom = GEOSGeomFromWKB_buf_r(idx->geos_ctx, wkb, wkblen);
    if(geom == NULL) {
        goto done;
    }

    if(GEOSisValid_r(idx->geos_ctx, geom) != 1) {
        goto done;
    }

    if(GEOSisValid_r(idx->geos_ctx, geom) != 0) {
        goto done;
    }

    env = GEOSEnvelope_r(idx->geos_ctx, geom);
    if(env == NULL) {
        goto done;
    }

    type = GEOSGeomTypeId_r(idx->geos_ctx, env);

    switch(type) {
        case GEOS_POINT:
            coords = GEOSGeom_getCoordSeq_r(idx->geos_ctx, env);
            ncoords = 1;
            break;
        case GEOS_POLYGON:
            // To get a coordinate sequence we need a simple
            // line which we can get by just requesting the
            // outside of the envelope. For this case its a
            // bit simple but Polygons could technically have
            // holes even though we should have a rectangle
            // here.
            ring = GEOSGetExteriorRing_r(idx->geos_ctx, env);
            coords = GEOSGeom_getCoordSeq_r(idx->geos_ctx, ring);
            ncoords = (uint32_t) GEOSGetNumCoordinates(ring);
            break;
        default:
            // By my reading the envelope is guaranteed to
            // be a point, polygon, or empty. Theoretically
            // empty should've been filtered out above so it
            // should be Point or Polygon here. Regardless
            // I'm not sure enough to assert on it.
            goto done;
    }

    // Do some sanity checking on our dimensionality
    if(!GEOSCoordSeq_getDimensions_r(idx->geos_ctx, coords, &dims)) {
        goto done;
    }

    if(dims != idx->dimensions) {
        goto done;
    }

    // Initialize our bounds based on the first
    // point in the sequence. This allows us to avoid
    // having to use sentinel values.
    for(j = 0; j < dims; j++) {
        if(!GEOSCoordSeq_getOrdinate_r(idx->geos_ctx, coords, 0, j, &v)) {
            goto done;
        }
        bounds[0][j] = v;
        bounds[1][j] = v;
    }

    // Notice that we skip comparisons on the first
    // coordinate here.
    for(i = 1; i < ncoords; i++) {
        for(j = 0; j < dims; j++) {
            if(!GEOSCoordSeq_getOrdinate_r(idx->geos_ctx, coords, i, j, &v)) {
                goto done;
            }

            if(v < bounds[0][j]) {
                bounds[0][j] = v;
            }

            if(v > bounds[1][j]) {
                bounds[1][j] = v;
            }
        }
    }

    ret = true;

done:
    if(geom != NULL) {
        GEOSGeom_destroy_r(idx->geos_ctx, geom);
    }

    if(env != NULL) {
        GEOSGeom_destroy_r(idx->geos_ctx, env);
    }

    // From my understanding ring and coords
    // are just borrowed references so we must
    // not free them.

    return ret;
}
