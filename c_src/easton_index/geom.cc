

#include <geos_c.h>
#include <spatialindex/capi/sidx_api.h>

#include "geom.hh"
#include "util.hh"


GEOSGeometry*
easton_geom_from_wkb(easton_idx_t* idx, uint8_t* wkb, uint32_t wkb)
{
    GEOSWKBReader* reader;
    GEOSGeometry* geom;
    
    reader = GEOSWKBReader_create_r(idx->geos_ctx);
    if(reader == NULL) {
        return NULL;
    }
    
    geom = GEOSWKBReader_read_r(idx->geos_ctx, reader, wkb, wkblen);

    GEOSWKBReader_destroy_r(idx->geos_ctx, reader);
    
    return geom;
}


GEOSGeometry*
easton_geom_from_item(easton_idx_t* idx, ItemH* item)
{
    uint8_t* data;
    uint32_t len;
    uint8_t* wkb;
    uint32_t wkblen;

    if(IndexItem_GetData(item, (uint8_t **) &data, &len) != RT_None) {
        return NULL;
    }
    
    // Read and discard docid
    if(!easton_read_binary(&data, &len, &wkb, &wkblen)) {
        return NULL;
    }
    
    // Read the WKB
    if(!easton_read_binary(&data, &len, &wkb, &wkblen)) {
        return NULL;
    }
    
    return easton_geom_from_wkb(idx, wkb, wkblen);
}


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

    geom = easton_geom_from_wkb(idx, wkb, wkblen);
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


char
null_filter(GEOSContextHandle_t ctx,
        const GEOSPreparedGeometry* pg, const GEOSGeometry* g)
{
    return 1;
}


easton_geom_filter_t*
easton_gem_get_filter(uint8_t filter)
{
    easton_geom_filter_t* f = NULL;

    switch(filter) {
        case EASTON_FILTER_NONE:
            return &null_filter;
        case EASTON_FILTER_CONTAINS:
            return &GEOSPreparedContains_r;
        case EASTON_FILTER_CONTAINS_PROPERLY:
            return &GEOSPreparedContainsProperly_r;
        case EASTON_FILTER_COVERED_BY:
            return &GEOSPreparedCoveredBy_r;
        case EASTON_FILTER_COVERS:
            return &GEOSPreparedCovers_r;
        case EASTON_FILTER_CROSSES:
            return &GEOSPreparedCrosses_r;
        case EASTON_FILTER_DISJOINT:
            return &GEOSPreparedDisjoint_r;
        case EASTON_FILTER_INTERSECTS:
            return &GEOSPreparedIntersects_r;
        case EASTON_FILTER_OVERLAPS:
            return &GEOSPreparedOverlaps_r;
        case EASTON_FILTER_TOUCHES:
            return &GEOSPreparedTouches_r;
        case EASTON_FILTER_WITHIN:
            return &GEOSPreparedWithin_r;
        default:
            return NULL;
    }
}
