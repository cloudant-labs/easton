
#include <geos_c.h>
#include <spatialindex/capi/sidx_api.h>

#include "config.hh"
#include "exceptions.hh"
#include "geo.hh"


NS_EASTON_BEGIN
NS_EASTON_GEO_BEGIN

using namespace easton;


static void
geos_notice(const char* fmt, ...) {
    return;
}


static void
geos_error(const char* fmt, ...) {
    throw EastonExit(EASTON_ERROR_GEOS_EXCEPTION);
}


Bounds::Ptr
Bounds::create(uint32_t dims)
{
    return Bounds::Ptr(new Bounds(dims));
}


Bounds::Bounds(uint32_t dims)
{
    this->dims = dims;

    // Single array, mins in the first half,
    // maxs in the second.

    this->data = DimsPtr(new double[2 * this->dims]);
}


Bounds::~Bounds()
{
}


void
Bounds::set_min(uint32_t dim, double val)
{
    if(val < this->data[dim]) {
        this->data[dim] = val;
    }
}


void
Bounds::set_max(uint32_t dim, double val)
{
    if(val > this->data[this->dims + dim]) {
        this->data[this->dims + dim] = val;
    }
}


double*
Bounds::mins()
{
    return this->data.get();
}


double*
Bounds::maxs()
{
    return this->data.get() + this->dims;
}


Util::Ptr
Util::create()
{
    return Util::Ptr(new Util());
}


Util::Util()
{
    this->ctx = initGEOS_r(geos_notice, geos_error);
    if(this->ctx == NULL) {
        throw std::bad_alloc();
    }
}


Util::~Util()
{
    finishGEOS_r(this->ctx);
}


Bounds::Ptr
Util::get_bounds(io::Bytes::Ptr wkb, uint32_t dimensions)
{
    GEOSGeometry* geom = NULL;
	GEOSGeometry* env = NULL;
	const GEOSGeometry* ring;
	const GEOSCoordSequence* coords = NULL;
    int type;
    uint32_t ncoords;
    uint32_t dims;
    double v;
    Bounds::Ptr ret;

    geom = this->from_wkb(wkb);
    if(geom == NULL) {
        goto done;
    }

    if(GEOSisValid_r(this->ctx, geom) != 1) {
        goto done;
    }

    env = GEOSEnvelope_r(this->ctx, geom);
    if(env == NULL) {
        goto done;
    }

    type = GEOSGeomTypeId_r(this->ctx, env);

    switch(type) {
        case GEOS_POINT:
            coords = GEOSGeom_getCoordSeq_r(this->ctx, env);
            ncoords = 1;
            break;
        case GEOS_POLYGON:
            // To get a coordinate sequence we need a simple
            // line which we can get by just requesting the
            // outside of the envelope. For this case its a
            // bit simple but Polygons could technically have
            // holes even though we should have a rectangle
            // here.
            ring = GEOSGetExteriorRing_r(this->ctx, env);
            coords = GEOSGeom_getCoordSeq_r(this->ctx, ring);
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
    if(!GEOSCoordSeq_getDimensions_r(this->ctx, coords, &dims)) {
        goto done;
    }

    if(dims != dimensions) {
        goto done;
    }

    ret = Bounds::create(dims);

    // Initialize our bounds based on the first
    // point in the sequence. This allows us to avoid
    // having to use sentinel values.
    for(uint32_t j = 0; j < dims; j++) {
        if(!GEOSCoordSeq_getOrdinate_r(this->ctx, coords, 0, j, &v)) {
            ret.reset();
            goto done;
        }

        ret->set_min(j, v);
        ret->set_max(j, v);
    }

    // Notice that we skip comparisons on the first
    // coordinate here.
    for(uint32_t i = 1; i < ncoords; i++) {
        for(uint32_t j = 0; j < dims; j++) {
            if(!GEOSCoordSeq_getOrdinate_r(this->ctx, coords, i, j, &v)) {
                ret.reset();
                goto done;
            }

            ret->set_min(j, v);
            ret->set_max(j, v);
        }
    }

done:
    if(geom != NULL) {
        GEOSGeom_destroy_r(this->ctx, geom);
    }

    if(env != NULL) {
        GEOSGeom_destroy_r(this->ctx, env);
    }

    // From my understanding ring and coords
    // are just borrowed references so we must
    // not free them.

    if(!ret) {
        throw EastonException("Unable to get bounds for shape.");
    }

    return ret;
}


GEOSGeometry*
Util::from_wkb(io::Bytes::Ptr wkb)
{
    GEOSWKBReader* reader;
    GEOSGeometry* geom;

    reader = GEOSWKBReader_create_r(this->ctx);
    if(reader == NULL) {
        return NULL;
    }

    geom = GEOSWKBReader_read_r(this->ctx, reader, wkb->get(), wkb->size());

    GEOSWKBReader_destroy_r(this->ctx, reader);

    return geom;
}

/*
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
*/

NS_EASTON_GEO_END
NS_EASTON_END
