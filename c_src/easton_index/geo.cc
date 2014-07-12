
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


uint32_t
Bounds::get_dims()
{
    return this->dims;
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


Geom::~Geom()
{
}


int
Geom::get_type()
{
    return GEOSGeomTypeId_r(this->ctx->ctx, this->ro_g);
}


bool
Geom::is_valid()
{
    if(GEOSisValid_r(this->ctx->ctx, this->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
Geom::is_empty()
{
    if(GEOSisEmpty_r(this->ctx->ctx, this->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
Geom::is_ring()
{
    if(GEOSisRing_r(this->ctx->ctx, this->ro_g)) {
        return true;
    }

    return false;
}


bool
Geom::is_closed()
{
    if(GEOSisClosed_r(this->ctx->ctx, this->ro_g)) {
        return true;
    }

    return false;
}


bool
Geom::has_z()
{
    if(GEOSHasZ_r(this->ctx->ctx, this->ro_g)) {
        return true;
    }

    return false;
}


Geom::Ptr
Geom::get_envelope()
{
    GEOSGeometry* raw_env = GEOSEnvelope_r(this->ctx->ctx, this->ro_g);
    Geom::Ptr env = this->ctx->wrap(raw_env);

    if(!env) {
        return NULL;
    }

    if(env->is_empty()) {
        return NULL;
    }

    switch(env->get_type()) {
        case GEOS_POINT:
            return env;
        case GEOS_POLYGON:
            return env;
        default:
            return NULL;
    }
}


Geom::Ptr
Geom::get_exterior_ring()
{
    return this->ctx->wrap(GEOSGetExteriorRing_r(this->ctx->ctx, this->ro_g));
}


Bounds::Ptr
Geom::get_bounds()
{
    Geom::Ptr env = this->get_envelope();
    if(!env) {
        return NULL;
    }

    // We have to be careful with ring here to make
    // sure it stays alive as long as we have a
    // handle to its coordinate sequence.
    Geom::Ptr ring;
    const GEOSCoordSequence* coords;

    switch(env->get_type()) {
        case GEOS_POINT:
            coords = GEOSGeom_getCoordSeq_r(this->ctx->ctx, env->ro_g);
            break;
        case GEOS_POLYGON:
            ring = env->get_exterior_ring();
            coords = ring->get_coords();
            break;
        default:
            throw EastonException("Invalid envelope type in get_bounds.");
    }

    uint32_t ncoords = (uint32_t) GEOSGetNumCoordinates_r(
            this->ctx->ctx, ring->ro_g);

    uint32_t dims;
    if(!GEOSCoordSeq_getDimensions_r(this->ctx->ctx, coords, &dims)) {
        return NULL;
    }

    Bounds::Ptr ret = Bounds::create(dims);

    // Initialize our bounds based on the first
    // point in the sequence. This allows us to avoid
    // having to use sentinel values.
    double v;
    for(uint32_t j = 0; j < dims; j++) {
        if(!GEOSCoordSeq_getOrdinate_r(this->ctx->ctx, coords, 0, j, &v)) {
            return NULL;
        }

        ret->set_min(j, v);
        ret->set_max(j, v);
    }

    // Notice that we skip comparisons on the first
    // coordinate here.
    for(uint32_t i = 1; i < ncoords; i++) {
        for(uint32_t j = 0; j < dims; j++) {
            if(!GEOSCoordSeq_getOrdinate_r(this->ctx->ctx, coords, i, j, &v)) {
                return NULL;
            }

            ret->set_min(j, v);
            ret->set_max(j, v);
        }
    }

    return ret;
}


const GEOSCoordSequence*
Geom::get_coords()
{
    return GEOSGeom_getCoordSeq_r(this->ctx->ctx, this->ro_g);
}


GeomRO::GeomRO(Ctx::Ptr ctx, const GEOSGeometry* ro_g)
{
    this->ctx = ctx;
    this->ro_g = ro_g;
}


GeomRO::~GeomRO()
{
    // Don't attempt to delete the const ro_g pointer
    // as that's libgeos's flag for read-only.
}


GeomRW::GeomRW(Ctx::Ptr ctx, GEOSGeometry* rw_g)
{
    this->ctx = ctx;
    this->rw_g = rw_g;

    // Set ro_g so that all our derived functions will
    // operate on this instance.
    this->ro_g = ro_g;
}


GeomRW::~GeomRW()
{
    this->ctx->destroy(this->rw_g);
}


void
GeomRW::reproject(int src_srid, int tgt_srid)
{

}


Ctx::Ptr
Ctx::create()
{
    return Ptr(new Ctx());
}


Ctx::Ctx()
{
    this->ctx = initGEOS_r(geos_notice, geos_error);
    if(this->ctx == NULL) {
        throw std::bad_alloc();
    }
}


Ctx::~Ctx()
{
    finishGEOS_r(this->ctx);
}


Geom::Ptr
Ctx::from_wkb(io::Bytes::Ptr wkb)
{
    GEOSWKBReader* reader;
    GEOSGeometry* geom;

    reader = GEOSWKBReader_create_r(this->ctx);
    if(reader == NULL) {
        return NULL;
    }

    geom = GEOSWKBReader_read_r(this->ctx, reader, wkb->get(), wkb->size());

    GEOSWKBReader_destroy_r(this->ctx, reader);

    return this->wrap(geom);
}


GeomRO::Ptr
Ctx::wrap(const GEOSGeometry* g)
{
    if(!g) {
        return NULL;
    }

    return GeomRO::Ptr(new GeomRO(this->shared_from_this(), g));
}


GeomRW::Ptr
Ctx::wrap(GEOSGeometry* g)
{
    if(!g) {
        return NULL;
    }

    return GeomRW::Ptr(new GeomRW(this->shared_from_this(), g));
}


void
Ctx::destroy(GEOSGeometry* g)
{
    GEOSGeom_destroy_r(this->ctx, g);
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
