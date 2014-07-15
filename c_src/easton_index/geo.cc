
#include <geos_c.h>
#include <spatialindex/capi/sidx_api.h>

#include "config.hh"
#include "exceptions.hh"
#include "geo.hh"


#define MAXBUFLEN 1024


NS_EASTON_BEGIN
NS_EASTON_GEO_BEGIN

using namespace easton;


static void
geos_notice(const char* fmt, ...) {
    return;
}


static void
geos_error(const char* fmt, ...) {
    char buf[MAXBUFLEN];
    va_list ap;

    va_start(ap, fmt);
    vsnprintf(buf, MAXBUFLEN, fmt, ap);
    va_end(ap);

    throw GeoException(buf);
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
    this->data[dim] = val;
}


void
Bounds::set_max(uint32_t dim, double val)
{
    this->data[this->dims + dim] = val;
}


void
Bounds::update_min(uint32_t dim, double val)
{
    if(val < this->data[dim]) {
        this->data[dim] = val;
    }
}


void
Bounds::update_max(uint32_t dim, double val)
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


std::string
Geom::to_wkt()
{
    char* data = GEOSGeomToWKT_r(this->ctx->ctx, this->ro_g);
    std::string ret(data);
    free(data);
    return ret;
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

    uint32_t ncoords;
    switch(env->get_type()) {
        case GEOS_POINT:
            coords = GEOSGeom_getCoordSeq_r(this->ctx->ctx, env->ro_g);
            ncoords = 1;
            break;
        case GEOS_POLYGON:
            ring = env->get_exterior_ring();
            ncoords = (uint32_t) GEOSGetNumCoordinates_r(
                    this->ctx->ctx, ring->ro_g);
            coords = ring->get_coords();
            break;
        default:
            throw EastonException("Invalid envelope type in get_bounds.");
    }

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

            ret->update_min(j, v);
            ret->update_max(j, v);
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
    this->ro_g = rw_g;
}


GeomRW::~GeomRW()
{
    this->ctx->destroy(this->rw_g);
}


void
GeomRW::reproject(int src_srid, int tgt_srid)
{

}


PrepGeom::PrepGeom(Ctx::Ptr ctx, Geom::Ptr base)
{
    this->ctx = ctx;
    this->base = base;
    this->prep = GEOSPrepare_r(this->ctx->ctx, base->ro_g);
    if(!prep) {
        throw std::bad_alloc();
    }
}


PrepGeom::~PrepGeom()
{
    GEOSPreparedGeom_destroy_r(this->ctx->ctx, this->prep);
}


bool
PrepGeom::contains(Geom::Ptr subj)
{
    if(GEOSPreparedContains_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::contains_properly(Geom::Ptr subj)
{
    if(GEOSPreparedContainsProperly_r(
            this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::covered_by(Geom::Ptr subj)
{
    if(GEOSPreparedCovers_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::covers(Geom::Ptr subj)
{
    if(GEOSPreparedCovers_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::crosses(Geom::Ptr subj)
{
    if(GEOSPreparedCrosses_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::disjoint(Geom::Ptr subj)
{
    if(GEOSPreparedDisjoint_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::intersects(Geom::Ptr subj)
{
    if(GEOSPreparedIntersects_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::overlaps(Geom::Ptr subj)
{
    if(GEOSPreparedOverlaps_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::touches(Geom::Ptr subj)
{
    if(GEOSPreparedTouches_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


bool
PrepGeom::within(Geom::Ptr subj)
{
    if(GEOSPreparedWithin_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
        return true;
    }

    return false;
}


GeomFilter::GeomFilter(Ctx::Ptr ctx, Geom::Ptr g, uint64_t filter)
{
    this->pg = PrepGeom::Ptr(new PrepGeom(ctx, g));
    this->filter = filter;
}


GeomFilter::~GeomFilter()
{
}


bool
GeomFilter::operator()(Geom::Ptr other)
{
    // Theoretically I could do this with a function pointer
    // instead of running the switch for each invocation. I
    // tried it. It was a lot more complicated than I wanted
    // so I reverted to the this more straightforward method
    // in the interest of not making non-C++ reviewers' eyes
    // bleed.
    switch(this->filter) {
        case EASTON_FILTER_NONE:
            return true;
        case EASTON_FILTER_CONTAINS:
            return this->pg->contains(other);
        case EASTON_FILTER_CONTAINS_PROPERLY:
            return this->pg->contains_properly(other);
        case EASTON_FILTER_COVERED_BY:
            return this->pg->covered_by(other);
        case EASTON_FILTER_COVERS:
            return this->pg->covers(other);
        case EASTON_FILTER_CROSSES:
            return this->pg->crosses(other);
        case EASTON_FILTER_DISJOINT:
            return this->pg->disjoint(other);
        case EASTON_FILTER_INTERSECTS:
            return this->pg->intersects(other);
        case EASTON_FILTER_OVERLAPS:
            return this->pg->overlaps(other);
        case EASTON_FILTER_TOUCHES:
            return this->pg->touches(other);
        case EASTON_FILTER_WITHIN:
            return this->pg->within(other);
        default:
            throw EastonException("Invalid filter function requested.");
    }
}


Ctx::Ptr
Ctx::create(uint32_t dimensions, int32_t srid)
{
    return Ptr(new Ctx(dimensions, srid));
}


Ctx::Ctx(uint32_t dimensions, int32_t srid)
{
    this->dimensions = dimensions;
    this->srid = srid;
    this->ctx = initGEOS_r(geos_notice, geos_error);
    if(this->ctx == NULL) {
        throw std::bad_alloc();
    }
}


int32_t
Ctx::get_srid()
{
    return this->srid;
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


GeomFilter
Ctx::make_filter(Geom::Ptr geom, uint64_t filter)
{
    return GeomFilter(this->shared_from_this(), geom, filter);
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


NS_EASTON_GEO_END
NS_EASTON_END
