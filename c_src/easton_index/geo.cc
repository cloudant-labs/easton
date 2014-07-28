
#include <string>
#include <sstream>

#include <geos/geom/Coordinate.h>
#include <geos/geom/GeometryFactory.h>
#include <geos/geom/Polygon.h>
#include <geos/geom/PrecisionModel.h>
#include <geos/io/WKBWriter.h>
#include <geos/util/GeometricShapeFactory.h>
#include <geos_c.h>

#include <spatialindex/capi/sidx_api.h>

#include <CsMap/cs_map.h>
#include <CsMap/csNameMapperSupport.h>

#include "config.hh"
#include "epsg.hh"
#include "exceptions.hh"
#include "geo.hh"
#include "reproject.hh"


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


Bounds::Ptr
Bounds::read(io::Reader::Ptr reader)
{
    int32_t points;
    if(!reader->read_list(points)) {
        throw EastonException("Error reading bounds");
    }

    if(points != 4 && points != 6 && points != 8) {
        throw EastonException("Invalid bounds dimensions.");
    }

    uint32_t dims = (uint32_t) (points / 2);
    Bounds::Ptr ret = Bounds::create(dims);

    double v;
    for(int32_t i = 0; i < dims; i++) {
        if(!reader->read(v)) {
            throw EastonException("Invalid bounds min value.");
        }
        ret->set_min(i, v);
    }

    for(int32_t i = 0; i < dims; i++) {
        if(!reader->read(v)) {
            throw EastonException("Invalid bounds max value.");
        }
        ret->set_max(i, v);
    }

    if(!reader->read_empty_list()) {
        throw EastonException("Improper bounds list.");
    }

    return ret;
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
Bounds::expand(Bounds::Ptr other)
{
    if(!other) {
        throw EastonException("Invalid bounds for expansion.");
    }

    if(other->dims != this->dims) {
        throw EastonException("Invalid dimensions for bounds expansion.");
    }

    for(uint32_t i = 0; i < this->dims; i++) {
        this->update_min(i, other->mins()[i]);
        this->update_max(i, other->maxs()[i]);
    }
}


void
Bounds::write(io::Writer::Ptr writer)
{
    writer->start_list(this->dims * 2);
    for(uint32_t i = 0; i < dims * 2; i++) {
        writer->write(this->data[i]);
    }
    writer->write_empty_list();
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


SRID::Ptr
SRID::LL()
{
    return Ptr(new SRID("LL"));
}


SRID::Ptr
SRID::from_reader(io::Reader::Ptr reader)
{
    std::string maybe_default;
    if(reader->read(maybe_default)) {
        if(maybe_default == "default") {
            return NULL;
        }
    }

    if(!reader->read_tuple_n(2)) {
        throw EastonException("Invalid SRID defintion.");
    }

    std::string type;
    if(!reader->read(type)) {
        throw EastonException("Invalid SRID type.");
    }

    const char* name = NULL;

    if(type == "epsg") {
        uint64_t code;
        if(!reader->read(code)) {
            throw EastonException("Invalid EPSG code type.");
        }

        // Try using our custom EPSG table before
        // falling back to the CsMap implementation.
        // This saves us about 1.5s on the first access
        // and 0.3ms every time after.
        name = easton_epsg_lookup(code);

        if(name == NULL) {
            name = CSepsg2adskCS((long) code);
        }

        if(name == NULL) {
            throw EastonException("Invalid EPSG code.");
        }
    }

    if(name == NULL) {
        throw EastonException("Unknown SRID type: " + type);
    }

    return Ptr(new SRID(name));
}


SRID::SRID(const char* name) : name(name)
{
}


std::string
SRID::str()
{
    return this->name;
}


const char*
SRID::c_str()
{
    return this->name.c_str();
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


io::Bytes::Ptr
Geom::to_wkb()
{
    uint8_t* wkb;
    uint32_t wkblen;
    io::Bytes::Ptr ret;

    GEOSWKBWriter* writer = GEOSWKBWriter_create_r(this->ctx->ctx);
    GEOSWKBWriter_setOutputDimension_r(this->ctx->ctx, writer,
            this->ctx->dimensions);
    wkb = GEOSWKBWriter_write_r(this->ctx->ctx,
            writer, this->ro_g, (unsigned long*) &wkblen);

    if(wkb) {
        ret = io::Bytes::copy(wkb, wkblen);
    }

    GEOSFree_r(this->ctx->ctx, wkb);
    GEOSWKBWriter_destroy_r(this->ctx->ctx, writer);

    return ret;
}


uint32_t
Geom::get_dims()
{
    int dims = GEOSGeom_getDimensions_r(this->ctx->ctx, this->ro_g);
    return (uint32_t) dims;
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


double
Geom::distance(Ptr other)
{
    double dist;

    Geom::Ptr c1 = this->get_centroid();
    Geom::Ptr c2 = other->get_centroid();

    if(!GEOSDistance_r(this->ctx->ctx, c1->ro_g, c2->ro_g, &dist)) {
        throw EastonException("Error generating distance for geometries.");
    }

    return dist;
}


Geom::Ptr
Geom::get_centroid()
{
    return this->ctx->wrap(GEOSGetCentroid_r(this->ctx->ctx, this->ro_g));
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


Geom::Ptr
Geom::reproject(SRID::Ptr src, SRID::Ptr tgt)
{
    return this->ctx->wrap(geo::reproject(
            this->ctx->ctx,
            this->ro_g,
            src,
            tgt
        ));
}


Bounds::Ptr
Geom::get_bounds()
{
    switch(GEOSGeomTypeId_r(this->ctx->ctx, this->ro_g)) {
        case GEOS_POINT:
            return this->get_bounds_simple();
        case GEOS_LINESTRING:
            return this->get_bounds_simple();
        case GEOS_LINEARRING:
            return this->get_bounds_simple();
        case GEOS_POLYGON:
            return this->get_bounds_polygon();
        case GEOS_MULTIPOINT:
        case GEOS_MULTILINESTRING:
        case GEOS_MULTIPOLYGON:
        case GEOS_GEOMETRYCOLLECTION:
            return this->get_bounds_collection();
        default:
            return NULL;
    }

}


const GEOSCoordSequence*
Geom::get_coords()
{
    return GEOSGeom_getCoordSeq_r(this->ctx->ctx, this->ro_g);
}


Bounds::Ptr
Geom::get_bounds_simple()
{
    const GEOSCoordSequence* seq = this->get_coords();
    if(!seq) {
        return NULL;
    }

    Bounds::Ptr bounds = Bounds::create(this->ctx->dimensions);

    uint32_t len;
    if(GEOSCoordSeq_getSize_r(this->ctx->ctx, seq, &len) == 0) {
        throw EastonException("Error getting geometry coordinate sequence.");
    }

    if(len == 0) {
        throw EastonException("Invalid empty geometry for get_bounds.");
    }

    double c;
    for(uint32_t j = 0; j < this->ctx->dimensions; j++) {
        if(GEOSCoordSeq_getOrdinate_r(this->ctx->ctx, seq, 0, j, &c) == 0) {
            throw EastonException("Error getting coordinate.");
        }
        bounds->set_min(j, c);
        bounds->set_max(j, c);
    }

    for(uint32_t i = 1; i < len; i++) {
        for(uint32_t j = 0; j < this->ctx->dimensions; j++) {
            if(GEOSCoordSeq_getOrdinate_r(this->ctx->ctx, seq, i, j, &c) == 0) {
                throw EastonException("Error getting coordinate.");
            }
            bounds->update_min(j, c);
            bounds->update_max(j, c);
        }
    }

    return bounds;
}


Bounds::Ptr
Geom::get_bounds_polygon()
{
    std::vector<Geom::Ptr> rings = this->get_rings();
    if(rings.size() == 0) {
        throw EastonException("Invalid rings for polygon bounds.");
    }

    Bounds::Ptr bounds = rings.at(0)->get_bounds();
    for(uint32_t i = 1; i < rings.size(); i++) {
        bounds->expand(rings.at(i)->get_bounds());
    }

    return bounds;
}


Bounds::Ptr
Geom::get_bounds_collection()
{
    std::vector<Geom::Ptr> geoms = this->get_geoms();
    if(geoms.size() == 0) {
        throw EastonException("Invalid geoms for collection bounds.");
    }

    Bounds::Ptr bounds = geoms.at(0)->get_bounds();
    for(uint32_t i = 1; i < geoms.size(); i++) {
        bounds->expand(geoms.at(i)->get_bounds());
    }

    return bounds;
}


std::vector<Geom::Ptr>
Geom::get_rings()
{
    std::vector<Geom::Ptr> ret;
    const GEOSGeometry* r;

    int nrings = GEOSGetNumInteriorRings_r(this->ctx->ctx, this->ro_g);
    if(nrings < 0) {
        throw EastonException("Invalid polygon geometry.");
    }

    r = GEOSGetExteriorRing_r(this->ctx->ctx, this->ro_g);
    ret.push_back(this->ctx->wrap(r));

    for(int i = 0; i < nrings; i++) {
        r = GEOSGetInteriorRingN_r(this->ctx->ctx, this->ro_g, i);
        ret.push_back(this->ctx->wrap(r));
    }

    return ret;
}


std::vector<Geom::Ptr>
Geom::get_geoms()
{
    std::vector<Geom::Ptr> ret;
    const GEOSGeometry* r;

    int ngeoms = GEOSGetNumGeometries_r(this->ctx->ctx, this->ro_g);
    if(ngeoms <= 0) {
        throw EastonException("Invalid polygon geometry.");
    }

    for(int i = 0; i < ngeoms; i++) {
        r = GEOSGetGeometryN_r(this->ctx->ctx, this->ro_g, i);
        ret.push_back(this->ctx->wrap(r));
    }

    return ret;
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
    if(GEOSPreparedCoveredBy_r(this->ctx->ctx, this->prep, subj->ro_g) == 1) {
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


GeomFilter::GeomFilter()
{
}


GeomFilter::GeomFilter(Ctx::Ptr ctx, Geom::Ptr g, uint64_t filter)
{
    this->pg = PrepGeom::Ptr(new PrepGeom(ctx, g));
    this->filter = filter;
}


GeomFilter::~GeomFilter()
{
}


double
GeomFilter::distance(Geom::Ptr other)
{
    return this->pg->base->distance(other);
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
Ctx::create(uint32_t dimensions, SRID::Ptr srid)
{
    return Ptr(new Ctx(dimensions, srid));
}


Ctx::Ctx(uint32_t dimensions, SRID::Ptr srid)
{
    this->dimensions = dimensions;
    this->srid = srid;
    this->ctx = initGEOS_r(geos_notice, geos_error);
    if(this->ctx == NULL) {
        throw std::bad_alloc();
    }
}


SRID::Ptr
Ctx::get_srid()
{
    return this->srid;
}


Bounds::Ptr
Ctx::get_zero_bounds()
{
    geo::Bounds::Ptr ret = geo::Bounds::create(this->dimensions);

    for(uint32_t i = 0; i < this->dimensions; i++) {
        ret->set_min(i, 0.0);
        ret->set_max(i, 0.0);
    }

    return ret;
}


Ctx::~Ctx()
{
    finishGEOS_r(this->ctx);
}


GeomFilter
Ctx::make_filter(Geom::Ptr geom, uint64_t filter)
{
    return GeomFilter(this->shared_from_this(), geom, filter);
}


Geom::Ptr
Ctx::geom_from_reader(io::Reader::Ptr reader, SRID::Ptr srid)
{
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Invalid geometry tuple.");
    }

    std::string qtype;
    if(!reader->read(qtype)) {
        throw EastonException("Error decoding geometry type.");
    }

    if(qtype == "wkb") {
        io::Bytes::Ptr wkb = reader->read_bytes();
        if(!wkb) {
            throw EastonException("Invalid geometry data for wkb.");
        }
        return this->from_wkb(wkb, srid);
    }

    if(qtype == "wkt") {
        io::Bytes::Ptr wkt = reader->read_bytes();
        if(!wkt) {
            throw EastonException("Invalid geometry data for wkt");
        }
        return this->from_wkt(wkt, srid);
    }

    if(qtype == "bbox") {
        int32_t arity;
        if(!reader->read_list(arity)) {
            throw EastonException("Invalid coordinate list for bbox.");
        }

        if(arity != 4 && arity != 6 && arity != 8) {
            throw EastonException("Invalid bounding box dimensions.");
        }

        double mins[arity/2];
        double maxs[arity/2];

        for(uint32_t i = 0; i < arity/2; i++) {
            if(!reader->read(mins[i])) {
                throw EastonException("Invalid coordinate in bbox.");
            }
        }

        for(uint32_t i = 0; i < arity/2; i++) {
            if(!reader->read(maxs[i])) {
                throw EastonException("Invalid coordinate in bounding box.");
            }
        }

        // Read past the list tail
        if(!reader->read_empty_list()) {
            throw EastonException("Improper list for bbox coordinates.");
        }


        return this->make_rectangle(mins, maxs, arity/2, srid);
    }

    if(qtype == "circle") {
        if(!reader->read_tuple_n(3)) {
            throw EastonException("Invalid circle definition.");
        }
        double x;
        double y;
        double r;

        if(!reader->read(x)) {
            throw EastonException("Invalid x coordinate for circle.");
        }

        if(!reader->read(y)) {
            throw EastonException("Invalid y coordinate for circle.");
        }

        if(!reader->read(r)) {
            throw EastonException("Invalid radius for circle.");
        }

        return this->make_circle(x, y, r, srid);
    }

    if(qtype == "ellipse") {
        if(!reader->read_tuple_n(4)) {
            throw EastonException("Invalid ellipse definition.");
        }

        double x;
        double y;
        double x_range;
        double y_range;

        if(!reader->read(x)) {
            throw EastonException("Invalid x coordinate for ellipse.");
        }

        if(!reader->read(y)) {
            throw EastonException("Invalid y coordinate for ellipse.");
        }

        if(!reader->read(x_range)) {
            throw EastonException("Invalid x range for ellipse.");
        }

        if(!reader->read(y_range)) {
            throw EastonException("Invalid y range for ellipse.");
        }

        return this->make_ellipse(x, y, x_range, y_range, srid);
    }

    throw EastonException("Unsupported geometry definition type.");
}


Geom::Ptr
Ctx::from_wkb(io::Bytes::Ptr wkb, SRID::Ptr srid)
{
    Geom::Ptr ret = this->from_wkb(wkb);

    if(srid->str() != this->srid->str()) {
        return ret->reproject(srid, this->srid);
    }

    return ret;
}


Geom::Ptr
Ctx::from_wkt(io::Bytes::Ptr wkt, SRID::Ptr srid)
{
    Geom::Ptr ret = this->from_wkt(wkt);

    if(srid->str() != this->srid->str()) {
        return ret->reproject(srid, this->srid);
    }

    return ret;
}


Geom::Ptr
Ctx::from_wkb(io::Bytes::Ptr wkb)
{
    GEOSWKBReader* reader;

    reader = GEOSWKBReader_create_r(this->ctx);
    if(reader == NULL) {
        return NULL;
    }

    Geom::Ptr geom = this->wrap(
            GEOSWKBReader_read_r(this->ctx, reader, wkb->get(), wkb->size())
        );

    GEOSWKBReader_destroy_r(this->ctx, reader);

    return geom;
}


Geom::Ptr
Ctx::from_wkt(io::Bytes::Ptr wkt)
{
    GEOSWKTReader* reader;

    reader = GEOSWKTReader_create_r(this->ctx);
    if(reader == NULL) {
        return NULL;
    }

    Geom::Ptr geom = this->wrap(
            GEOSWKTReader_read_r(this->ctx, reader, (char*) wkt->get())
        );

    GEOSWKTReader_destroy_r(this->ctx, reader);

    return geom;
}


Geom::Ptr
Ctx::make_point(double x, double y, SRID::Ptr srid)
{
    GEOSCoordSequence* cs = GEOSCoordSeq_create_r(
            this->ctx, 1, this->dimensions);

    if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, 0, x)) {
        throw EastonException("Error setting x coordinate for point.");
    }

    if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, 1, y)) {
        throw EastonException("Error setting y coordinate for point.");
    }

    for(uint32_t i = 2; i < this->dimensions; i++) {
        if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, i, 0.0)) {
            throw EastonException("Error setting default point dimension.");
        }
    }

    Geom::Ptr p = this->wrap(GEOSGeom_createPoint_r(this->ctx, cs));
    if(!p) {
        // Only destroy CS if the geometry constructor failed
        // to take ownership.
        GEOSCoordSeq_destroy_r(this->ctx, cs);
        throw EastonException("Error creating geometry for point.");
    }

    if(srid->str() != this->srid->str()) {
        return p->reproject(srid, this->srid);
    }

    return p;
}


Geom::Ptr
Ctx::make_rectangle(double* mins, double* maxs, uint32_t dims, SRID::Ptr srid)
{
    GEOSCoordSequence* cs = GEOSCoordSeq_create_r(
            this->ctx, 2, this->dimensions);

    for(uint32_t i = 0; i < dims; i++)
    {
        if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, i, mins[i])) {
            throw EastonException("Error setting bbox dimension.");
        }
        if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 1, i, maxs[i])) {
            throw EastonException("Error setting bbox dimension.");
        }
    }

    // Set the z and m dimensions to 0.0 if they aren't
    // specified as part of the bounding box.
    for(uint32_t i = dims; i < this->dimensions; i++) {
        if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, i, 0.0)) {
            throw EastonException("Error setting default bbox dimension.");
        }
        if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 1, i, 0.0)) {
            throw EastonException("Error setting default bbox dimension.");
        }
    }

    Geom::Ptr ls = this->wrap(GEOSGeom_createLineString_r(this->ctx, cs));
    if(!ls) {
        // Only destroy CS if the geometry constructor failed
        // to take ownership.
        GEOSCoordSeq_destroy_r(this->ctx, cs);
        throw EastonException("Error creating geometry for bbox.");
    }

    // libgeos isn't very good with >2d support so if we have a
    // bounding box request come in with > 2 dimensions we'll
    // just leave it as a simple linestring between its two
    // extremes. The bounding box logic will give us the correct
    // answer but it can't be used with GeomFilter to get any
    // sort of meaningful result. Instead we're relying on Erlang
    // to reject any queries that specify a bounding box and
    // a filter.

    Geom::Ptr bbox;
    if(this->dimensions == 2) {
        bbox = ls->get_envelope();
    } else {
        bbox = ls;
    }

    if(!bbox) {
        throw EastonException("Eror creating bbox.");
    }

    if(srid->str() != this->srid->str()) {
        return bbox->reproject(srid, this->srid);
    }

    return bbox;
}


Geom::Ptr
Ctx::make_circle(double x, double y, double r, SRID::Ptr srid)
{
    // A circle with a zero-radius is a point. For some
    // reason the regular circle generation returns an
    // empty geometry.

    if(r == 0.0) {
        return this->make_point(x, y, srid);
    }

    // While reading about this I found these [1] PostGIS docs
    // that say this isn't the most efficient method for
    // performing a radius search. I'm not sure how much that
    // has to do with their index types but it might be
    // something to investigate in the future.
    //
    // [1] http://postgis.refractions.net/docs/ST_Buffer.html

    double xyz[3];

    xyz[0] = x;
    xyz[1] = y;
    xyz[2] = 0.0;

    // First we reproject to the index SRID
    if(!reproject(srid, this->srid, xyz)) {
        throw EastonException("Error reprojecting index system.");
    }

    // Then convert to the LL coordinate system. I'm guessing
    // this is to guarantee that we have a known ellipsoid
    // to perform our calculations on.
    if(!reproject(this->srid, SRID::LL(), xyz)) {
        throw EastonException("Error reprojecting to LL system.");
    }

    char ellipsoid[MAXBUFLEN];
    double e_radius;
    double e_exc_sq;

    if(CS_getEllipsoidOf("LL", ellipsoid, MAXBUFLEN) != 0) {
        throw EastonException("Error getting the LL ellipsoid.");
    }

    // This gets the equatorial radius and excentricity squared
    // values for the ellipsoid. Radius should be straight forward.
    // The excentricity squared just tells us how oval-y the
    // ellipsoid is.
    if(CS_getElValues(ellipsoid, &e_radius, &e_exc_sq) != 0) {
        throw EastonException("Error getting the LL ellipsoid parameters.");
    }

    // This calculates the coordinates of a point at the
    // given azimuth (90.0, degrees east of north, whatever
    // that means) and a distance (r) in units that match
    // e_radius. Basically, we're figuring out how far away
    // a point on the circumference would be accounting for the
    // ellipsoidal nature of things.
    double azdd_xyz[3];
    if(CS_azddll(e_radius, e_exc_sq, xyz, 90.0, r, azdd_xyz) != 0) {
        throw EastonException("Error calculating the LL XYZ point.");
    }

    // Now convert both of our coordinates back to the
    // index's srid so we can create the circle.
    if(!reproject(SRID::LL(), this->srid, xyz)) {
        throw EastonException("Error reprojecting circle center from LL.");
    }

    if(!reproject(SRID::LL(), this->srid, azdd_xyz)) {
        throw EastonException("Error reprojecting circumference point from LL");
    }

    // Our final radius is the difference in X coordinates
    // because we used 90.0 degrees in the CS_azddll
    // calculation.
    double final_radius = fabs(xyz[0] - azdd_xyz[0]);

    return this->make_circle_int(xyz[0], xyz[1], final_radius);
}

Geom::Ptr
Ctx::make_circle_int(double x, double y, double radius)
{
    // Create a circle by defining the center point
    // and then adding a defined buffer distance using
    // the requested radius.
    GEOSBufferParams* bp = NULL;
    GEOSCoordSequence* cs = NULL;
    GEOSGeometry* pt = NULL;
    GEOSGeometry* ret = NULL;

    bp = GEOSBufferParams_create_r(this->ctx);
    if(bp == NULL) {
        goto done;
    }

    cs = GEOSCoordSeq_create_r(this->ctx, 1, this->dimensions);
    if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, 0, x)) {
        goto done;
    }

    if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, 1, y)) {
        goto done;
    }

    // Fill in the z and m dimensions if they exist
    for(uint32_t i = 2; i < this->dimensions; i++) {
        if(!GEOSCoordSeq_setOrdinate_r(this->ctx, cs, 0, i, 0.0)) {
            goto done;
        }
    }

    pt = GEOSGeom_createPoint_r(this->ctx, cs);
    if(pt == NULL) {
        goto done;
    }

    // cs is now owned by pt.
    cs = NULL;

    ret = GEOSBufferWithParams_r(this->ctx, pt, bp, radius);

done:
    if(bp != NULL) {
        GEOSBufferParams_destroy_r(this->ctx, bp);
    }

    if(cs != NULL) {
        GEOSCoordSeq_destroy_r(this->ctx, cs);
    }

    if(pt != NULL) {
        GEOSGeom_destroy_r(this->ctx, pt);
    }

    return this->wrap(ret);
}


Geom::Ptr
Ctx::make_ellipse(double x, double y, double x_range, double y_range,
        SRID::Ptr srid)
{
    // An ellipse with zero length axes is a point.
    if(x_range == 0.0 && y_range == 0.0) {
        return this->make_point(x, y, srid);
    }

    // For more explanation on what this function is doing,
    // see the definition of make_circle above that includes
    // the srid. The only difference here is that we calculate
    // two distances for the X and Y directions.
    //
    // TODO: Ask Norman if we can't just re-implement make_circle
    // by passing radius as both x_range and y_range. It seems like
    // it'd be more accurate in terms of the ellipsoidal calcualtions
    // but perhaps the geometry generation isn't as good here.

    double xyz[3];

    xyz[0] = x;
    xyz[1] = y;
    xyz[2] = 0.0;

    if(!reproject(srid, this->srid, xyz)) {
        throw EastonException("Error reprojecting index system.");
    }

    if(!reproject(this->srid, SRID::LL(), xyz)) {
        throw EastonException("Error reprojecting to LL system.");
    }

    char ellipsoid[MAXBUFLEN];
    double e_radius;
    double e_exc_sq;

    if(CS_getEllipsoidOf("LL", ellipsoid, MAXBUFLEN) != 0) {
        throw EastonException("Error getting the LL ellipsoid.");
    }

    if(CS_getElValues(ellipsoid, &e_radius, &e_exc_sq) != 0) {
        throw EastonException("Error getting the LL ellipsoid parameters.");
    }

    double azdd_xyz_x[3];
    if(CS_azddll(e_radius, e_exc_sq, xyz, 90.0, x_range, azdd_xyz_x) != 0) {
        throw EastonException("Error calculating the LL XYZ point.");
    }

    double azdd_xyz_y[3];
    if(CS_azddll(e_radius, e_exc_sq, xyz, 0.0, y_range, azdd_xyz_y) != 0) {
        throw EastonException("Error calculating the LL XYZ point.");
    }

    if(!reproject(SRID::LL(), this->srid, xyz)) {
        throw EastonException("Error reprojecting ellipse center from LL.");
    }

    if(!reproject(SRID::LL(), this->srid, azdd_xyz_x)) {
        throw EastonException("Error reprojecting x_range point from LL");
    }

    if(!reproject(SRID::LL(), this->srid, azdd_xyz_y)) {
        throw EastonException("Error reprojecting y_range point from LL.");
    }

    x_range = fabs(xyz[0] - azdd_xyz_x[0]);
    y_range = fabs(xyz[1] - azdd_xyz_y[1]);

    return this->make_ellipse_int(xyz[0], xyz[1], x_range, y_range);
}


Geom::Ptr
Ctx::make_ellipse_int(double x, double y, double x_range, double y_range)
{
    geos::geom::PrecisionModel pm;
    geos::geom::GeometryFactory gf(&pm);
    geos::util::GeometricShapeFactory sf(&gf);

    sf.setCentre(geos::geom::Coordinate(x, y));
    sf.setWidth(x_range * 2.0);
    sf.setHeight(y_range * 2.0);

    std::unique_ptr<geos::geom::Polygon> p(sf.createCircle());
    if(!p) {
        return NULL;
    }

    geos::io::WKBWriter writer;
    writer.setOutputDimension(this->dimensions);
    std::stringstream ss;
    writer.write(*p.get(), ss);
    std::string wkb_str = ss.str();

    io::Bytes::Ptr wkb = io::Bytes::proxy(
            (uint8_t*) wkb_str.data(), wkb_str.size());
    return this->from_wkb(wkb);
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
