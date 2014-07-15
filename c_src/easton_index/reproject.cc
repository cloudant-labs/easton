
#include <CsMap/cs_map.h>
#include <CsMap/csNameMapperSupport.h>


#include "exceptions.hh"
#include "geo.hh"
#include "reproject.hh"


NS_EASTON_BEGIN
NS_EASTON_GEO_BEGIN


typedef GEOSGeometry* (GeomFromCS) (GEOSCtx, GEOSCoordSequence*);

class Rep
{
    public:
        Rep(GEOSCtx c, int src_srid, int tgt_srid);

        GEOSGeometry* run(const GEOSGeometry* g);

        GEOSGeometry* point(const GEOSGeometry* g);
        GEOSGeometry* linestring(const GEOSGeometry* g);
        GEOSGeometry* linearring(const GEOSGeometry* g);
        GEOSGeometry* polygon(const GEOSGeometry* g);
        GEOSGeometry* collection(const GEOSGeometry* g);

        GEOSGeometry* reproject(const GEOSGeometry* g, GeomFromCS gfcs);

    private:
        GEOSCtx c;
        int src_srid;
        int tgt_srid;
        const char* src_name;
        const char* tgt_name;
};


Rep::Rep(GEOSCtx c, int src_srid, int tgt_srid)
{
    this->c = c;
    this->src_srid = src_srid;
    this->tgt_srid = tgt_srid;
    this->src_name = CSepsg2adskCS(this->src_srid);
    this->tgt_name = CSepsg2adskCS(this->tgt_srid);
}


GEOSGeometry*
Rep::run(const GEOSGeometry* g)
{
    switch(GEOSGeomTypeId_r(this->c, g)) {
        case GEOS_POINT:
            return this->point(g);
        case GEOS_LINESTRING:
            return this->linestring(g);
        case GEOS_LINEARRING:
            return this->linearring(g);
        case GEOS_POLYGON:
            return this->polygon(g);
        case GEOS_MULTIPOINT:
        case GEOS_MULTILINESTRING:
        case GEOS_MULTIPOLYGON:
        case GEOS_GEOMETRYCOLLECTION:
            return this->collection(g);
        default:
            return NULL;
    }
}


GEOSGeometry*
Rep::point(const GEOSGeometry* g)
{
    return this->reproject(g, GEOSGeom_createPoint_r);
}


GEOSGeometry*
Rep::linestring(const GEOSGeometry* g)
{
    return this->reproject(g, GEOSGeom_createLineString_r);
}


GEOSGeometry*
Rep::linearring(const GEOSGeometry* g)
{
    return this->reproject(g, GEOSGeom_createLinearRing_r);
}


GEOSGeometry*
Rep::polygon(const GEOSGeometry* g)
{
    GEOSGeometry* er = NULL;
    GEOSGeometry** rings = NULL;
    GEOSGeometry* p = NULL;
    int nrings = GEOSGetNumInteriorRings_r(this->c, g);
    int i;

    if(nrings < 0) {
        goto error;
    }

    er = this->run(GEOSGetExteriorRing_r(this->c, g));
    if(!er) {
        throw EastonException("Error reprojecting exterior ring.");
    }

    rings = new GEOSGeometry*[nrings];
    for(i = 0; i < nrings; i++) {
        rings[i] = NULL;
    }

    for(i = 0; i < nrings; i++) {
        const GEOSGeometry* r = GEOSGetInteriorRingN_r(this->c, g, i);
        if(r == NULL) {
            goto error;
        }
        rings[i] = this->run(r);
        if(rings[i] == NULL) {
            goto error;
        }
    }

    p = GEOSGeom_createPolygon_r(this->c, er, rings, nrings);
    if(p == NULL) {
        goto error;
    }

    delete [] rings;
    return p;

error:
    if(er != NULL) {
        GEOSGeom_destroy_r(this->c, er);
    }

    if(rings != NULL) {
        for(int i = 0; i < nrings; i++) {
            if(rings[i] != NULL) {
                GEOSGeom_destroy_r(this->c, rings[i]);
            }
        }

        delete [] rings;
    }

    return NULL;
}


GEOSGeometry*
Rep::collection(const GEOSGeometry* g)
{
    GEOSGeometry** geoms = NULL;
    GEOSGeometry* r = NULL;

    int type = GEOSGeomTypeId_r(this->c, g);

    int ngeoms = GEOSGetNumGeometries_r(this->c, g);
    if(ngeoms < 0) {
        goto error;
    }

    geoms = new GEOSGeometry*[ngeoms];
    for(int i = 0; i < ngeoms; i++) {
        geoms[i] = NULL;
    }

    for(int i = 0; i < ngeoms; i++) {
        const GEOSGeometry* subg = GEOSGetGeometryN_r(this->c, g, i);
        if(subg == NULL) {
            goto error;
        }
        geoms[i] = this->run(subg);
        if(geoms[i] == NULL) {
            goto error;
        }
    }

    r = GEOSGeom_createCollection_r(this->c, type, geoms, ngeoms);
    if(r == NULL) {
        goto error;
    }

    delete [] geoms;
    return r;

error:
    if(geoms != NULL) {
        for(int i = 0; i < ngeoms; i++) {
            if(geoms[i] != NULL) {
                GEOSGeom_destroy_r(this->c, geoms[i]);
            }
        }

        delete [] geoms;
    }

    return NULL;
}


GEOSGeometry*
Rep::reproject(const GEOSGeometry* g, GeomFromCS gfcs)
{
    const GEOSCoordSequence* src;
    GEOSCoordSequence* tgt = NULL;
    GEOSGeometry* ret = NULL;
    double xyzm[4] = {0.0, 0.0, 0.0, 0.0};
    uint32_t dims;

    src = GEOSGeom_getCoordSeq_r(this->c, g);
    if(src == NULL) {
        goto error;
    }

    tgt = GEOSCoordSeq_clone_r(this->c, src);
    if(tgt == NULL) {
        goto error;
    }

    uint32_t len;
    if(GEOSCoordSeq_getSize_r(this->c, src, &len) == 0) {
        goto error;
    }

    for(uint32_t i = 0; i < len; i++) {
        if(GEOSCoordSeq_getDimensions_r(this->c, src, &dims) == 0) {
            goto error;
        }

        for(uint32_t j = 0; j < dims; j++) {
            if(GEOSCoordSeq_getOrdinate_r(this->c, src, i, j, &xyzm[j]) == 0) {
                goto error;
            }
        }

        if(!geo::reproject(this->src_name, this->tgt_name, xyzm)) {
            goto error;
        }

        for(uint32_t j = 0; j < dims; j++) {
            if(GEOSCoordSeq_setOrdinate_r(this->c, tgt, i, j, xyzm[j]) == 0) {
                goto error;
            }
        }
    }

    ret = gfcs(this->c, tgt);
    if(ret == NULL) {
        goto error;
    }

    return ret;

error:
    if(tgt != NULL) {
        GEOSCoordSeq_destroy_r(this->c, tgt);
    }

    return NULL;
}


GEOSGeometry*
reproject(GEOSCtx c, const GEOSGeometry* g, int src_srid, int tgt_srid)
{
    return Rep(c, src_srid, tgt_srid).run(g);
}


bool
reproject(int src_srid, int tgt_srid, double xyzm[4])
{
    return reproject(CSepsg2adskCS(src_srid), CSepsg2adskCS(tgt_srid), xyzm);
}


bool
reproject(int src_srid, const char* tgt_name, double xyzm[4])
{
    return reproject(CSepsg2adskCS(src_srid), tgt_name, xyzm);
}


bool
reproject(const char* src_name, int32_t tgt_srid, double xyzm[4])
{
    return reproject(src_name, CSepsg2adskCS(tgt_srid), xyzm);
}


bool
reproject(const char* src_name, const char* tgt_name, double xyzm[4])
{
    if(src_name == NULL) {
        throw EastonException("Invalid src_srid.");
    }

    if(tgt_name == NULL) {
        throw EastonException("Invalid tgt_srid.");
    }

    int slen = strlen(src_name);
    int tlen = strlen(tgt_name);

    if(slen == tlen && memcmp(src_name, tgt_name, slen) == 0) {
        return true;
    }

    if(CS_cnvrt(src_name, tgt_name, xyzm) != 0) {
        return false;
    }

    return true;
}


NS_EASTON_GEO_END
NS_EASTON_END
