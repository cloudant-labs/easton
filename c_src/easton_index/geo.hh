
#ifndef EASTON_GEOM_HH
#define EASTON_GEOM_HH


// Prevent the misuse of non thread-safe GEOS functions
#define GEOS_USE_ONLY_R_API


#include <geos_c.h>

#include "easton.hh"
#include "geo.hh"
#include "io.hh"

using namespace easton;


NS_EASTON_BEGIN
NS_EASTON_GEO_BEGIN


typedef GEOSContextHandle_t GEOSCtx;


class Ctx;
class Geom;
class GeomRO;
class GeomRW;
class PrepGeom;
class GeomFilter;


class Bounds
{
    public:
        typedef std::shared_ptr<Bounds> Ptr;
        typedef std::vector<Ptr> Vector;
        typedef std::vector<Ptr>::iterator VIter;
        typedef std::unique_ptr<double[]> DimsPtr;

        static Ptr create(uint32_t dims);
        ~Bounds();

        void set_min(uint32_t dim, double val);
        void set_max(uint32_t dim, double val);

        void update_min(uint32_t dim, double val);
        void update_max(uint32_t dim, double val);

        uint32_t get_dims();

        double* mins();
        double* maxs();

    private:
        Bounds();
        Bounds(uint32_t dims);
        Bounds(const Bounds& other);

        DimsPtr data;
        uint32_t dims;
};


class Geom
{
    public:
        typedef std::shared_ptr<Geom> Ptr;

        virtual ~Geom() = 0;

        std::string to_wkt();
        io::Bytes::Ptr to_wkb();

        int get_type();

        bool is_valid();
        bool is_empty();
        bool is_ring();
        bool is_closed();
        bool has_z();

        Ptr get_envelope();
        Ptr get_exterior_ring();

        Ptr reproject(int32_t src_srid, int32_t tgt_srid);

        Bounds::Ptr get_bounds();

        const GEOSCoordSequence* get_coords();

    protected:
        std::shared_ptr<Ctx> ctx;
        const GEOSGeometry* ro_g;

        friend class Ctx;
        friend class PrepGeom;
};


class GeomRO: public Geom
{
    public:
        typedef std::shared_ptr<GeomRO> Ptr;

        virtual ~GeomRO();

    private:
        GeomRO();
        GeomRO(std::shared_ptr<Ctx> ctx, const GEOSGeometry* g);
        GeomRO(const GeomRO& other);

        friend class Ctx;
};


class GeomRW: public Geom
{
    public:
        typedef std::shared_ptr<GeomRW> Ptr;

        virtual ~GeomRW();

    private:
        GeomRW();
        GeomRW(std::shared_ptr<Ctx> ctx, GEOSGeometry* g);
        GeomRW(const GeomRW& other);

        GEOSGeometry* rw_g;

        friend class Ctx;
};


class PrepGeom
{
    public:
        typedef std::shared_ptr<PrepGeom> Ptr;

        ~PrepGeom();

        bool contains(Geom::Ptr subj);
        bool contains_properly(Geom::Ptr subj);
        bool covered_by(Geom::Ptr subj);
        bool covers(Geom::Ptr subj);
        bool crosses(Geom::Ptr subj);
        bool disjoint(Geom::Ptr subj);
        bool intersects(Geom::Ptr subj);
        bool overlaps(Geom::Ptr subj);
        bool touches(Geom::Ptr subj);
        bool within(Geom::Ptr subj);

    private:
        PrepGeom();
        PrepGeom(std::shared_ptr<Ctx> ctx, Geom::Ptr base);
        PrepGeom(const PrepGeom& other);

        std::shared_ptr<Ctx> ctx;
        Geom::Ptr base;
        const GEOSPreparedGeometry* prep;

        friend class Ctx;
        friend class GeomFilter;
};


class GeomFilter
{
    public:
        GeomFilter(std::shared_ptr<Ctx> ctx, Geom::Ptr g, uint64_t filter);
        ~GeomFilter();

        bool operator()(Geom::Ptr other);

    private:
        PrepGeom::Ptr pg;
        uint64_t filter;

        friend class Geom;
};


class Ctx: public std::enable_shared_from_this<Ctx>
{
    public:
        typedef std::shared_ptr<Ctx> Ptr;

        static Ptr create(uint32_t dimensions, int32_t srid);
        ~Ctx();

        int32_t get_srid();

        GeomFilter make_filter(Geom::Ptr geom, uint64_t filter);

        Geom::Ptr geom_from_reader(io::Reader::Ptr reader, int32_t srid);

        Geom::Ptr from_wkb(io::Bytes::Ptr wkb, int32_t srid);
        Geom::Ptr from_wkt(io::Bytes::Ptr wkt, int32_t srid);

        Geom::Ptr from_wkb(io::Bytes::Ptr wkb);
        Geom::Ptr from_wkt(io::Bytes::Ptr wkt);

        Geom::Ptr make_rectangle(
                double* mins,
                double* maxs,
                uint32_t dimensions,
                int32_t srid
            );

        Geom::Ptr make_circle(
                double x,
                double y,
                double r,
                int32_t srid
            );

        Geom::Ptr make_ellipse(
                double x,
                double y,
                double x_range,
                double y_range,
                int32_t srid
            );

    private:
        Ctx();
        Ctx(uint32_t dimensions, int32_t srid);
        Ctx(const Ctx& other);

        GeomRO::Ptr wrap(const GEOSGeometry* g);
        GeomRW::Ptr wrap(GEOSGeometry* g);

        void destroy(GEOSGeometry* g);

        Geom::Ptr make_circle_int(
                double x,
                double y,
                double r
            );

        Geom::Ptr make_ellipse_int(
                double x,
                double y,
                double x_range,
                double y_range
            );

        uint32_t dimensions;
        int32_t srid;
        GEOSCtx ctx;

        friend class Geom;
        friend class GeomRO;
        friend class GeomRW;
        friend class PrepGeom;
};


NS_EASTON_GEO_END
NS_EASTON_END


#endif
