
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


class Ctx;
class Geom;
class GeomRO;
class GeomRW;


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

        int get_type();

        bool is_valid();
        bool is_empty();
        bool is_ring();
        bool is_closed();
        bool has_z();

        Ptr get_envelope();
        Ptr get_exterior_ring();

        Bounds::Ptr get_bounds();

        const GEOSCoordSequence* get_coords();

    protected:
        std::shared_ptr<Ctx> ctx;
        const GEOSGeometry* ro_g;

        friend class Ctx;
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

        void reproject(int src_srid, int tgt_srid);

    private:
        GeomRW();
        GeomRW(std::shared_ptr<Ctx> ctx, GEOSGeometry* g);
        GeomRW(const GeomRW& other);

        GEOSGeometry* rw_g;

        friend class Ctx;
};


class Ctx: public std::enable_shared_from_this<Ctx>
{
    public:
        typedef std::shared_ptr<Ctx> Ptr;

        static Ptr create();
        ~Ctx();

        Geom::Ptr from_wkb(io::Bytes::Ptr wkb);

    private:
        Ctx();
        Ctx(const Ctx& other);

        GeomRO::Ptr wrap(const GEOSGeometry* g);
        GeomRW::Ptr wrap(GEOSGeometry* g);

        void destroy(GEOSGeometry* g);

        GEOSContextHandle_t ctx;

        friend class Geom;
        friend class GeomRO;
        friend class GeomRW;
};


NS_EASTON_GEO_END
NS_EASTON_END


#endif
