
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

        double* mins();
        double* maxs();

    private:
        Bounds();
        Bounds(uint32_t dims);
        Bounds(const Bounds& other);

        DimsPtr data;
        uint32_t dims;
};


class Util
{
    public:
        typedef std::shared_ptr<Util> Ptr;

        static Ptr create();
        ~Util();

        Bounds::Ptr get_bounds(io::Bytes::Ptr wkb, uint32_t dimensions);
        bool filter(uint8_t filter, io::Bytes::Ptr wkb);

    private:
        Util();
        Util(const Util& other);

        GEOSGeometry* from_wkb(io::Bytes::Ptr wkb);

        GEOSContextHandle_t ctx;
};


NS_EASTON_GEO_END
NS_EASTON_END


#endif
