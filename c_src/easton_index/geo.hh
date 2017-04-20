// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

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
        static Ptr read(io::Reader::Ptr reader);
        ~Bounds();

        void expand(Ptr other);

        void write(io::Writer::Ptr writer);

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


class SRID
{
    public:
        typedef std::shared_ptr<SRID> Ptr;

        static Ptr LL();
        static Ptr from_reader(io::Reader::Ptr reader);

        std::string str();
        const char* c_str();

    private:
        SRID(const char* name);
        SRID(const SRID& other);

        std::string name;
};


class Geom
{
    public:
        typedef std::shared_ptr<Geom> Ptr;

        virtual ~Geom() = 0;

        std::string to_wkt();
        io::Bytes::Ptr to_wkb();

        int get_type();

        uint32_t get_dims();
        bool is_valid();
        bool is_empty();
        bool is_ring();
        bool is_closed();
        bool has_z();

        double distance(Ptr other);

        Ptr get_centroid();
        Ptr get_envelope();
        Ptr get_exterior_ring();

        Ptr reproject(SRID::Ptr src, SRID::Ptr tgt);

        Bounds::Ptr get_bounds();

        const GEOSCoordSequence* get_coords();

    protected:
        std::shared_ptr<Ctx> ctx;
        const GEOSGeometry* ro_g;

        Bounds::Ptr get_bounds_simple();
        Bounds::Ptr get_bounds_polygon();
        Bounds::Ptr get_bounds_collection();

        std::vector<Ptr> get_rings();
        std::vector<Ptr> get_geoms();

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
        GeomFilter();
        GeomFilter(std::shared_ptr<Ctx> ctx, Geom::Ptr g, uint64_t filter);
        ~GeomFilter();

        double distance(Geom::Ptr other);

        bool operator()(Geom::Ptr other);
        Geom::Ptr geom();

    private:
        PrepGeom::Ptr pg;
        uint64_t filter;

        friend class Geom;
};


class Ctx: public std::enable_shared_from_this<Ctx>
{
    public:
        typedef std::shared_ptr<Ctx> Ptr;

        static Ptr create(uint32_t dimensions, SRID::Ptr srid);
        ~Ctx();

        SRID::Ptr get_srid();

        geo::Bounds::Ptr get_zero_bounds();

        GeomFilter make_filter(Geom::Ptr geom, uint64_t filter);

        Geom::Ptr geom_from_reader(io::Reader::Ptr reader, SRID::Ptr srid);

        Geom::Ptr from_wkb(io::Bytes::Ptr wkb, SRID::Ptr srid);
        Geom::Ptr from_wkt(io::Bytes::Ptr wkt, SRID::Ptr srid);

        Geom::Ptr from_wkb(io::Bytes::Ptr wkb);
        Geom::Ptr from_wkt(io::Bytes::Ptr wkt);

        Geom::Ptr make_point(
                double x,
                double y,
                SRID::Ptr srid
            );

        Geom::Ptr make_rectangle(
                double* mins,
                double* maxs,
                uint32_t dimensions,
                SRID::Ptr srid
            );

        Geom::Ptr make_circle(
                double x,
                double y,
                double r,
                SRID::Ptr srid
            );

        Geom::Ptr make_ellipse(
                double x,
                double y,
                double x_range,
                double y_range,
                SRID::Ptr srid
            );

    private:
        Ctx();
        Ctx(uint32_t dimensions, SRID::Ptr srid);
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
        SRID::Ptr srid;
        GEOSCtx ctx;

        friend class Bounds;
        friend class Geom;
        friend class GeomRO;
        friend class GeomRW;
        friend class PrepGeom;
};


NS_EASTON_GEO_END
NS_EASTON_END


#endif
