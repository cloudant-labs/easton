
#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


#include <spatialindex/capi/sidx_api.h>
#include <spatialindex/capi/sidx_impl.h>

#include "easton.hh"
#include "geo.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_BEGIN


class Hit
{
    public:
        Hit();
        Hit(io::Bytes::Ptr docid, geo::Geom::Ptr geom, double distance);

        io::Bytes::Ptr docid;
        geo::Geom::Ptr geom;
        io::Bytes::Ptr wkb;
        double distance;
};


struct HitCmp
{
    bool operator()(Hit const &h1, Hit const &h2);
};


class TopHits
{
    public:
        TopHits(Hit bookmark, geo::GeomFilter filt, uint32_t limit);
        ~TopHits();

        double distance(io::Bytes::Ptr docid, geo::Geom::Ptr geom);

        uint32_t size();

        void push(io::Bytes::Ptr docid, geo::Geom::Ptr geom);
        Hit pop();

        std::priority_queue<Hit, std::vector<Hit>, HitCmp> hits;
        HitCmp cmp;
        Hit bookmark;
        geo::GeomFilter filt;
        uint32_t limit;
};


class NNComparator: public SpatialIndex::INearestNeighborComparator
{
    public:
        NNComparator(geo::Ctx::Ptr ctx, TopHits& hits);

        double getMinimumDistance(
                const SpatialIndex::IShape& query,
                const SpatialIndex::IShape& entry
            );

	    double getMinimumDistance(
	            const SpatialIndex::IShape& query,
	            const SpatialIndex::IData& data
            );

        geo::Ctx::Ptr ctx;
        TopHits& hits;
};


class EntryVisitor: public SpatialIndex::IVisitor
{
    public:
        EntryVisitor(geo::Ctx::Ptr ctx, TopHits& hits);
        ~EntryVisitor();

        void visitNode(const SpatialIndex::INode& n);
        void visitData(const SpatialIndex::IData& d);
        void visitData(std::vector<const SpatialIndex::IData*>& v);

        geo::Ctx::Ptr ctx;
        TopHits& hits;
};


class Index
{
    public:
        typedef std::shared_ptr<Index> Ptr;

        static Ptr create(int argc, const char* argv[]);
        ~Index();

        void sync();

        geo::Ctx::Ptr get_geo_ctx();

        uint64_t curr_docid_num();
        uint64_t doc_count();

        void put_kv(io::Bytes::Ptr key, io::Bytes::Ptr val);
        io::Bytes::Ptr get_kv(io::Bytes::Ptr key);
        void del_kv(io::Bytes::Ptr key);

        void update(io::Bytes::Ptr docid, io::Bytes::Vector wkbs);
        void remove(io::Bytes::Ptr docid);
        void search(TopHits& collector, geo::Bounds::Ptr bounds, bool nearest);

    private:
        Index();
        Index(int argc, const char* argv[]);
        Index(const Index& other);

        void remove_int(io::Bytes::Ptr docid);

        void init_storage();
        void init_geo_idx(int argc, const char* argv[]);
        void init_srid(const char* srid_str);
        void load_index_id();
        void store_index_id();

        uint64_t get_docid_num(io::Bytes::Ptr docid);

        io::Bytes::Ptr make_id_value(
                uint64_t docnum, geo::Bounds::Vector bounds);
        uint64_t read_id_value(
                io::Bytes::Ptr data, geo::Bounds::Vector& bounds);

        io::Bytes::Ptr make_geo_value(
                io::Bytes::Ptr docid, io::Bytes::Ptr wkb);
        void read_geo_value(IndexItemH item,
                io::Bytes::Ptr& docid, io::Bytes::Ptr& wkb);

        std::string db_dir;

        io::Storage::Ptr store;
        IndexH geo_idx;
        geo::Ctx::Ptr geo_ctx;

        int32_t srid;

        io::Bytes::Ptr docid_num_key;

        uint64_t dimensions;
        uint64_t docid_num;
        int64_t index_id;
};


NS_EASTON_END


#endif
