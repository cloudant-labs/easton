
#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


#include <spatialindex/capi/sidx_api.h>
#include <spatialindex/capi/sidx_impl.h>

#include "easton.hh"
#include "geo.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_BEGIN


class Index;


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


class Entry
{
    public:
        typedef std::shared_ptr<Entry> Ptr;
        typedef std::vector<Ptr> Vector;
        typedef std::vector<Ptr>::iterator VIter;

        virtual ~Entry() = 0;

        virtual void write_id(io::Writer::Ptr writer) = 0;

        virtual void update(Index* idx, io::Bytes::Ptr docid,
                uint64_t docnum, uint64_t dims) = 0;
        virtual void remove(Index* idx, uint64_t docnum, uint64_t dims) = 0;
        virtual void search(Index* idx, TopHits& collector, bool nearest) = 0;

        virtual geo::Geom::Ptr get_geometry() = 0;
        virtual geo::GeomFilter make_filter(geo::Ctx::Ptr ctx,
                uint64_t filter) = 0;
};


class SpatialEntry: public Entry
{
    public:
        static Entry::Ptr read_id(io::Reader::Ptr reader);
        static Entry::Ptr read_geo(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx, io::Bytes::Ptr& docid);
        static Entry::Ptr read_update(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx);
        static Entry::Ptr read_query(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx, geo::SRID::Ptr srid);

        virtual ~SpatialEntry();

        virtual void write_id(io::Writer::Ptr writer);

        virtual void update(Index* idx, io::Bytes::Ptr docid,
                uint64_t docnum, uint64_t dims);
        virtual void remove(Index* idx, uint64_t docnum, uint64_t dims);
        virtual void search(Index* idx, TopHits& collector, bool nearest);

        virtual geo::Geom::Ptr get_geometry();
        virtual geo::GeomFilter make_filter(geo::Ctx::Ptr ctx, uint64_t filter);

    private:
        SpatialEntry();
        SpatialEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom);
        SpatialEntry(geo::Bounds::Ptr bbx);
        SpatialEntry(const SpatialEntry& other);

        io::Bytes::Ptr wkb;
        geo::Geom::Ptr geom;
        geo::Bounds::Ptr bbox;
};


class EntryReader
{
    public:
        typedef std::shared_ptr<EntryReader> Ptr;

        static Ptr create(geo::Ctx::Ptr ctx, int64_t index_type);

        Entry::Ptr read_id(io::Reader::Ptr reader);
        Entry::Ptr read_geo(io::Reader::Ptr reader, io::Bytes::Ptr& docid);
        Entry::Ptr read_update(io::Reader::Ptr reader);
        Entry::Ptr read_query(io::Reader::Ptr reader, geo::SRID::Ptr srid);

    private:
        EntryReader();
        EntryReader(geo::Ctx::Ptr ctx, int64_t index_type);
        EntryReader(const EntryReader& other);

        geo::Ctx::Ptr ctx;
        int64_t index_type;
};


class NNComparator: public SpatialIndex::INearestNeighborComparator
{
    public:
        NNComparator(geo::Ctx::Ptr ctx, EntryReader::Ptr reader, TopHits& hits);

        double getMinimumDistance(
                const SpatialIndex::IShape& query,
                const SpatialIndex::IShape& entry
            );

	    double getMinimumDistance(
	            const SpatialIndex::IShape& query,
	            const SpatialIndex::IData& data
            );

        geo::Ctx::Ptr ctx;
        EntryReader::Ptr reader;
        TopHits& hits;
};


class EntryVisitor: public SpatialIndex::IVisitor
{
    public:
        EntryVisitor(geo::Ctx::Ptr ctx, EntryReader::Ptr reader, TopHits& hits);
        ~EntryVisitor();

        void visitNode(const SpatialIndex::INode& n);
        void visitData(const SpatialIndex::IData& d);
        void visitData(std::vector<const SpatialIndex::IData*>& v);

        geo::Ctx::Ptr ctx;
        EntryReader::Ptr reader;
        TopHits& hits;
};


class Index
{
    public:
        typedef std::shared_ptr<Index> Ptr;

        static Ptr create(io::Reader::Ptr reader);
        ~Index();

        void sync();

        IndexH get_index();
        geo::Ctx::Ptr get_geo_ctx();
        EntryReader::Ptr get_reader();

        uint64_t curr_docid_num();
        uint64_t get_doc_count();
        uint64_t get_geom_count();
        uint64_t data_size();

        void put_kv(io::Bytes::Ptr key, io::Bytes::Ptr val);
        io::Bytes::Ptr get_kv(io::Bytes::Ptr key);
        void del_kv(io::Bytes::Ptr key);

        void update(io::Bytes::Ptr docid, Entry::Vector entries);
        void remove(io::Bytes::Ptr docid);
        void search(TopHits& collector, Entry::Ptr query, bool nearest);

    private:
        Index();
        Index(std::string dir, int64_t type, int64_t dims, geo::SRID::Ptr srid);
        Index(const Index& other);

        void remove_int(io::Bytes::Ptr docid);

        void init_storage();
        void init_geo_idx(int64_t type, int64_t dims);
        void init_srid(geo::SRID::Ptr srid);
        void init_counts();
        void load_index_id();
        void store_index_id();
        void store_counts();

        uint64_t get_docid_num(io::Bytes::Ptr docid);

        std::string db_dir;

        io::Storage::Ptr store;
        IndexH geo_idx;
        geo::Ctx::Ptr geo_ctx;
        EntryReader::Ptr reader;

        geo::SRID::Ptr srid;

        io::Bytes::Ptr docid_num_key;

        uint64_t dimensions;
        uint64_t docid_num;
        int64_t index_id;

        uint64_t doc_count;
        uint64_t geom_count;
};


NS_EASTON_END


#endif
