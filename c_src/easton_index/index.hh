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

#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


#include <spatialindex/capi/sidx_api.h>
#include <spatialindex/capi/sidx_impl.h>

#include "easton.hh"
#include "geo.hh"
#include "io.hh"


using namespace easton;
namespace sidx = SpatialIndex;


NS_EASTON_BEGIN


class Index;


class Hit
{
    public:
        Hit();
        Hit(io::Bytes::Ptr docid, geo::Geom::Ptr geom, double distance);

        friend bool operator<(const Hit &h1, const Hit &h2);
        friend bool operator<=(const Hit &h1, const Hit &h2);

        io::Bytes::Ptr docid;
        geo::Geom::Ptr geom;
        io::Bytes::Ptr wkb;
        double distance;
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

        void visit_node();
        void visit_data();

        std::priority_queue<Hit, std::vector<Hit>> hits;
        Hit bookmark;
        geo::GeomFilter filt;
        uint32_t limit;

        uint64_t nodes_visited;
        uint64_t leaves_visited;
        uint64_t leaves_filtered;
        uint64_t leaves_dropped;
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


class TemporalEntry: public Entry
{
    public:
        static Entry::Ptr read_id(io::Reader::Ptr reader);
        static Entry::Ptr read_geo(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx, io::Bytes::Ptr& docid);
        static Entry::Ptr read_update(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx);
        static Entry::Ptr read_query(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx, geo::SRID::Ptr srid);

        virtual ~TemporalEntry();

        virtual void write_id(io::Writer::Ptr writer);

        virtual void update(Index* idx, io::Bytes::Ptr docid,
                uint64_t docnum, uint64_t dims);
        virtual void remove(Index* idx, uint64_t docnum, uint64_t dims);
        virtual void search(Index* idx, TopHits& collector, bool nearest);

        virtual geo::Geom::Ptr get_geometry();
        virtual geo::GeomFilter make_filter(geo::Ctx::Ptr ctx, uint64_t filter);

    private:
        TemporalEntry();
        TemporalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom);
        TemporalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom,
                geo::Bounds::Ptr vbox, double t_start, double t_end);
        TemporalEntry(geo::Bounds::Ptr bbox, geo::Bounds::Ptr vbox,
                double t_start, double t_end);
        TemporalEntry(const SpatialEntry& other);

        io::Bytes::Ptr wkb;
        geo::Geom::Ptr geom;
        geo::Bounds::Ptr bbox;
        geo::Bounds::Ptr vbox;
        double t_start;
        double t_end;
};

class HistoricalEntry: public Entry
{
    public:
        static Entry::Ptr read_id(io::Reader::Ptr reader);
        static Entry::Ptr read_geo(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx, io::Bytes::Ptr& docid);
        static Entry::Ptr read_update(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx);
        static Entry::Ptr read_query(io::Reader::Ptr reader,
                geo::Ctx::Ptr ctx, geo::SRID::Ptr srid);

        virtual ~HistoricalEntry();

        virtual void write_id(io::Writer::Ptr writer);

        virtual void update(Index* idx, io::Bytes::Ptr docid,
                uint64_t docnum, uint64_t dims);
        virtual void remove(Index* idx, uint64_t docnum, uint64_t dims);
        virtual void search(Index* idx, TopHits& collector, bool nearest);

        virtual geo::Geom::Ptr get_geometry();
        virtual geo::GeomFilter make_filter(geo::Ctx::Ptr ctx, uint64_t filter);

    private:
        HistoricalEntry();
        HistoricalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom);
        HistoricalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom, double t_start,
                double t_end);
        HistoricalEntry(geo::Bounds::Ptr bbox, double t_start, double t_end);
        HistoricalEntry(const SpatialEntry& other);

        io::Bytes::Ptr wkb;
        geo::Geom::Ptr geom;
        geo::Bounds::Ptr bbox;
        geo::Bounds::Ptr vbox;
        double t_start;
        double t_end;
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


class RTreePager : public sidx::IQueryStrategy
{
    public:
        RTreePager(EntryVisitor* visitor, sidx::IShape* query);
        ~RTreePager();

        void getNextEntry(const sidx::IEntry& entry,
                            int64_t& next_id, bool& hasNext);

    private:
        bool should_visit(const sidx::IShape* mbr);

        std::stack<int64_t> ids;

        EntryVisitor* visitor;
        sidx::IShape* query;
        sidx::IShape* query_center;
};


class Index
{
    public:
        typedef std::shared_ptr<Index> Ptr;

        static Ptr create(io::Reader::Ptr reader);
        ~Index();

        void sync();

        IndexH get_index();
        void reset_index(bool validate = true);

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
        void init_geo_idx(bool validate = true);
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

        int64_t idx_type;
        int64_t idx_dims;

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
