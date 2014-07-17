
#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


#include <spatialindex/capi/sidx_api.h>

#include "easton.hh"
#include "geo.hh"
#include "io.hh"


using namespace easton;


NS_EASTON_BEGIN


class Index
{
    public:
        typedef std::shared_ptr<Index> Ptr;
        typedef std::pair<io::Bytes::Ptr, io::Bytes::Ptr> Result;

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

        std::vector<Result> search(geo::Bounds::Ptr bounds, bool nearest);

    private:
        Index();
        Index(int argc, const char* argv[]);
        Index(const Index& other);

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
