
#ifndef EASTON_INDEX_HH
#define EASTON_INDEX_HH


#include <tchdb.h>
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

        uint64_t curr_docid_num();
        uint64_t doc_count();

        void put_kv(io::Bytes::Ptr key, io::Bytes::Ptr val);
        io::Bytes::Ptr get_kv(io::Bytes::Ptr key);
        bool del_kv(io::Bytes::Ptr key);

        void update(io::Bytes::Ptr docid, io::Bytes::Vector wkbs);
        void remove(io::Bytes::Ptr docid);

        std::vector<Result> query(geo::Bounds::Ptr bounds, bool nearest);

    private:
        Index();
        Index(int argc, const char* argv[]);
        Index(const Index& other);

        void init_id_idx();
        void init_geo_idx(int argc, const char* argv[]);

        io::Bytes::Ptr make_dockey(io::Bytes::Ptr docid);

        uint64_t get_doc_num(io::Bytes::Ptr docid);

        io::Bytes::Ptr make_id_value(
                uint64_t docnum, geo::Bounds::Vector bounds);
        uint64_t read_id_value(
                io::Bytes::Ptr data, geo::Bounds::Vector& bounds);

        io::Bytes::Ptr make_geo_value(
                io::Bytes::Ptr docid, io::Bytes::Ptr wkb);
        void read_geo_value(IndexItemH item,
                io::Bytes::Ptr& docid, io::Bytes::Ptr& wkb);

        std::string base_dir;
        std::string id_file;
        std::string geo_file;

        TCHDB* id_idx;
        IndexH geo_idx;
        geo::Util::Ptr geo_util;

        uint64_t dimensions;
        uint64_t doc_id_num;
};


NS_EASTON_END


#endif
