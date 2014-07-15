
#ifndef EASTON_IO_HH
#define EASTON_IO_HH


#include "ei.h"

#include "easton.hh"
#include <leveldb/db.h>


NS_EASTON_BEGIN
NS_EASTON_IO_BEGIN


bool is_dir(std::string dname);


class Bytes
{
    public:
        typedef std::shared_ptr<Bytes> Ptr;
        typedef std::vector<Ptr> Vector;
        typedef std::vector<Ptr>::iterator VIter;

        // Create objects that own the underlying memory
        static Ptr create(uint32_t len);
        static Ptr create(uint8_t* data, uint32_t len);

        // Create objects by copying the provided memory
        static Ptr copy(uint8_t* data, uint32_t len);

        // Create objects that only proxy to the underlying memory
        static Ptr proxy(const char* data);
        static Ptr proxy(uint8_t* data, uint32_t len);

        ~Bytes();

        leveldb::Slice slice();

        uint8_t* get();
        uint32_t size();

    private:
        Bytes();
        Bytes(uint32_t len);
        Bytes(uint8_t* data, uint32_t len, bool owner);
        Bytes(const Bytes& other);

        bool owner;
        uint8_t* data;
        uint32_t len;
};


class Reader
{
    public:
        typedef std::shared_ptr<Reader> Ptr;

        static Ptr recv();
        static Ptr create(Bytes::Ptr data);
        ~Reader();

        bool read(bool& val);
        bool read(uint64_t& val);
        bool read(double& val);

        Bytes::Ptr read_bytes();

        // The read_SOMETHING_n functions are to assert
        // that the given arity was found for the data
        // type rather than an investigatory what is the
        // arity.

        bool read_tuple(int32_t& arity);
        bool read_tuple_n(int32_t arity);

        bool read_list(int32_t& arity);
        bool read_list_n(int32_t arity);

    private:
        Reader();
        Reader(Bytes::Ptr data);
        Reader(const Reader& other);

        Bytes::Ptr data;
        int32_t pos;
};


class Writer
{
    public:
        typedef std::shared_ptr<Writer> Ptr;

        static Ptr create();
        ~Writer();

        void send();
        Bytes::Ptr serialize();

        void write(bool val);
        void write(const char* val);
        void write(uint64_t val);
        void write(double val);
        void write(Bytes::Ptr val);

        void start_tuple(int32_t arity);
        void start_list(int32_t arity);
        void write_empty_list();

    private:
        Writer();
        Writer(const Writer& other);

        ei_x_buff* buff;
};


class Storage
{
    public:
        typedef std::shared_ptr<Storage> Ptr;

        static Ptr create(std::string dirname);
        ~Storage();

        void put_kv(Bytes::Ptr key, Bytes::Ptr val);
        Bytes::Ptr get_kv(Bytes::Ptr key);
        void del_kv(Bytes::Ptr key);

    private:
        Storage();
        Storage(std::string dirname);
        Storage(const Storage& other);

        std::string dirname;
        leveldb::DB* db;
        leveldb::Options o_opts;
        leveldb::ReadOptions r_opts;
        leveldb::WriteOptions w_opts;
};


NS_EASTON_IO_END
NS_EASTON_END

#endif
