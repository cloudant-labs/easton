
#ifndef EASTON_IO_HH
#define EASTON_IO_HH

#include <sys/time.h>

#include <ctime>
#include <string>
#include <unordered_map>

#include <leveldb/db.h>
#include <leveldb/write_batch.h>
#include <spatialindex/capi/sidx_api.h>
#include <spatialindex/capi/CustomStorage.h>

#include "ei.h"

#include "easton.hh"


typedef struct SpatialIndex::StorageManager::CustomStorageManagerCallbacks
            SpatialIndexStorageManager;


NS_EASTON_BEGIN
NS_EASTON_IO_BEGIN


bool is_dir(std::string dname);


class Timer
{
    public:
        void start();
        void print(std::string prefix);

    private:
        struct timeval tv;
};


class Bytes
{
    public:
        typedef std::shared_ptr<Bytes> Ptr;
        typedef std::vector<Ptr> Vector;
        typedef std::vector<Ptr>::iterator VIter;

        void display();

        // Create objects that own the underlying memory
        static Ptr create(uint32_t len);
        static Ptr create(uint8_t* data, uint32_t len);

        // Create objects by copying the provided memory
        static Ptr copy(const uint8_t* const data, uint32_t len);

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

        void print();

        bool read(bool& val);
        bool read(int64_t& val);
        bool read(uint64_t& val);
        bool read(double& val);
        bool read(std::string& val);

        Bytes::Ptr read_bytes();

        // The read_SOMETHING_n functions are to assert
        // that the given arity was found for the data
        // type rather than an investigatory what is the
        // arity.

        bool read_tuple(int32_t& arity);
        bool read_tuple_n(int32_t arity);

        bool read_list(int32_t& arity);
        bool read_list_n(int32_t arity);
        bool read_empty_list();

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
        typedef std::unique_ptr<ei_x_buff> EIXBuffPtr;

        static Ptr create();
        ~Writer();

        void send();
        Bytes::Ptr serialize();

        void write(bool val);
        void write(const char* val);
        void write(int64_t val);
        void write(uint64_t val);
        void write(double val);
        void write(Bytes::Ptr val);

        void start_tuple(int32_t arity);
        void start_list(int32_t arity);
        void write_empty_list();

    private:
        Writer();
        Writer(const Writer& other);

        EIXBuffPtr buff;
};


class Transaction;
typedef std::weak_ptr<Transaction> TxMonitor;


class Storage
{
    public:
        typedef std::shared_ptr<Storage> Ptr;

        static Ptr create(std::string dirname);
        ~Storage();

        uint64_t data_size();

        io::Bytes::Ptr make_key(const char* tag, const char* val);
        io::Bytes::Ptr make_key(const char* tag, io::Bytes::Ptr val);

        Bytes::Ptr get_kv(Bytes::Ptr key);
        void put_kv(Bytes::Ptr key, Bytes::Ptr val);
        void del_kv(Bytes::Ptr key);

        void* get_storage_manager();
        int64_t new_geoid();

    private:
        Storage();
        Storage(std::string dirname);
        Storage(const Storage& other);

        void set_transaction(TxMonitor tx);
        void write(leveldb::WriteBatch* batch);

        std::string dirname;
        leveldb::DB* db;
        leveldb::Options o_opts;
        leveldb::ReadOptions r_opts;
        leveldb::WriteOptions w_opts;
        TxMonitor tx;

        Bytes::Ptr geoid_num_key;
        int64_t geoid_num;
        SpatialIndexStorageManager sm;

        friend class Transaction;
};


class Transaction
{
    public:
        typedef std::shared_ptr<Transaction> Ptr;
        typedef std::unique_ptr<leveldb::WriteBatch> WBPtr;

        static Ptr open(Storage::Ptr store);
        static Ptr autocommit(Storage::Ptr store);
        ~Transaction();

        void commit();

    private:
        Transaction(Storage::Ptr store, bool is_autocommit);
        Transaction(const Transaction& other);

        io::Bytes::Ptr get_kv(Bytes::Ptr key);
        void put_kv(Bytes::Ptr key, Bytes::Ptr val);
        void del_kv(Bytes::Ptr key);

        Storage::Ptr store;
        WBPtr batch;
        std::unordered_map<std::string, io::Bytes::Ptr> rbuf;
        bool is_autocommit;

        friend class Storage;
};


NS_EASTON_IO_END
NS_EASTON_END

#endif
