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

#include <stdio.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/stat.h>

#include <stdexcept>
#include <string>

#include <leveldb/filter_policy.h>

#include "config.hh"
#include "exceptions.hh"
#include "io.hh"


typedef SpatialIndex::StorageManager::CustomStorageManager CSM;


NS_EASTON_BEGIN
NS_EASTON_IO_BEGIN


Bytes::Ptr
geo_key(const int64_t id)
{
    Writer::Ptr writer = Writer::create();
    writer->start_tuple(2);
    writer->write("geoid");
    writer->write(id);
    return writer->serialize();
}


Storage*
get_storage(const void* context)
{
    return const_cast<Storage*>(static_cast<const Storage*>(context));
}


void
sm_flush_cb(const void* context, int* err)
{
    *err = CSM::NoError;
}


void
sm_load_cb(const void* context,
        const int64_t id, uint32_t* len, uint8_t** data, int* err)
{
    Storage* s = get_storage(context);
    Bytes::Ptr key = geo_key(id);
    Bytes::Ptr val = s->get_kv(key);

    if(!val) {
        *len = 0;
        *data = NULL;
        *err = CSM::InvalidPageError;
        return;
    }

    *len = val->size();
    *data = new uint8_t[val->size()];
    memcpy(*data, val->get(), val->size());

    *err = CSM::NoError;
}

void
sm_store_cb(const void* context,
        int64_t* id, const uint32_t len, const uint8_t* const data, int* err)
{
    Storage* s = get_storage(context);

    if(*id == SpatialIndex::StorageManager::NewPage) {
        *id = s->new_geoid();
    }

    // TODO: Figure out how to make a const proxy.
    Bytes::Ptr key = geo_key(*id);
    Bytes::Ptr val = Bytes::copy(data, len);

    s->put_kv(key, val);

    *err = CSM::NoError;
}


void
sm_delete_cb(const void* context, const int64_t id, int* err)
{
    Storage* s = get_storage(context);
    Bytes::Ptr key = geo_key(id);
    s->del_kv(key);
    *err = CSM::NoError;
}


bool
is_dir(std::string dname)
{
    struct stat s;

    if(stat(dname.c_str(), &s) != 0) {
        return false;
    }

    if(s.st_mode && S_IFDIR == S_IFDIR) {
        return true;
    }

    return false;
}


void
Timer::start()
{
    gettimeofday(&this->tv, NULL);
}


void
Timer::print(std::string prefix)
{
    uint32_t pid = (uint32_t) getpid();
    struct timeval tv2;
    gettimeofday(&tv2, NULL);

    uint64_t t1 = ((uint64_t) this->tv.tv_sec) * 1000000
            + ((uint64_t) this->tv.tv_usec);
    uint64_t t2 = ((uint64_t) tv2.tv_sec) * 1000000
            + ((uint64_t) tv2.tv_usec);
    double diff = (double) (t2 - t1) / 1000.0;
    fprintf(stderr, "[%u] %s :: %0.3f ms\r\n", pid, prefix.c_str(), diff);
}


Bytes::Ptr
Bytes::create(uint32_t len)
{
    return Ptr(new Bytes(len));
}


Bytes::Ptr
Bytes::create(uint8_t* data, uint32_t len)
{
    return Ptr(new Bytes(data, len, true));
}


Bytes::Ptr
Bytes::copy(const uint8_t* const data, uint32_t len)
{
    Ptr p(new Bytes(len));
    memcpy(p->get(), data, len);
    return p;
}


Bytes::Ptr
Bytes::proxy(const char* data)
{
    return Ptr(new Bytes((uint8_t*) data, strlen(data), false));
}


Bytes::Ptr
Bytes::proxy(uint8_t* data, uint32_t len)
{
    return Ptr(new Bytes(data, len, false));
}


Bytes::Bytes(uint32_t len)
{
    this->owner = true;
    this->data = new uint8_t[len];
    this->len = len;
}


Bytes::Bytes(uint8_t* data, uint32_t len, bool owner)
{
    this->owner = owner;
    this->data = data;
    this->len = len;
}


Bytes::~Bytes()
{
    if(owner) {
        delete [] this->data;
    }
}


void
Bytes::display()
{
    for(uint32_t i = 0; i < this->len; i += 2) {
        fprintf(stderr, "%02x", this->data[i]);
        if(i + 1 < this->len) {
            fprintf(stderr, "%02x", this->data[i+1]);
        }
        if((i + 2) % 16 == 0) {
            fprintf(stderr, "\r\n");
        } else {
            fprintf(stderr, " ");
        }
    }
    fprintf(stderr, "\r\n");
}


leveldb::Slice
Bytes::slice()
{
    return leveldb::Slice((char*) this->data, this->len);
}

uint8_t*
Bytes::get()
{
    return this->data;
}


uint32_t
Bytes::size()
{
    return this->len;
}


Reader::Ptr
Reader::recv()
{
    uint32_t packet_len;
    uint32_t pos;
    ssize_t ret;

    ret = ::read(EASTON_STREAM_IN, &packet_len, sizeof(uint32_t));
    if(ret == 0) {
        return Reader::Ptr();
    } else if(ret != sizeof(uint32_t)) {
        throw EastonExit(EASTON_ERROR_BAD_READ);
    }

    packet_len = ntohl(packet_len);

    Bytes::Ptr data = Bytes::create(packet_len);
    pos = 0;
    do {
        ret = ::read(EASTON_STREAM_IN, data->get() + pos, packet_len - pos);
        if(ret <= 0) {
            throw EastonExit(EASTON_ERROR_BAD_READ);
        }
        pos += (uint32_t) ret;
    } while(pos < packet_len);

    return Ptr(new Reader(data));
}


Reader::Ptr
Reader::create(Bytes::Ptr data)
{
    return Ptr(new Reader(data));
}


Reader::Reader(Bytes::Ptr data)
{
    this->data = data;
    this->pos = 0;

    if(!this->data) {
        throw EastonException("Invalid data source for Reader.");
    }

    int vsn;
    if(ei_decode_version((char*) this->data->get(), &(this->pos), &vsn) != 0) {
        throw EastonException("Invalid version data in Reader data.");
    }
}


Reader::~Reader()
{
}


void
Reader::print()
{
    int32_t pos = this->pos;
    ei_print_term(stderr, (char*) this->data->get(), &(pos));
    fprintf(stderr, "\r\n");
}


bool
Reader::read(bool& val)
{
    int32_t b;

    if(ei_decode_boolean((char*) this->data->get(), &(this->pos), &b) != 0) {
        return false;
    }

    if(b) {
        val = true;
    } else {
        val = false;
    }

    return true;
}


bool
Reader::read(int64_t& val)
{
    long long int tmp;
    if(ei_decode_longlong(
            (char*) this->data->get(), &(this->pos), &tmp) != 0) {
        return false;
    }

    val = (int64_t) tmp;
    return true;
}


bool
Reader::read(uint64_t& val)
{
    long long unsigned int tmp;
    if(ei_decode_ulonglong(
            (char*) this->data->get(), &(this->pos), &tmp) != 0) {
        return false;
    }

    val = (uint64_t) tmp;
    return true;
}


bool
Reader::read(double& val)
{
    if(ei_decode_double(
            (char*) this->data->get(), &(this->pos), &val) != 0) {
        return false;
    }

    return true;
}


bool
Reader::read(std::string& val)
{
    int32_t type;
    int32_t bytes;

    if(ei_get_type(
            (char*) this->data->get(), &(this->pos), &type, &bytes) != 0) {
        return false;
    }

    if(type != ERL_BINARY_EXT && type != ERL_ATOM_EXT) {
        return false;
    }

    val.resize(bytes);
    if(type == ERL_ATOM_EXT) {
        if(ei_decode_atom(
                (char*) this->data->get(), &(this->pos),
                (char*) val.data()) != 0) {
            return false;
        }
    } else if(type == ERL_BINARY_EXT) {
        if(ei_decode_binary(
                (char*) this->data->get(), &(this->pos),
                (void*) val.data(), NULL) != 0) {
            return false;
        }
    } else {
        return false;
    }

    return true;
}


Bytes::Ptr
Reader::read_bytes()
{
    int32_t type;
    int32_t bytes;

    if(ei_get_type(
            (char*) this->data->get(), &(this->pos), &type, &bytes) != 0) {
        return Bytes::Ptr();
    }

    if(type != ERL_BINARY_EXT) {
        return Bytes::Ptr();
    }

    Bytes::Ptr b = Bytes::create(bytes);
    if(ei_decode_binary(
            (char*) this->data->get(), &(this->pos),
            b->get(), NULL) != 0) {
        return Bytes::Ptr();
    }

    return b;
}


bool
Reader::read_tuple(int32_t& arity)
{
    if(ei_decode_tuple_header(
            (char*) this->data->get(), &(this->pos), &arity) != 0) {
        return false;
    }

    return true;
}


bool
Reader::read_tuple_n(int32_t arity)
{
    int32_t found;

    if(ei_decode_tuple_header(
            (char*) this->data->get(), &(this->pos), &found) != 0) {
        return false;
    }

    if(found != arity) {
        return false;
    }

    return true;
}


bool
Reader::read_list(int32_t& arity)
{
    if(ei_decode_list_header(
            (char*) this->data->get(), &(this->pos), &arity) != 0) {
        return false;
    }

    return true;
}


bool
Reader::read_list_n(int32_t arity)
{
    int32_t found;

    if(ei_decode_list_header(
            (char*) this->data->get(), &(this->pos), &found) != 0) {
        return false;
    }

    if(found != arity) {
        return false;
    }

    return true;
}


bool
Reader::read_empty_list()
{
    return this->read_list_n(0);
}


Writer::Ptr
Writer::create()
{
    return Ptr(new Writer());
}


Writer::Writer()
{
    this->buff = EIXBuffPtr(new ei_x_buff);
    if(ei_x_new_with_version(this->buff.get()) != 0) {
        throw EastonException("Error initializing Writer buffer.");
    }
}


Writer::~Writer()
{
    if(ei_x_free(this->buff.get()) != 0) {
        throw EastonException("Error destroying Writer buffer.");
    }
}


void
Writer::send()
{
    uint32_t packet_len = htonl((uint32_t) this->buff->index);
    uint32_t pos;
    ssize_t ret;

    ret = ::write(EASTON_STREAM_OUT, &packet_len, sizeof(uint32_t));
    if(ret != sizeof(uint32_t)) {
        throw EastonExit(EASTON_ERROR_BAD_WRITE);
    }

    pos = 0;
    do {
        ret = ::write(
                    EASTON_STREAM_OUT,
                    this->buff->buff + pos,
                    this->buff->index - pos
                );
        if(ret <= 0) {
            throw EastonExit(EASTON_ERROR_BAD_READ);
        }
        pos += (uint32_t) ret;
    } while(pos < (uint32_t) this->buff->index);
}


Bytes::Ptr
Writer::serialize()
{
    // This needs to be a copy because the Bytes::Ptr may
    // outlive the underlying ei_x_buff object if this
    // writer is destroyed.
    //
    // Surely there's a way to fix the lifetime thing but
    // that's a todo.
    //
    // TODO: Fix buffer lifetime issue here to avoid memcpy
    return Bytes::copy((uint8_t*) this->buff->buff, this->buff->index);
}


void
Writer::write(bool val)
{
    if(ei_x_encode_boolean(this->buff.get(), val) != 0) {
        throw EastonException("Unable to encode boolean value.");
    }
}


void
Writer::write(const char* val)
{
    if(ei_x_encode_atom(this->buff.get(), val) != 0) {
        throw EastonException("Unable to encode atom value.");
    }
}


void
Writer::write(int64_t val)
{
    if(ei_x_encode_longlong(this->buff.get(), val) != 0) {
        throw EastonException("Unable to encode int64_t value.");
    }
}


void
Writer::write(uint64_t val)
{
    if(ei_x_encode_ulonglong(this->buff.get(), val) != 0) {
        throw EastonException("Unable to encode uint64_t value.");
    }
}


void
Writer::write(double val)
{
    if(ei_x_encode_double(this->buff.get(), val) != 0) {
        throw EastonException("Unable to encode double value.");
    }
}


void
Writer::write(Bytes::Ptr val)
{
    if(ei_x_encode_binary(this->buff.get(), val->get(), val->size()) != 0) {
        throw EastonException("Unable to encode binary value.");
    }
}


void
Writer::start_tuple(int32_t arity)
{
    if(ei_x_encode_tuple_header(this->buff.get(), arity) != 0) {
        throw EastonException("Unable to encode tuple header.");
    }
}


void
Writer::start_list(int32_t arity)
{
    if(ei_x_encode_list_header(this->buff.get(), arity) != 0) {
        throw EastonException("Unable to encode list header.");
    }
}


void
Writer::write_empty_list()
{
    if(ei_x_encode_empty_list(this->buff.get()) != 0) {
        throw EastonException("Unable to encode empty list.");
    }
}


Storage::Ptr
Storage::create(std::string dirname)
{
    return Ptr(new Storage(dirname));
}


Storage::Storage(std::string dirname)
{
    this->dirname = dirname;

    this->o_opts.create_if_missing = true;
    this->o_opts.paranoid_checks = true;
    this->o_opts.filter_policy = leveldb::NewBloomFilterPolicy(10);

    this->r_opts.verify_checksums = true;
    this->r_opts.fill_cache = true;

    this->w_opts.sync = true;

    leveldb::Status s = leveldb::DB::Open(this->o_opts, dirname, &this->db);
    if(!s.ok()) {
        throw EastonException("Error opening storage layer: " + s.ToString());
    }

    this->sm.context = (void*) this;
    this->sm.flushCallback = sm_flush_cb;
    this->sm.loadByteArrayCallback = sm_load_cb;
    this->sm.storeByteArrayCallback = sm_store_cb;
    this->sm.deleteByteArrayCallback = sm_delete_cb;

    this->geoid_num_key = this->make_key("meta", "geoid_num");
    io::Bytes::Ptr val = this->get_kv(this->geoid_num_key);

    if(!val) {
        this->geoid_num = 0;
    } else {
        io::Reader::Ptr reader = io::Reader::create(val);

        if(!reader->read(this->geoid_num)) {
            throw EastonException("Error reading geo id number.");
        }
    }
}


Storage::~Storage()
{
    delete this->db;
}


uint64_t
Storage::data_size()
{
    leveldb::Range rng("", "\xFF\xFF\xFF\xFF");
    uint64_t size;

    this->db->GetApproximateSizes(&rng, 1, &size);

    return size;
}


io::Bytes::Ptr
Storage::make_key(const char* tag, const char* val)
{
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write(tag);
    writer->write(val);
    return writer->serialize();
}


io::Bytes::Ptr
Storage::make_key(const char* tag, io::Bytes::Ptr val)
{
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write(tag);
    writer->write(val);
    return writer->serialize();
}


Bytes::Ptr
Storage::get_kv(Bytes::Ptr key)
{
    Transaction::Ptr t = this->tx.lock();

    if(t) {
        try {
            return t->get_kv(key);
        } catch (const std::out_of_range& e) {
            // Fallback to the underlying data
            // storage if the key isn't in the
            // current transaction.
        }
    }

    std::string val;
    leveldb::Status s = this->db->Get(this->r_opts, key->slice(), &val);
    if(!s.ok() && !s.IsNotFound()) {
        throw EastonException("Error retrieving key: " + s.ToString());
    }

    if(s.IsNotFound()) {
        return Bytes::Ptr();
    }

    return Bytes::copy((uint8_t*) val.data(), val.size());
}


void
Storage::put_kv(Bytes::Ptr key, Bytes::Ptr val)
{
    Transaction::Ptr t = this->tx.lock();

    if(!t) {
        throw EastonException("No transaction in progress.");
    }

    t->put_kv(key, val);
}


void
Storage::del_kv(Bytes::Ptr key)
{
    Transaction::Ptr t = this->tx.lock();

    if(!t) {
        throw EastonException("No transaction in progress.");
    }

    t->del_kv(key);
}


void*
Storage::get_storage_manager()
{
    return &(this->sm);
}


int64_t
Storage::new_geoid()
{
    int64_t ret = this->geoid_num++;

    io::Writer::Ptr writer = io::Writer::create();
    writer->write(this->geoid_num);
    this->put_kv(this->geoid_num_key, writer->serialize());

    return ret;
}


void
Storage::set_transaction(TxMonitor tx)
{
    if(!this->tx.expired()) {
        throw EastonException("Transaction already in progress.");
    }

    this->tx = tx;
}


void
Storage::write(leveldb::WriteBatch* batch)
{
    leveldb::Status s = this->db->Write(this->w_opts, batch);
    if(!s.ok()) {
        throw EastonException("Error writing batch: " + s.ToString());
    }
}


Transaction::Transaction(Storage::Ptr store, bool is_autocommit)
{
    this->store = store;
    this->is_autocommit = is_autocommit;
    this->batch = WBPtr(new leveldb::WriteBatch());
}


Transaction::~Transaction()
{
    if(this->is_autocommit && this->batch) {
        this->commit();
    }
}


Transaction::Ptr
Transaction::open(Storage::Ptr store)
{
    Ptr tx(new Transaction(store, false));
    store->set_transaction(tx);
    return tx;
}


Transaction::Ptr
Transaction::autocommit(Storage::Ptr store)
{
    Ptr tx(new Transaction(store, true));
    store->set_transaction(tx);
    return tx;
}


void
Transaction::commit()
{
    // Copy and reset our batch before attempting
    // to commit. This way if the commit write fails
    // we can't retry the same write accidentally.
    WBPtr b;
    b.swap(this->batch);
    this->store->write(b.get());
    this->rbuf.clear();
}


Bytes::Ptr
Transaction::get_kv(Bytes::Ptr key)
{
    std::string skey((char*) key->get(), key->size());
    return this->rbuf.at(skey);
}


void
Transaction::put_kv(Bytes::Ptr key, Bytes::Ptr val)
{
    if(!this->batch) {
        throw EastonException("Transaction already committed.");
    }

    this->batch->Put(key->slice(), val->slice());
    this->rbuf[std::string((char*) key->get(), key->size())] = val;
}


void
Transaction::del_kv(Bytes::Ptr key)
{
    if(!this->batch) {
        throw EastonException("Transaction already committed.");
    }

    this->batch->Delete(key->slice());
    this->rbuf[std::string((char*) key->get(), key->size())] = Bytes::Ptr();
}


NS_EASTON_IO_END
NS_EASTON_END
