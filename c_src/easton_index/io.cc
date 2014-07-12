
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/stat.h>

#include <string>

#include <leveldb/filter_policy.h>

#include "config.hh"
#include "exceptions.hh"
#include "io.hh"


NS_EASTON_BEGIN
NS_EASTON_IO_BEGIN


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
Bytes::copy(uint8_t* data, uint32_t len)
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
    this->owner = false;
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
    uint32_t ret;

    ret = ::read(EASTON_STREAM_IN, &packet_len, sizeof(uint32_t));
    if(ret == 0) {
        return NULL;
    } else if(ret != sizeof(uint32_t)) {
        throw EastonExit(EASTON_ERROR_BAD_READ);
    }

    packet_len = ntohl(packet_len);

    Bytes::Ptr data = Bytes::create(packet_len);
    ret = ::read(EASTON_STREAM_IN, data->get(), packet_len);
    if(ret != packet_len) {
        throw EastonExit(EASTON_ERROR_BAD_READ);
    }

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
Reader::read(uint64_t& val)
{
    if(ei_decode_ulonglong(
            (char*) this->data->get(), &(this->pos), &val) != 0) {
        return false;
    }

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


Bytes::Ptr
Reader::read_bytes()
{
    int32_t type;
    int32_t bytes;

    if(ei_get_type(
            (char*) this->data->get(), &(this->pos), &type, &bytes) != 0) {
        return NULL;
    }

    if(type != ERL_BINARY_EXT) {
        return NULL;
    }

    Bytes::Ptr b = Bytes::create(bytes);
    if(ei_decode_binary(
            (char*) this->data->get(), &(this->pos),
            b->get(), (long*) &bytes) != 0) {
        return NULL;
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


Writer::Ptr
Writer::create()
{
    return Ptr(new Writer());
}


Writer::Writer()
{
    buff = new ei_x_buff;
    if(ei_x_new_with_version(buff) != 0) {
        throw EastonException("Error initializing Writer buffer.");
    }
}


Writer::~Writer()
{
    if(ei_x_free(buff) != 0) {
        throw EastonException("Error destroying Writer buffer.");
    }

    delete buff;
}


void
Writer::send()
{
    uint32_t packet_len = htonl((uint32_t) this->buff->index);
    uint32_t ret;

    ret = ::write(EASTON_STREAM_OUT, &packet_len, sizeof(uint32_t));
    if(ret != sizeof(uint32_t)) {
        throw EastonExit(EASTON_ERROR_BAD_WRITE);
    }

    ret = ::write(EASTON_STREAM_OUT, this->buff->buff, this->buff->index);
    if(ret != this->buff->index) {
        throw EastonExit(EASTON_ERROR_BAD_WRITE);
    }
}


Bytes::Ptr
Writer::serialize()
{
    return Bytes::create((uint8_t*) this->buff->buff, this->buff->index);
}


void
Writer::write(bool val)
{
    if(ei_x_encode_boolean(this->buff, val) != 0) {
        throw EastonException("Unable to encode boolean value.");
    }
}


void
Writer::write(const char* val)
{
    if(ei_x_encode_atom(this->buff, val) != 0) {
        throw EastonException("Unable to encode atom value.");
    }
}


void
Writer::write(uint64_t val)
{
    if(ei_x_encode_ulonglong(this->buff, val) != 0) {
        throw EastonException("Unable to encode uint64_t value.");
    }
}


void
Writer::write(double val)
{
    if(ei_x_encode_double(this->buff, val) != 0) {
        throw EastonException("Unable to encode double value.");
    }
}


void
Writer::write(Bytes::Ptr val)
{
    if(ei_x_encode_binary(this->buff, val->get(), val->size()) != 0) {
        throw EastonException("Unable to encode binary value.");
    }
}


void
Writer::start_tuple(int32_t arity)
{
    if(ei_x_encode_tuple_header(this->buff, arity) != 0) {
        throw EastonException("Unable to encode tuple header.");
    }
}


void
Writer::start_list(int32_t arity)
{
    if(ei_x_encode_list_header(this->buff, arity) != 0) {
        throw EastonException("Unable to encode list header.");
    }
}


void
Writer::write_empty_list()
{
    if(!ei_x_encode_empty_list(this->buff)) {
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

    this->w_opts.sync = false;

    leveldb::Status s = leveldb::DB::Open(this->o_opts, dirname, &this->db);
    if(!s.ok()) {
        throw EastonException("Error opening storage layer: " + s.ToString());
    }
}


Storage::~Storage()
{
    delete this->db;
}


void
Storage::put_kv(Bytes::Ptr key, Bytes::Ptr val)
{
    leveldb::Status s = this->db->Put(this->w_opts, key->slice(), val->slice());
    if(!s.ok()) {
        throw EastonException("Error storing key: " + s.ToString());
    }
}


Bytes::Ptr
Storage::get_kv(Bytes::Ptr key)
{
    std::string val;
    leveldb::Status s = this->db->Get(this->r_opts, key->slice(), &val);
    if(!s.ok() && !s.IsNotFound()) {
        throw EastonException("Error retrieving key: " + s.ToString());
    }

    if(s.IsNotFound()) {
        return NULL;
    }

    return Bytes::copy((uint8_t*) val.data(), val.size());
}


void
Storage::del_kv(Bytes::Ptr key)
{
    leveldb::Status s = this->db->Delete(this->w_opts, key->slice());
    if(!s.ok()) {
        throw EastonException("Error deleting key: " + s.ToString());
    }
}


NS_EASTON_IO_END
NS_EASTON_END
