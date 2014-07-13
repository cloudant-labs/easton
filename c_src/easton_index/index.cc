
#include <float.h>
#include <stdlib.h>

#include "config.hh"
#include "exceptions.hh"
#include "index.hh"


NS_EASTON_BEGIN


Index::Ptr
Index::create(int argc, const char* argv[])
{
    return Index::Ptr(new Index(argc, argv));
}


Index::Index(int argc, const char* argv[])
{
    if(argc < 3) {
        throw EastonException("Not enough arguments for index creation.");
    }

    this->db_dir = argv[0];
    if(!io::is_dir(this->db_dir)) {
        throw EastonException("Index directory does not exist.");
    }

    this->init_storage();
    this->init_geo_idx(argc, argv);

    this->geo_ctx = geo::Ctx::create();
}


Index::~Index()
{
    Index_Destroy(this->geo_idx);
    this->store.reset();
}


void
Index::sync()
{
    Index_Flush(this->geo_idx);
}


geo::Ctx::Ptr
Index::get_geo_ctx()
{
    return this->geo_ctx;
}


uint64_t
Index::curr_docid_num()
{
    return this->docid_num;
}


uint64_t
Index::doc_count()
{
    double mins[this->dimensions];
    double maxs[this->dimensions];
    uint64_t n;

    for(uint32_t i = 0; i < this->dimensions; i++) {
        mins[i] = -DBL_MAX;
        maxs[i] = DBL_MAX;
    }

    if(Index_Intersects_count(this->geo_idx,
            mins, maxs, this->dimensions, &n) != RT_None) {
        return UINT64_MAX;
    }

    return n;
}


void
Index::put_kv(io::Bytes::Ptr user_key, io::Bytes::Ptr val)
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    io::Bytes::Ptr key = this->store->make_key("user", user_key);
    this->store->put_kv(key, val);
}


io::Bytes::Ptr
Index::get_kv(io::Bytes::Ptr user_key)
{
    io::Bytes::Ptr key = this->store->make_key("user", user_key);
    return this->store->get_kv(key);
}


void
Index::del_kv(io::Bytes::Ptr user_key)
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    io::Bytes::Ptr key = this->store->make_key("user", user_key);
    this->store->del_kv(key);
}


void
Index::update(io::Bytes::Ptr docid, io::Bytes::Vector wkbs)
{
    io::Transaction::Ptr tx = io::Transaction::open(this->store);
    io::Bytes::Ptr dockey = this->store->make_key("docid", docid);
    uint64_t docnum = this->get_docid_num(dockey);
    geo::Bounds::Vector bounds;

    for(io::Bytes::VIter vi = wkbs.begin(); vi != wkbs.end(); vi++) {
        geo::Geom::Ptr geom = this->geo_ctx->from_wkb(*vi);
        if(!geom) {
            throw EastonException("Invalid WKB in update.");
        }
        geo::Bounds::Ptr b = geom->get_bounds();
        if(b->get_dims() != this->dimensions) {
            throw EastonException("Attempted to index mismatched dimensions.");
        }
        bounds.push_back(b);
    }

    io::Bytes::Ptr val = this->make_id_value(docnum, bounds);
    this->store->put_kv(dockey, val);

    for(uint32_t i = 0; i < wkbs.size(); i++) {
        val = this->make_geo_value(docid, wkbs[i]);

        if(Index_InsertData(this->geo_idx, docnum,
                bounds[i]->mins(), bounds[i]->maxs(),
                this->dimensions, val->get(), val->size()) != RT_None) {
            throw EastonException("Error updating geo index.");
        }
    }

    tx->commit();
}


void
Index::remove(io::Bytes::Ptr docid)
{
    io::Transaction::Ptr tx = io::Transaction::open(this->store);
    io::Bytes::Ptr dockey = this->store->make_key("docid", docid);
    io::Bytes::Ptr val = this->store->get_kv(dockey);
    geo::Bounds::Vector bounds;
    uint64_t docnum = this->read_id_value(val, bounds);

    for(geo::Bounds::VIter vi = bounds.begin(); vi != bounds.end(); vi++) {
        if(Index_DeleteData(this->geo_idx, docnum, (*vi)->mins(), (*vi)->maxs(),
                this->dimensions) != RT_None) {
            throw EastonException("Error removing document.");
        }
    }

    this->store->del_kv(dockey);
    tx->commit();
}


std::vector<Index::Result>
Index::search(geo::Bounds::Ptr query, bool nearest)
{
    RTError err;
    IndexItemH* items;
    uint64_t num_items;

    if(nearest) {
        err = Index_NearestNeighbors_obj(this->geo_idx,
                query->mins(), query->maxs(),
                this->dimensions, &items, &num_items);
    } else {
        err = Index_Intersects_obj(this->geo_idx,
                query->mins(), query->maxs(),
                this->dimensions, &items, &num_items);
    }

    if(err != RT_None) {
        throw EastonException("Error executing search.");
    }

    io::Bytes::Ptr docid;
    io::Bytes::Ptr wkb;
    std::vector<Result> ret;

    for(uint32_t i = 0; i < num_items; i++) {
        this->read_geo_value(items[i], docid, wkb);
        ret.push_back(Result(docid, wkb));
    }

    return ret;
}


void
Index::init_storage()
{
    this->store = io::Storage::create(this->db_dir);
    this->docid_num_key = this->store->make_key("meta", "docid_num");
    io::Bytes::Ptr val = this->store->get_kv(this->docid_num_key);

    if(!val) {
        this->docid_num = 0;
    } else {
        io::Reader::Ptr reader = io::Reader::create(val);

        if(!reader->read(this->docid_num)) {
            throw EastonException("Error reading document id number.");
        }
    }
}


void
Index::init_geo_idx(int argc, const char* argv[])
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    IndexPropertyH props = IndexProperty_Create();

    int64_t idx_type = atoi(argv[1]);
    int64_t dims = atoi(argv[2]);

    RTIndexType it;

    this->dimensions = dims;

    if(IndexProperty_SetIndexStorage(props, RT_Custom) != RT_None) {
        throw EastonException("Error setting index storage type.");
    }

    uint32_t sism_sz = sizeof(SpatialIndexStorageManager);
    if(IndexProperty_SetCustomStorageCallbacksSize(props, sism_sz) != RT_None) {
        throw EastonException("Error setting storage callbacks size.");
    }

    void* sism = this->store->get_storage_manager();
    if(IndexProperty_SetCustomStorageCallbacks(props, sism) != RT_None) {
        throw EastonException("Error setting storage callbacks.");
    }

    if(idx_type == EASTON_INDEX_TYPE_RTREE) {
        it = RT_RTree;
    } else if(idx_type == EASTON_INDEX_TYPE_TPRTREE) {
        it = RT_TPRTree;
    } else {
        throw EastonException("Invalid geo index type.");
    }

    if(IndexProperty_SetIndexType(props, it) != RT_None) {
        throw EastonException("Error setting geo index type.");
    }

    if(IndexProperty_SetDimension(props, (uint32_t) dims) != RT_None) {
        throw EastonException("Error setting geo index dimensions.");
    }

    // Setting the ResultSetLimit to 0 disables paging in
    // libspatialindex so that we can implement our own
    // after the fact. Internally libspatialindex already
    // loads the entire result set including false positives
    // into RAM so this isn't any less efficient.

    if(IndexProperty_SetResultSetLimit(props, 0) != RT_None) {
        throw EastonException("Error setting geo index result set limit.");
    }

    this->geo_idx = Index_Create(props);
    if(this->geo_idx == NULL) {
        throw EastonException("Error creating geo index.");
    }

    if(!Index_IsValid(this->geo_idx)) {
        throw EastonException("Created an invalid geo index.");
    }

    IndexProperty_Destroy(props);

    // Verify the properties after open are the
    // same as requested.

    props = Index_GetProperties(this->geo_idx);

    // For some reason the properties returned from the
    // tree don't set the index type.
    //
    // if(IndexProperty_GetIndexType(props) != it) {
    //     exit(EASTON_ERROR_BAD_GEO_IDX_CFG);
    // }

    if(IndexProperty_GetDimension(props) != this->dimensions) {
        throw EastonException("Dimension mismatch with existing geo index.");
    }

    IndexProperty_Destroy(props);
}


uint64_t
Index::get_docid_num(io::Bytes::Ptr docid_key)
{
    io::Bytes::Ptr val = this->store->get_kv(docid_key);
    uint64_t ret;

    if(!val) {
        ret = this->docid_num++;
    } else {
        io::Reader::Ptr reader = io::Reader::create(val);
        if(!reader->read(ret)) {
            throw EastonException("Unable to read existing document number.");
        }
    }

    // Store the new current doc id number in the index
    io::Writer::Ptr writer = io::Writer::create();
    writer->write(ret);
    val = writer->serialize();
    this->store->put_kv(this->docid_num_key, val);

    return ret;
}


io::Bytes::Ptr
Index::make_id_value(uint64_t docnum, geo::Bounds::Vector bounds)
{
    io::Writer::Ptr writer = io::Writer::create();

    writer->write(docnum);
    writer->write((uint64_t) bounds.size());

    for(uint32_t i = 0; i < bounds.size(); i++) {
        for(uint32_t j = 0; j < this->dimensions; j++) {
            writer->write(bounds[i]->mins()[j]);
        }
        for(uint32_t j = 0; j < this->dimensions; j++) {
            writer->write(bounds[i]->maxs()[j]);
        }
    }

    return writer->serialize();
}


uint64_t
Index::read_id_value(io::Bytes::Ptr value, geo::Bounds::Vector& bounds)
{
    io::Reader::Ptr reader = io::Reader::create(value);
    uint64_t docnum;
    uint64_t num_bounds;
    double dbl;

    if(!reader->read(docnum)) {
        throw EastonException("Error reading id index value: docnum");
    }

    if(!reader->read(num_bounds)) {
        throw EastonException("Error reading id index value: num_bounds");
    }

    bounds.clear();

    for(uint64_t i = 0; i < num_bounds; i++) {
        geo::Bounds::Ptr b = geo::Bounds::create(this->dimensions);
        for(uint32_t j = 0; j < this->dimensions; j++) {
            if(!reader->read(dbl)) {
                throw EastonException("Error reading id index value: bounds");
            }
            b->set_min(j, dbl);
        }
        for(uint32_t j = 0; j < this->dimensions; j++) {
            if(!reader->read(dbl)) {
                throw EastonException("Error reading id index value: bounds");
            }
            b->set_max(j, dbl);
        }
        bounds.push_back(b);
    }

    return docnum;
}


io::Bytes::Ptr
Index::make_geo_value(io::Bytes::Ptr docid, io::Bytes::Ptr wkb)
{
    io::Writer::Ptr writer = io::Writer::create();

    writer->write(docid);
    writer->write(wkb);

    return writer->serialize();
}


void
Index::read_geo_value(IndexItemH item,
        io::Bytes::Ptr& docid, io::Bytes::Ptr& wkb)
{
    uint8_t* data;
    uint64_t len;

    if(IndexItem_GetData(item, (uint8_t **) &data, &len) != RT_None) {
        throw EastonException("Error getting data from geo index entry.");
    }

    io::Reader::Ptr reader = io::Reader::create(io::Bytes::create(data, len));

    docid = reader->read_bytes();
    wkb = reader->read_bytes();
}


NS_EASTON_END
