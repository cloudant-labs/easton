
#include <stdlib.h>

#include "config.hh"
#include "exceptions.hh"
#include "index.hh"


#define DOC_ID_NUM_KEY "meta:doc_id_num"


NS_EASTON_BEGIN


Index::Ptr
Index::create(int argc, const char* argv[])
{
    return Index::Ptr(new Index(argc, argv));
}


Index::Index(int argc, const char* argv[])
{
    if(argc < 4) {
        throw EastonException("Not enough arguments for index creation.");
    }

    this->base_dir = argv[1];
    if(!io::is_dir(this->base_dir)) {
        throw EastonException("Index directory does not exist.");
    }

    this->geo_file = (this->base_dir + "/") + EASTON_FILE_GEO_IDX;

    this->init_storage();
    this->init_geo_idx(argc, argv);

    this->geo_util = geo::Util::create();
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


uint64_t
Index::curr_docid_num()
{
    return this->doc_id_num;
}


uint64_t
Index::doc_count()
{
    double* mins;
    double* maxs;
    uint32_t dims;
    uint64_t n;
    uint64_t ret = UINT64_MAX;

    if(Index_GetBounds(this->geo_idx, &mins, &maxs, &dims) != RT_None) {
        goto done;
    }

    if(Index_Intersects_count(this->geo_idx, mins, maxs, dims, &n) != RT_None) {
        goto done;
    }

    ret = n;

done:

    if(mins != NULL) {
        free(mins);
    }

    if(maxs != NULL) {
        free(maxs);
    }

    if(ret == UINT64_MAX) {
        throw EastonException("Error getting document count.");
    }

    return ret;
}


void
Index::put_kv(io::Bytes::Ptr key, io::Bytes::Ptr val)
{
    this->store->put_kv(key, val);
}


io::Bytes::Ptr
Index::get_kv(io::Bytes::Ptr key)
{
    return this->store->get_kv(key);
}


void
Index::del_kv(io::Bytes::Ptr key)
{
    this->store->del_kv(key);
}


void
Index::update(io::Bytes::Ptr docid, io::Bytes::Vector wkbs)
{
    io::Bytes::Ptr dockey = this->make_dockey(docid);
    uint64_t docnum = this->get_doc_num(dockey);
    geo::Bounds::Vector bounds;

    docnum = this->get_doc_num(dockey);

    for(io::Bytes::VIter vi = wkbs.begin(); vi != wkbs.end(); vi++) {
        geo::Bounds::Ptr b = this->geo_util->get_bounds(*vi, this->dimensions);
        bounds.push_back(b);
    }

    io::Bytes::Ptr val = this->make_id_value(docnum, bounds);
    this->put_kv(dockey, val);

    for(uint32_t i = 0; i < wkbs.size(); i++) {
        val = this->make_geo_value(docid, wkbs[i]);

        if(Index_InsertData(this->geo_idx, docnum,
                bounds[i]->mins(), bounds[i]->maxs(),
                this->dimensions, val->get(), val->size()) != RT_None) {
            throw EastonException("Error updating geo index.");
        }
    }
}


void
Index::remove(io::Bytes::Ptr docid)
{
    io::Bytes::Ptr dockey = this->make_dockey(docid);
    io::Bytes::Ptr val = this->get_kv(dockey);
    geo::Bounds::Vector bounds;
    uint64_t docnum = this->read_id_value(val, bounds);

    for(geo::Bounds::VIter vi = bounds.begin(); vi != bounds.end(); vi++) {
        if(Index_DeleteData(this->geo_idx, docnum, (*vi)->mins(), (*vi)->maxs(),
                this->dimensions) != RT_None) {
            throw EastonException("Error removing document.");
        }
    }

    this->del_kv(dockey);
}


std::vector<Index::Result>
Index::query(geo::Bounds::Ptr query, bool nearest)
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
        throw EastonException("Error executing query.");
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
    io::Bytes::Ptr key = io::Bytes::proxy(DOC_ID_NUM_KEY);
    io::Bytes::Ptr val;

    this->store = io::Storage::create(this->base_dir);
    val = this->store->get_kv(key);

    if(!val) {
        this->doc_id_num = 0;
    } else {
        io::Reader::Ptr reader = io::Reader::create(val);

        if(!reader->read(this->doc_id_num)) {
            throw EastonException("Error reading document id number.");
        }
    }
}


void
Index::init_geo_idx(int argc, const char* argv[])
{
    IndexPropertyH props = IndexProperty_Create();

    int64_t idx_type = atoi(argv[2]);
    int64_t dims = atoi(argv[3]);

    RTIndexType it;

    this->dimensions = dims;

    if(IndexProperty_SetFileName(props, this->geo_file.c_str()) != RT_None) {
        throw EastonException("Error setting geo index filename.");
    }

    if(IndexProperty_SetIndexStorage(props, RT_Disk) != RT_None) {
        throw EastonException("Error setting index storage type.");
    }

    if(IndexProperty_SetOverwrite(props, 0) != RT_None) {
        throw EastonException("Error setting overwrite property.");
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

    // Setting the limit and offset below to UINT64_MAX is so
    // that we can do our own paging. Underneat the covers
    // libspatialindex is loading all results into RAM and then
    // has a post-processing step. Rather than futz around trying
    // to page things propery we'll just disable its paging
    // and use our own.

    if(IndexProperty_SetResultSetLimit(props, UINT64_MAX) != RT_None) {
        throw EastonException("Error setting geo index result set limit.");
    }

    this->geo_idx = Index_Create(props);
    if(this->geo_idx == NULL) {
        throw EastonException("Error creating geo index.");
    }

    if(!Index_IsValid(this->geo_idx)) {
        throw EastonException("Created an invalid geo index.");
    }

    Index_SetResultSetOffset(this->geo_idx, UINT64_MAX);

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


io::Bytes::Ptr
Index::make_dockey(io::Bytes::Ptr docid)
{
    // Dirty. I need to make the io::Bytes API moar better.
    const char* prefix = "docid:";
    io::Bytes::Ptr ret = io::Bytes::create(docid->size() + strlen(prefix));
    memcpy(ret->get(), prefix, strlen(prefix));
    memcpy(ret->get() + strlen(prefix), docid->get(), docid->size());
    return ret;
}


uint64_t
Index::get_doc_num(io::Bytes::Ptr dockey)
{
    io::Bytes::Ptr val = this->get_kv(dockey);
    uint64_t ret;

    if(!val) {
        ret = this->doc_id_num++;
    } else {
        io::Reader::Ptr reader = io::Reader::create(val);
        if(!reader->read(ret)) {
            throw EastonException("Unable to read existing document number.");
        }
    }

    // Store the new current doc id number in the index
    io::Writer::Ptr writer = io::Writer::create();
    writer->write(ret);

    io::Bytes::Ptr key = io::Bytes::proxy(DOC_ID_NUM_KEY);
    val = writer->serialize();
    this->put_kv(key, val);

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
