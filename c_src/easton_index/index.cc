
#include <float.h>
#include <stdlib.h>

#include "config.hh"
#include "exceptions.hh"
#include "index.hh"


NS_EASTON_BEGIN


std::string
sidx_error()
{
    char* err = Error_GetLastErrorMsg();
    std::string m(err);
    free(err);
    return m;
}


IndexPropertyH
sidx_properties(IndexH index)
{
    ::Index* idx = static_cast<::Index*>(index);
    Tools::PropertySet* ps = new Tools::PropertySet;

    *ps = idx->GetProperties();
    return (IndexPropertyH)ps;
}


Hit::Hit()
{
    this->distance = -1.0;
}


Hit::Hit(io::Bytes::Ptr docid, geo::Geom::Ptr geom, double distance)
{
    this->docid = docid;
    this->geom = geom;
    this->distance = distance;
}


bool
HitCmp::operator()(Hit const &h1, Hit const &h2) {
    // First order by distance.

    if(h1.distance <= h2.distance) {
        return true;
    }

    if(h1.distance > h2.distance) {
        return false;
    }

    // Use the document ID to break ties. Empty
    // document IDs sort first.

    if(!h1.docid) {
        return true;
    }

    if(!h2.docid) {
        return false;
    }

    // Raw byte comparisons here since I don't
    // feel like pulling in ICU as a dependency.

    uint32_t d1 = h1.docid->size();
    uint32_t d2 = h2.docid->size();
    uint32_t len = d1 < d2 ? d1 : d2;
    int r = memcmp(h1.docid->get(), h2.docid->get(), len);

    return r <= 0;
}


TopHits::TopHits(Hit bookmark, geo::GeomFilter filt, uint32_t limit)
{
    this->bookmark = bookmark;
    this->filt = filt;
    this->limit = limit;
}


TopHits::~TopHits()
{
}


double
TopHits::distance(io::Bytes::Ptr docid, geo::Geom::Ptr geom)
{
    if(!this->filt(geom)) {
        return DBL_MAX;
    }

    double dist = this->filt.distance(geom);

    // Create a hit object so we can compare against our
    // configured bookmark.
    Hit maybe_hit(docid, geom, dist);

    // Ignore anything closer than our configured
    // bookmark.
    if(this->cmp(maybe_hit, this->bookmark)) {
        return DBL_MAX;
    }

    return dist;
}


uint32_t
TopHits::size()
{
    return this->hits.size();
}


void
TopHits::push(io::Bytes::Ptr docid, geo::Geom::Ptr geom)
{
    // This logic is admittedly a bit weird. We're relying
    // on TopHits::distance to return DBL_MAX for anything
    // we don't want to include in the result set. The
    // reason for this is so that we can reuse the distance
    // function for nearest neighbor queries and still
    // return properly paged result sets.

    double dist = this->distance(docid, geom);

    if(dist == DBL_MAX) {
        return;
    }

    Hit hit(docid, geom, dist);

    if(this->hits.size() < limit) {
        this->hits.push(hit);
    } else {
        if(this->cmp(hit, this->hits.top())) {
            this->hits.pop();
            this->hits.push(hit);
        }
    }
}


Hit
TopHits::pop()
{
    Hit ret = hits.top();
    hits.pop();
    return ret;
}


NNComparator::NNComparator(geo::Ctx::Ptr ctx, TopHits& hits) : hits(hits)
{
    this->ctx = ctx;
}


double
NNComparator::getMinimumDistance(const SpatialIndex::IShape& query,
        const SpatialIndex::IShape& e)
{
    return query.getMinimumDistance(e);
}


double
NNComparator::getMinimumDistance(const SpatialIndex::IShape& query,
        const SpatialIndex::IData& d)
{
    uint8_t* data;
    uint32_t size;

    d.getData(size, &data);

    io::Bytes::Ptr buf = io::Bytes::proxy(data, size);
    io::Reader::Ptr reader = io::Reader::create(buf);

    io::Bytes::Ptr docid = reader->read_bytes();
    io::Bytes::Ptr wkb = reader->read_bytes();
    geo::Geom::Ptr geom = this->ctx->from_wkb(wkb);

    return this->hits.distance(docid, geom);
}


EntryVisitor::EntryVisitor(geo::Ctx::Ptr ctx, TopHits& hits) : hits(hits)
{
    this->ctx = ctx;
}


EntryVisitor::~EntryVisitor()
{
}


void
EntryVisitor::visitNode(const SpatialIndex::INode& in)
{
}


void
EntryVisitor::visitData(const SpatialIndex::IData& d)
{
    uint8_t* data;
    uint32_t size;

    d.getData(size, &data);

    io::Bytes::Ptr buf = io::Bytes::proxy(data, size);
    io::Reader::Ptr reader = io::Reader::create(buf);

    io::Bytes::Ptr docid = reader->read_bytes();
    io::Bytes::Ptr wkb = reader->read_bytes();
    geo::Geom::Ptr geom = this->ctx->from_wkb(wkb);

    this->hits.push(docid, geom);
}


void
EntryVisitor::visitData(std::vector<const SpatialIndex::IData*>& v)
{
}


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
    this->init_srid(argv[3]);

    this->geo_ctx = geo::Ctx::create(this->dimensions, this->srid);
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

    // Remove any results currently in the index for
    // this docid.
    this->remove_int(docid);

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
    this->remove_int(docid);
    tx->commit();
}


void
Index::remove_int(io::Bytes::Ptr docid)
{
    io::Bytes::Ptr dockey = this->store->make_key("docid", docid);
    io::Bytes::Ptr val = this->store->get_kv(dockey);

    if(!val) {
        return;
    }

    geo::Bounds::Vector bounds;
    uint64_t docnum = this->read_id_value(val, bounds);

    for(geo::Bounds::VIter vi = bounds.begin(); vi != bounds.end(); vi++) {
        if(Index_DeleteData(this->geo_idx, docnum, (*vi)->mins(), (*vi)->maxs(),
                this->dimensions) != RT_None) {
            throw EastonException("Error removing document.");
        }
    }

    this->store->del_kv(dockey);
}


void
Index::search(TopHits& collector, geo::Bounds::Ptr query, bool nearest)
{
    ::Index* idx = static_cast<::Index*>(this->geo_idx);
    EntryVisitor visitor(this->geo_ctx, collector);
    uint32_t lim = collector.limit;

    try {
        SpatialIndex::Region r(query->mins(), query->maxs(), query->get_dims());

        if(nearest) {
            NNComparator nnc(this->geo_ctx, collector);
            idx->index().nearestNeighborQuery(lim, r, visitor, nnc);
        } else {
            idx->index().intersectsWithQuery(r, visitor);
        }
    } catch(Tools::Exception& e) {
        throw IndexException("Search error: " + e.what());
    }
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

    this->load_index_id();
    if(this->index_id != SpatialIndex::StorageManager::NewPage) {
        if(IndexProperty_SetIndexID(props, this->index_id) != RT_None) {
            throw EastonException("Error setting index id.");
        }
    }

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

    if(IndexProperty_SetWriteThrough(props, 1) != RT_None) {
        throw EastonException("Error setting write through.");
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
        throw IndexException("Geo Index Error: " + sidx_error());
    }

    if(!Index_IsValid(this->geo_idx)) {
        throw IndexException("Invalid Geo Index: " + sidx_error());
    }

    IndexProperty_Destroy(props);

    // Verify the properties after open are the
    // same as requested.

    props = sidx_properties(this->geo_idx);

    if(IndexProperty_GetDimension(props) != this->dimensions) {
        throw EastonException("Dimension mismatch with existing geo index.");
    }

    if(this->index_id == SpatialIndex::StorageManager::NewPage) {
        this->index_id = IndexProperty_GetIndexID(props);
        if(this->index_id == SpatialIndex::StorageManager::NewPage) {
            throw EastonException("Error retrieving index id.");
        }
        this->store_index_id();
    }

    IndexProperty_Destroy(props);
}


void
Index::init_srid(const char* srid_str)
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    this->srid = atoi(srid_str);

    // Verify that we have the same SRID if we're
    // reopening the index.
    io::Bytes::Ptr key = this->store->make_key("meta", "srid");
    io::Bytes::Ptr val = this->store->get_kv(key);

    if(!val) {
        // This is the first time we've opened the index.
        // Store the SRID and call it a day.
        io::Writer::Ptr writer = io::Writer::create();
        uint64_t u_srid = this->srid;
        writer->write(u_srid);
        this->store->put_kv(key, writer->serialize());
        return;
    }

    io::Reader::Ptr reader = io::Reader::create(val);
    int64_t old_srid;
    if(!reader->read(old_srid)) {
        throw EastonException("Error reading stored SRID.");
    }

    if(this->srid != old_srid) {
        throw EastonException("Invalid SRID.");
    }
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


void
Index::load_index_id()
{
    io::Bytes::Ptr key = this->store->make_key("meta", "index_id");
    io::Bytes::Ptr val = this->store->get_kv(key);

    if(!val) {
        this->index_id = SpatialIndex::StorageManager::NewPage;
    } else {
        io::Reader::Ptr reader = io::Reader::create(val);
        if(!reader->read(this->index_id)) {
            throw EastonException("Error reading index id.");
        }
    }
}


void
Index::store_index_id()
{
    io::Bytes::Ptr key = this->store->make_key("meta", "index_id");
    io::Writer::Ptr writer = io::Writer::create();
    writer->write(this->index_id);
    this->store->put_kv(key, writer->serialize());
}


NS_EASTON_END
