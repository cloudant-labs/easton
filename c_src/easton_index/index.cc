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

#include <float.h>
#include <stdlib.h>

#include "config.hh"
#include "exceptions.hh"
#include "index.hh"


NS_EASTON_BEGIN


typedef std::shared_ptr<sidx::IShape> SidxShapePtr;


SidxShapePtr
sidx_get_shape(const sidx::IEntry* entry)
{
    sidx::IShape* s;
    entry->getShape(&s);
    return SidxShapePtr(s);
}


SidxShapePtr
sidx_get_child_shape(const sidx::INode* node, uint32_t child)
{
    sidx::IShape* s;
    node->getChildShape(child, &s);
    return SidxShapePtr(s);
}


std::string
sidx_error()
{
    char* err = Error_GetLastErrorMsg();
    std::string m;

    if(err != NULL) {
        m = std::string(err);
    } else {
        m = std::string("");
    }

    free(err);
    return m;
}


IndexPropertyH
sidx_properties(IndexH index)
{
    ::Index* idx = (::Index*) index;
    Tools::PropertySet* ps = new Tools::PropertySet;

    *ps = idx->GetProperties();
    return (IndexPropertyH)ps;
}


Hit::Hit()
{
    this->docid = io::Bytes::create(0);
    this->distance = -1.0;
}


Hit::Hit(io::Bytes::Ptr docid, geo::Geom::Ptr geom, double distance)
{
    this->docid = docid;
    this->geom = geom;
    this->distance = distance;
}


bool
operator<(const Hit &h1, const Hit &h2) {
    // First order by distance.

    if(h1.distance < h2.distance) {
        return true;
    }

    if(h1.distance > h2.distance) {
        return false;
    }

    // Raw byte comparisons here since I don't
    // feel like pulling in ICU as a dependency.

    uint32_t d1 = h1.docid->size();
    uint32_t d2 = h2.docid->size();
    uint32_t len = d1 < d2 ? d1 : d2;

    // Check for empty ids

    if(d1 == 0 && d2 == 0) {
        return false;
    } else if(d1 == 0) {
        return true;
    } else if(d2 == 0) {
        return false;
    }

    int r = memcmp(h1.docid->get(), h2.docid->get(), len);

    if(r < 0) {
        return true;
    } else if(r == 0) {
        return d1 < d2;
    }

    return false;
}

bool
operator<=(const Hit &h1, const Hit &h2) {
    if(h1 < h2) {
        return true;
    }

    if(h2 < h1) {
        return false;
    }

    return true;
}


TopHits::TopHits(Hit bookmark, geo::GeomFilter filt, uint32_t limit)
{
    this->bookmark = bookmark;
    this->filt = filt;
    this->limit = limit;

    this->nodes_visited = 0;
    this->leaves_visited = 0;
    this->leaves_filtered = 0;
    this->leaves_dropped = 0;
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
    if(maybe_hit <= this->bookmark) {
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
        this->leaves_filtered += 1;
        return;
    }

    Hit hit(docid, geom, dist);

    if(this->hits.size() < limit) {
        this->hits.push(hit);
    } else if(hit < this->hits.top()) {
        this->hits.pop();
        this->hits.push(hit);
    } else {
        this->leaves_dropped += 1;
    }
}


Hit
TopHits::pop()
{
    Hit ret = hits.top();
    hits.pop();
    return ret;
}


void
TopHits::visit_node()
{
    this->nodes_visited += 1;
}


void
TopHits::visit_data()
{
    this->leaves_visited += 1;
}


Entry::~Entry()
{
}


Entry::Ptr
SpatialEntry::read_id(io::Reader::Ptr reader)
{
    geo::Bounds::Ptr bounds = geo::Bounds::read(reader);

    return Entry::Ptr(new SpatialEntry(bounds));
}


Entry::Ptr
SpatialEntry::read_geo(io::Reader::Ptr reader, geo::Ctx::Ptr ctx,
    io::Bytes::Ptr& docid)
{
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Error reading spatial entry geo tuple.");
    }

    docid = reader->read_bytes();
    if(!docid) {
        throw EastonException("Error reading doc id from geo spatial entry.");
    }

    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Error reading wkb from geo spatial entry.");
    }

    geo::Geom::Ptr geom = ctx->from_wkb(wkb);
    if(!geom) {
        throw EastonException("Error creating geometry from spatial entry.");
    }

    return Entry::Ptr(new SpatialEntry(wkb, geom));
}


Entry::Ptr
SpatialEntry::read_update(io::Reader::Ptr reader, geo::Ctx::Ptr ctx)
{
    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Error reading WKB.");
    }

    geo::Geom::Ptr geom = ctx->from_wkb(wkb);
    if(!geom) {
        throw EastonException("Error getting geometry from WKB.");
    }

    return Entry::Ptr(new SpatialEntry(wkb, geom));
}


Entry::Ptr
SpatialEntry::read_query(io::Reader::Ptr reader, geo::Ctx::Ptr ctx,
        geo::SRID::Ptr srid)
{
    geo::Geom::Ptr geom = ctx->geom_from_reader(reader, srid);
    if(!geom) {
        throw EastonException("Error reading query geometry.");
    }

    return Entry::Ptr(new SpatialEntry(io::Bytes::Ptr(), geom));
}


SpatialEntry::SpatialEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom)
{
    this->wkb = wkb;
    this->geom = geom;

    this->bbox = this->geom->get_bounds();
    if(!this->bbox) {
        throw EastonException("Error getting bounds from geometry.");
    }
}


SpatialEntry::SpatialEntry(geo::Bounds::Ptr bbox)
{
    this->bbox = bbox;
}


SpatialEntry::~SpatialEntry()
{
}


void
SpatialEntry::write_id(io::Writer::Ptr writer)
{
    if(!this->bbox) {
        throw EastonException("Invalid spatial entry ID serialization.");
    }

    this->bbox->write(writer);
}


void
SpatialEntry::update(Index* idx, io::Bytes::Ptr docid,
        uint64_t docnum, uint64_t dimensions)
{
    if(!this->wkb) {
        throw EastonException("Invalid spatial entry wkb for index update.");
    }

    if(!this->bbox) {
        throw EastonException("Invalid spatial entry bbox for index update.");
    }

    if(dimensions != this->bbox->get_dims()) {
        throw EastonException("Invalid spatial entry bbox dimensions.");
    }

    // Create the value for the geo entry
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write(docid);
    writer->write(this->wkb);
    io::Bytes::Ptr val = writer->serialize();

    if(Index_InsertData(
                idx->get_index(),
                docnum,
                this->bbox->mins(),
                this->bbox->maxs(),
                this->bbox->get_dims(),
                val->get(),
                val->size()
            ) != RT_None) {
        throw EastonException("Error inserting spatial entry.");
    }
}


void
SpatialEntry::remove(Index* idx, uint64_t docnum, uint64_t dimensions)
{
    if(!this->bbox) {
        throw EastonException("Invalid spatial entry for removal.");
    }

    if(dimensions != this->bbox->get_dims()) {
        throw EastonException("Invalid spatial entry bbox dimensions.");
    }

    if(Index_DeleteData(
                idx->get_index(),
                docnum,
                this->bbox->mins(),
                this->bbox->maxs(),
                this->bbox->get_dims()
            ) != RT_None) {
        throw EastonException("Error removing spatial entry.");
    }
}


void
SpatialEntry::search(Index* idx, TopHits& collector, bool nearest)
{
    ::Index* index = (::Index*) idx->get_index();

    SpatialIndex::Region r(
            this->bbox->mins(),
            this->bbox->maxs(),
            this->bbox->get_dims()
        );

    EntryVisitor visitor(idx->get_geo_ctx(), idx->get_reader(), collector);

    if(nearest) {
        NNComparator nnc(idx->get_geo_ctx(), idx->get_reader(), collector);
        NNRTreePager pager(&visitor, &r, &nnc);
        index->index().queryStrategy(pager);
    } else {
        RTreePager pager(&visitor, &r);
        index->index().queryStrategy(pager);
    }
}


geo::Geom::Ptr
SpatialEntry::get_geometry()
{
    if(!this->geom) {
        throw EastonException("Invalid spatial entry has no geometry.");
    }

    return this->geom;
}


geo::GeomFilter
SpatialEntry::make_filter(geo::Ctx::Ptr ctx, uint64_t filter)
{
    if(!this->geom) {
        throw EastonException("Invalid spatial entry has no geometry.");
    }

    return ctx->make_filter(this->geom, filter);
}


Entry::Ptr
TemporalEntry::read_id(io::Reader::Ptr reader)
{
    if(!reader->read_tuple_n(4)) {
        throw EastonException("Invalid temporal entry id tuple.");
    }

    geo::Bounds::Ptr bbox = geo::Bounds::read(reader);

    if(!bbox) {
        throw EastonException("Invalid bbox for temporal entry id.");
    }

    geo::Bounds::Ptr vbox = geo::Bounds::read(reader);

    if(!vbox) {
        throw EastonException("Invalid vbox for temporal entry id.");
    }

    double t_start;
    double t_end;

    if(!reader->read(t_start)) {
        throw EastonException("Invalid temporal entry id start time.");
    }

    if(!reader->read(t_end)) {
        throw EastonException("Invalid temporal entry id end time.");
    }

    return Entry::Ptr(new TemporalEntry(bbox, vbox, t_start, t_end));
}


Entry::Ptr
TemporalEntry::read_geo(io::Reader::Ptr reader, geo::Ctx::Ptr ctx,
    io::Bytes::Ptr& docid)
{
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Error reading temporal entry geo tuple.");
    }

    docid = reader->read_bytes();
    if(!docid) {
        throw EastonException("Error reading doc id from geo temporal entry.");
    }

    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Error reading wkb from geo temporal entry.");
    }

    geo::Geom::Ptr geom = ctx->from_wkb(wkb);
    if(!geom) {
        throw EastonException("Error creating geometry from temporal entry.");
    }

    return Entry::Ptr(new TemporalEntry(wkb, geom));
}


Entry::Ptr
TemporalEntry::read_update(io::Reader::Ptr reader, geo::Ctx::Ptr ctx)
{
    int32_t size;
    if(!reader->read_tuple(size)) {
        throw EastonException("Invalid temporal entry update tuple.");
    }

    if(size < 3 || size > 4) {
        throw EastonException("Invaldi temporay entry update tuple size.");
    }

    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Error reading WKB.");
    }

    geo::Geom::Ptr geom = ctx->from_wkb(wkb);
    if(!geom) {
        throw EastonException("Error getting geometry from WKB.");
    }

    double t_start;
    double t_end;

    if(!reader->read(t_start)) {
        throw EastonException("Error reading temporal start time.");
    }

    if(!reader->read(t_end)) {
        throw EastonException("Erorr reading temporal end time.");
    }

    geo::Bounds::Ptr vbox;
    if(size == 4) {
        vbox = geo::Bounds::read(reader);
    } else {
        vbox = ctx->get_zero_bounds();
    }

    if(!vbox) {
        throw EastonException("Invalid vbox for temporal index update.");
    }

    return Entry::Ptr(new TemporalEntry(wkb, geom, vbox, t_start, t_end));
}


Entry::Ptr
TemporalEntry::read_query(io::Reader::Ptr reader, geo::Ctx::Ptr ctx,
        geo::SRID::Ptr srid)
{
    int32_t size;
    if(!reader->read_tuple(size)) {
        throw EastonException("Invalid temporal index query tuple.");
    }

    if(size < 3 || size > 4) {
        throw EastonException("Invalid temporal index tuple size.");
    }

    geo::Geom::Ptr geom = ctx->geom_from_reader(reader, srid);
    if(!geom) {
        throw EastonException("Error reading query geometry.");
    }

    double t_start;
    double t_end;

    if(!reader->read(t_start)) {
        throw EastonException("Invalid temporal query start time.");
    }

    if(!reader->read(t_end)) {
        throw EastonException("Invalid temporal query end time.");
    }

    geo::Bounds::Ptr vbox;
    if(size == 4) {
        vbox = geo::Bounds::read(reader);
    } else {
        vbox = ctx->get_zero_bounds();
    }

    if(!vbox) {
        throw EastonException("Invalid vbox for temporal index update.");
    }

    return Entry::Ptr(
            new TemporalEntry(io::Bytes::Ptr(), geom, vbox, t_start, t_end)
        );
}


TemporalEntry::TemporalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom)
{
    this->wkb = wkb;
    this->geom = geom;

    this->bbox = this->geom->get_bounds();
    if(!this->bbox) {
        throw EastonException("Error getting bounds from geometry.");
    }
}


TemporalEntry::TemporalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom,
        geo::Bounds::Ptr vbox, double t_start, double t_end)
{
    this->wkb = wkb;
    this->geom = geom;
    this->bbox = this->geom->get_bounds();
    this->vbox = vbox;
    this->t_start = t_start;
    this->t_end = t_end;

    if(!this->bbox) {
        throw EastonException("Error getting bounds from geometry.");
    }
}


TemporalEntry::TemporalEntry(geo::Bounds::Ptr bbox, geo::Bounds::Ptr vbox,
        double t_start, double t_end)
{
    this->bbox = bbox;
    this->vbox = vbox;
    this->t_start = t_start;
    this->t_end = t_end;
}


TemporalEntry::~TemporalEntry()
{
}


void
TemporalEntry::write_id(io::Writer::Ptr writer)
{
    if(!this->bbox) {
        throw EastonException("Invalid temporal entry bbox for id.");
    }

    if(!this->vbox) {
        throw EastonException("Invalid temporal entry vbox for id.");
    }

    writer->start_tuple(4);
    this->bbox->write(writer);
    this->vbox->write(writer);
    writer->write(this->t_start);
    writer->write(this->t_end);
}


void
TemporalEntry::update(Index* idx, io::Bytes::Ptr docid,
        uint64_t docnum, uint64_t dimensions)
{
    if(!this->wkb) {
        throw EastonException("Invalid temporal entry wkb for index update.");
    }

    if(!this->bbox) {
        throw EastonException("Invalid temporal entry bbox for index update.");
    }

    if(!this->vbox) {
        throw EastonException("Invalid temporal entry vbox for index update.");
    }

    if(dimensions != this->bbox->get_dims()) {
        throw EastonException("Invalid temporal entry bbox dimensions.");
    }

    if(dimensions != this->vbox->get_dims()) {
        throw EastonException("Invalid temporal entry vbox dimensions.");
    }

    // Create the value for the geo entry
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write(docid);
    writer->write(this->wkb);
    io::Bytes::Ptr val = writer->serialize();

    if(Index_InsertTPData(
                idx->get_index(),
                docnum,
                this->bbox->mins(),
                this->bbox->maxs(),
                this->vbox->mins(),
                this->vbox->maxs(),
                this->t_start,
                this->t_end,
                this->bbox->get_dims(),
                val->get(),
                val->size()
            ) != RT_None) {
        throw EastonException("Error inserting temporal entry.");
    }
}


void
TemporalEntry::remove(Index* idx, uint64_t docnum, uint64_t dimensions)
{
    if(!this->bbox) {
        throw EastonException("Invalid temporal entry for removal.");
    }

    if(!this->vbox) {
        throw EastonException("Invalid temporal entry for removal.");
    }

    if(dimensions != this->bbox->get_dims()) {
        throw EastonException("Invalid temporal entry bbox dimensions.");
    }

    if(dimensions != this->vbox->get_dims()) {
        throw EastonException("Invalid temporal entry vbox dimensions.");
    }

    if(Index_DeleteTPData(
                idx->get_index(),
                docnum,
                this->bbox->mins(),
                this->bbox->maxs(),
                this->vbox->mins(),
                this->vbox->maxs(),
                this->t_start,
                this->t_end,
                this->bbox->get_dims()
            ) != RT_None) {
        throw EastonException("Error removing temporal entry.");
    }
}


void
TemporalEntry::search(Index* idx, TopHits& collector, bool nearest)
{
    ::Index* index = (::Index*) idx->get_index();

    if(!this->bbox) {
        throw EastonException("Invalid temporal entry for removal.");
    }

    if(!this->vbox) {
        throw EastonException("Invalid temporal entry for removal.");
    }

    if(this->bbox->get_dims() != this->vbox->get_dims()) {
        throw EastonException("Invalid temporal entry bounding boxes.");
    }

    SpatialIndex::MovingRegion r(
            this->bbox->mins(),
            this->bbox->maxs(),
            this->vbox->mins(),
            this->vbox->maxs(),
            this->t_start,
            this->t_end,
            this->bbox->get_dims()
        );

    EntryVisitor visitor(idx->get_geo_ctx(), idx->get_reader(), collector);

    if(nearest) {
        NNComparator nnc(idx->get_geo_ctx(), idx->get_reader(), collector);
        index->index().nearestNeighborQuery(collector.limit, r, visitor, nnc);
    } else {
        index->index().intersectsWithQuery(r, visitor);
    }
}


geo::Geom::Ptr
TemporalEntry::get_geometry()
{
    if(!this->geom) {
        throw EastonException("Invalid temporal entry has no geometry.");
    }

    return this->geom;
}


geo::GeomFilter
TemporalEntry::make_filter(geo::Ctx::Ptr ctx, uint64_t filter)
{
    if(!this->geom) {
        throw EastonException("Invalid temporal entry has no geometry.");
    }

    return ctx->make_filter(this->geom, filter);
}

Entry::Ptr
HistoricalEntry::read_id(io::Reader::Ptr reader)
{
    if(!reader->read_tuple_n(3)) {
        throw EastonException("Invalid historical entry id tuple.");
    }

    geo::Bounds::Ptr bbox = geo::Bounds::read(reader);

    if(!bbox) {
        throw EastonException("Invalid bbox for historical entry id.");
    }

    double t_start;
    double t_end;

    if(!reader->read(t_start)) {
        throw EastonException("Invalid historical entry id start time.");
    }

    if(!reader->read(t_end)) {
        throw EastonException("Invalid historical entry id end time.");
    }

    return Entry::Ptr(new HistoricalEntry(bbox, t_start, t_end));
}


Entry::Ptr
HistoricalEntry::read_geo(io::Reader::Ptr reader, geo::Ctx::Ptr ctx,
    io::Bytes::Ptr& docid)
{
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Error reading historical entry geo tuple.");
    }

    docid = reader->read_bytes();
    if(!docid) {
        throw EastonException("Error reading doc id from geo historical entry.");
    }

    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Error reading wkb from geo historical entry.");
    }

    geo::Geom::Ptr geom = ctx->from_wkb(wkb);
    if(!geom) {
        throw EastonException("Error creating geometry from historical entry.");
    }

    return Entry::Ptr(new HistoricalEntry(wkb, geom));
}


Entry::Ptr
HistoricalEntry::read_update(io::Reader::Ptr reader, geo::Ctx::Ptr ctx)
{
    int32_t size;
    if(!reader->read_tuple(size)) {
        throw EastonException("Invalid historical entry update tuple.");
    }

    if(size != 3) {
        throw EastonException("Invalid historical entry update tuple size.");
    }

    io::Bytes::Ptr wkb = reader->read_bytes();
    if(!wkb) {
        throw EastonException("Error reading WKB.");
    }

    geo::Geom::Ptr geom = ctx->from_wkb(wkb);
    if(!geom) {
        throw EastonException("Error getting geometry from WKB.");
    }

    double t_start;
    double t_end;

    if(!reader->read(t_start)) {
        throw EastonException("Error reading historical start time.");
    }

    if(!reader->read(t_end)) {
        throw EastonException("Erorr reading historical end time.");
    }

    return Entry::Ptr(new HistoricalEntry(wkb, geom, t_start, t_end));
}


Entry::Ptr
HistoricalEntry::read_query(io::Reader::Ptr reader, geo::Ctx::Ptr ctx,
        geo::SRID::Ptr srid)
{
    int32_t size;
    if(!reader->read_tuple(size)) {
        throw EastonException("Invalid historical index query tuple.");
    }

    if(size != 3) {
        throw EastonException("Invalid historical index tuple size.");
    }

    geo::Geom::Ptr geom = ctx->geom_from_reader(reader, srid);
    if(!geom) {
        throw EastonException("Error reading query geometry.");
    }

    double t_start;
    double t_end;

    if(!reader->read(t_start)) {
        throw EastonException("Invalid historical query start time.");
    }

    if(!reader->read(t_end)) {
        throw EastonException("Invalid historical query end time.");
    }

    return Entry::Ptr(
            new HistoricalEntry(io::Bytes::Ptr(), geom, t_start, t_end)
        );
}


HistoricalEntry::HistoricalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom)
{
    this->wkb = wkb;
    this->geom = geom;

    this->bbox = this->geom->get_bounds();
    if(!this->bbox) {
        throw EastonException("Error getting bounds from geometry.");
    }
}


HistoricalEntry::HistoricalEntry(io::Bytes::Ptr wkb, geo::Geom::Ptr geom,
        double t_start, double t_end)
{
    this->wkb = wkb;
    this->geom = geom;
    this->bbox = this->geom->get_bounds();
    this->t_start = t_start;
    this->t_end = t_end;

    if(!this->bbox) {
        throw EastonException("Error getting bounds from geometry.");
    }
}


HistoricalEntry::HistoricalEntry(geo::Bounds::Ptr bbox, double t_start,
        double t_end)
{
    this->bbox = bbox;
    this->t_start = t_start;
    this->t_end = t_end;
}


HistoricalEntry::~HistoricalEntry()
{
}


void
HistoricalEntry::write_id(io::Writer::Ptr writer)
{
    if(!this->bbox) {
        throw EastonException("Invalid historical entry bbox for id.");
    }

    writer->start_tuple(3);
    this->bbox->write(writer);
    writer->write(this->t_start);
    writer->write(this->t_end);
}


void
HistoricalEntry::update(Index* idx, io::Bytes::Ptr docid,
        uint64_t docnum, uint64_t dimensions)
{
    if(!this->wkb) {
        throw EastonException("Invalid historical entry wkb for index update.");
    }

    if(!this->bbox) {
        throw EastonException("Invalid historical entry bbox for index update.");
    }

    if(dimensions != this->bbox->get_dims()) {
        throw EastonException("Invalid historical entry bbox dimensions.");
    }

    // Create the value for the geo entry
    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write(docid);
    writer->write(this->wkb);
    io::Bytes::Ptr val = writer->serialize();

    if(Index_InsertMVRData(
                idx->get_index(),
                docnum,
                this->bbox->mins(),
                this->bbox->maxs(),
                this->t_start,
                this->t_end,
                this->bbox->get_dims(),
                val->get(),
                val->size()
            ) != RT_None) {
        throw EastonException("Error inserting historical entry.");
    }
}


void
HistoricalEntry::remove(Index* idx, uint64_t docnum, uint64_t dimensions)
{
    if(!this->bbox) {
        throw EastonException("Invalid historical entry for removal.");
    }

    if(dimensions != this->bbox->get_dims()) {
        throw EastonException("Invalid historical entry bbox dimensions.");
    }

    if(Index_DeleteMVRData(
                idx->get_index(),
                docnum,
                this->bbox->mins(),
                this->bbox->maxs(),
                this->t_start,
                this->t_end,
                this->bbox->get_dims()
            ) != RT_None) {
        throw EastonException("Error removing temporal entry.");
    }
}


void
HistoricalEntry::search(Index* idx, TopHits& collector, bool nearest)
{
    ::Index* index = (::Index*) idx->get_index();

    if(!this->bbox) {
        throw EastonException("Invalid historical entry for removal.");
    }

    SpatialIndex::TimeRegion r(
            this->bbox->mins(),
            this->bbox->maxs(),
            this->t_start,
            this->t_end,
            this->bbox->get_dims()
        );

    EntryVisitor visitor(idx->get_geo_ctx(), idx->get_reader(), collector);

    if(nearest) {
        NNComparator nnc(idx->get_geo_ctx(), idx->get_reader(), collector);
        index->index().nearestNeighborQuery(collector.limit, r, visitor, nnc);
    } else {
        index->index().intersectsWithQuery(r, visitor);
    }
}


geo::Geom::Ptr
HistoricalEntry::get_geometry()
{
    if(!this->geom) {
        throw EastonException("Invalid historical entry has no geometry.");
    }

    return this->geom;
}


geo::GeomFilter
HistoricalEntry::make_filter(geo::Ctx::Ptr ctx, uint64_t filter)
{
    if(!this->geom) {
        throw EastonException("Invalid historical entry has no geometry.");
    }

    return ctx->make_filter(this->geom, filter);
}

EntryReader::Ptr
EntryReader::create(geo::Ctx::Ptr ctx, int64_t index_type)
{
   return Ptr(new EntryReader(ctx, index_type));
}


EntryReader::EntryReader(geo::Ctx::Ptr ctx, int64_t index_type)
{
    this->ctx = ctx;
    this->index_type = index_type;
}


Entry::Ptr
EntryReader::read_id(io::Reader::Ptr reader)
{
    switch(this->index_type) {
        case EASTON_INDEX_TYPE_RTREE:
            return SpatialEntry::read_id(reader);
        case EASTON_INDEX_TYPE_MVRTREE:
            return HistoricalEntry::read_id(reader);
        case EASTON_INDEX_TYPE_TPRTREE:
            return TemporalEntry::read_id(reader);
        default:
            throw EastonException("Invalid index type.");
    }
}


Entry::Ptr
EntryReader::read_geo(io::Reader::Ptr reader, io::Bytes::Ptr& docid)
{
    switch(this->index_type) {
        case EASTON_INDEX_TYPE_RTREE:
            return SpatialEntry::read_geo(reader, this->ctx, docid);
        case EASTON_INDEX_TYPE_MVRTREE:
            return HistoricalEntry::read_geo(reader, this->ctx, docid);
        case EASTON_INDEX_TYPE_TPRTREE:
            return TemporalEntry::read_geo(reader, this->ctx, docid);
        default:
            throw EastonException("Invalid index type.");
    }
}


Entry::Ptr
EntryReader::read_update(io::Reader::Ptr reader)
{
    switch(this->index_type) {
        case EASTON_INDEX_TYPE_RTREE:
            return SpatialEntry::read_update(reader, this->ctx);
        case EASTON_INDEX_TYPE_MVRTREE:
            return HistoricalEntry::read_update(reader, this->ctx);
        case EASTON_INDEX_TYPE_TPRTREE:
            return TemporalEntry::read_update(reader, this->ctx);
        default:
            throw EastonException("Invalid index type.");
    }
}


Entry::Ptr
EntryReader::read_query(io::Reader::Ptr reader, geo::SRID::Ptr srid)
{
    switch(this->index_type) {
        case EASTON_INDEX_TYPE_RTREE:
            return SpatialEntry::read_query(reader, this->ctx, srid);
        case EASTON_INDEX_TYPE_MVRTREE:
            return HistoricalEntry::read_query(reader, this->ctx, srid);
        case EASTON_INDEX_TYPE_TPRTREE:
            return TemporalEntry::read_query(reader, this->ctx, srid);
        default:
            throw EastonException("Invalid index type.");
    }
}


NNComparator::NNComparator(geo::Ctx::Ptr ctx, EntryReader::Ptr reader,
        TopHits& hits) : hits(hits)
{
    this->ctx = ctx;
    this->reader = reader;
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

    std::unique_ptr<uint8_t[]> _auto_release(data);

    io::Bytes::Ptr buf = io::Bytes::proxy(data, size);
    io::Reader::Ptr reader = io::Reader::create(buf);
    io::Bytes::Ptr docid;
    Entry::Ptr entry = this->reader->read_geo(reader, docid);
    geo::Geom::Ptr geom = entry->get_geometry();

    return this->hits.distance(docid, geom);
}


EntryVisitor::EntryVisitor(geo::Ctx::Ptr ctx, EntryReader::Ptr reader,
        TopHits& hits) : hits(hits)
{
    this->ctx = ctx;
    this->reader = reader;
}


EntryVisitor::~EntryVisitor()
{
}


void
EntryVisitor::visitNode(const SpatialIndex::INode& in)
{
    this->hits.visit_node();
}


void
EntryVisitor::visitData(const SpatialIndex::IData& d)
{
    uint8_t* data;
    uint32_t size;

    this->hits.visit_data();

    d.getData(size, &data);

    std::unique_ptr<uint8_t[]> _auto_release(data);

    io::Bytes::Ptr buf = io::Bytes::proxy(data, size);
    io::Reader::Ptr reader = io::Reader::create(buf);
    io::Bytes::Ptr docid;
    Entry::Ptr entry = this->reader->read_geo(reader, docid);
    geo::Geom::Ptr geom = entry->get_geometry();

    this->hits.push(docid, geom);
}


void
EntryVisitor::visitData(std::vector<const SpatialIndex::IData*>& v)
{
}


RTreePager::RTreePager(EntryVisitor* visitor, sidx::IShape* query)
{
    this->visitor = visitor;
    this->query = query;

    geo::Geom::Ptr g = this->visitor->hits.filt.geom();
    geo::Bounds::Ptr b = g->get_centroid()->get_bounds();

    this->query_center = new SpatialIndex::Point(b->mins(), b->get_dims());
}


RTreePager::~RTreePager()
{
    delete this->query_center;
}


void
RTreePager::getNextEntry(const SpatialIndex::IEntry& entry,
        int64_t& next_id, bool& hasNext)
{
    const sidx::INode* node = dynamic_cast<const sidx::INode*>(&entry);
    SidxShapePtr mbr = sidx_get_shape(&entry);

    if(!this->query->intersectsShape(*(mbr.get()))) {
        goto done;
    }

    // The magic dust in our new tree traversal is this test
    // that can determine whether we need to inspect a given
    // node based on the minimum distance between the center
    // of the query and the MBR of the node. If that minimum
    // distance is larger than the last entry on the current
    // page we know that we don't have to bother even looking
    // at the node.
    //
    // The astute reader will realize that we're filtering
    // both before and after we add the node id to the stack.
    // This is because the data for our current page changes
    // inbetween so we need to check both times. The reason
    // we bother checking before we place it on the stack is
    // that it saves us even reading the node from disk as
    // the MBR is already present in the parent node as well.

    if(!this->should_visit(mbr.get())) {
        goto done;
    }

    this->visitor->hits.visit_node();

    if(node->isLeaf()) {
        for(uint32_t i = 0; i < node->getChildrenCount(); i++) {

            SidxShapePtr c_mbr = sidx_get_child_shape(node, i);

            if(!this->query->intersectsShape(*(c_mbr.get()))) {
                continue;
            }

            if(!this->should_visit(c_mbr.get())) {
                continue;
            }

            this->visitor->hits.visit_data();

            uint8_t* data;
            uint32_t size;

            node->getChildData(i, size, &data);

            io::Bytes::Ptr buf = io::Bytes::proxy(data, size);
            io::Reader::Ptr reader = io::Reader::create(buf);
            io::Bytes::Ptr docid;
            Entry::Ptr entry = this->visitor->reader->read_geo(reader, docid);
            geo::Geom::Ptr geom = entry->get_geometry();

            this->visitor->hits.push(docid, geom);
        }
    } else {
        // The sort here is so that we fill the page as quickly as
        // possible. We do this so that our checks for skipping
        // nodes starts as early as possible in our traversals.

        std::vector<std::pair<double, int64_t>> children;

        for(uint32_t i = 0; i < node->getChildrenCount(); i++) {

            SidxShapePtr c_mbr = sidx_get_child_shape(node, i);

            if(!this->query->intersectsShape(*(c_mbr.get()))) {
                continue;
            }

            if(!this->should_visit(c_mbr.get())) {
                continue;
            }

            double d = this->query_center->getMinimumDistance(*(c_mbr.get()));
            children.push_back(std::make_pair(d, node->getChildIdentifier(i)));
        }

        std::sort(children.begin(), children.end());

        // This iteration is a bit odd but just allows us to
        // place items on the stack in order of increasing
        // distance rather than figure out the template magic
        // to get the std::sort in the other direction.
        while(children.size()) {
            this->ids.push(children.back().second);
            children.pop_back();
        }
    }

done:
    if(this->ids.empty()) {
        hasNext = false;
    } else {
        hasNext = true;
        next_id = ids.top();
        ids.pop();
    }
}


bool
RTreePager::should_visit(const sidx::IShape* mbr)
{
    if(this->visitor->hits.size() < this->visitor->hits.limit) {
        return true;
    }

    double min_dist = this->query_center->getMinimumDistance(*mbr);
    double max_dist = this->visitor->hits.hits.top().distance;

    return min_dist < max_dist;
}

NNRTreePager::NNRTreePager(EntryVisitor* visitor, sidx::IShape* query, NNComparator* nnc)
{
    this->visitor = visitor;
    this->query = query;
    this->nnc = nnc;

    geo::Geom::Ptr g = this->visitor->hits.filt.geom();
    geo::Bounds::Ptr b = g->get_centroid()->get_bounds();

}


NNRTreePager::~NNRTreePager()
{
}


void
NNRTreePager::getNextEntry(const SpatialIndex::IEntry& entry,
        int64_t& next_id, bool& hasNext)
{
    const sidx::INode* node = dynamic_cast<const sidx::INode*>(&entry);
    SidxShapePtr mbr = sidx_get_shape(&entry);

    // using relation filter to exclude entries which doesn't satisfy condition
    SpatialIndex::Region mbr_region;
    sidx::IShape* s;
    entry.getShape(&s);
    s->getMBR(mbr_region);
    geo::Ctx::Ptr geo_ctx = this->visitor->ctx;
    geo::Geom::Ptr geom = geo_ctx->make_rectangle((mbr_region.m_pLow),
                                                  (mbr_region.m_pHigh),
                                                  (mbr_region.m_dimension),
                                                  geo_ctx->get_srid());
    if (!this->visitor->hits.filt(geom)) {
       goto done;
    }

    // The magic dust in our new tree traversal is this test
    // that can determine whether we need to inspect a given
    // node based on the minimum distance between the center
    // of the query and the MBR of the node. If that minimum
    // distance is larger than the last entry on the current
    // page we know that we don't have to bother even looking
    // at the node.
    //
    // The astute reader will realize that we're filtering
    // both before and after we add the node id to the stack.
    // This is because the data for our current page changes
    // inbetween so we need to check both times. The reason
    // we bother checking before we place it on the stack is
    // that it saves us even reading the node from disk as
    // the MBR is already present in the parent node as well.

    if(!this->should_visit(mbr.get())) {
        goto done;
    }

    this->visitor->hits.visit_node();

    if(node->isLeaf()) {
        for(uint32_t i = 0; i < node->getChildrenCount(); i++) {

            SidxShapePtr c_mbr = sidx_get_child_shape(node, i);

            // using relation filter to exclude leaf which doesn't satisfy condition
            c_mbr->getMBR(mbr_region);
            geo::Ctx::Ptr geo_ctx = this->visitor->ctx;
            geo::Geom::Ptr c_geom = geo_ctx->make_rectangle((mbr_region.m_pLow),
                                                            (mbr_region.m_pHigh),
                                                            (mbr_region.m_dimension),
                                                            geo_ctx->get_srid());

            if (!this->visitor->hits.filt(c_geom)) {
                continue;
            }

            if(!this->should_visit(c_mbr.get())) {
                continue;
            }

            this->visitor->hits.visit_data();

            uint8_t* data;
            uint32_t size;

            node->getChildData(i, size, &data);

            io::Bytes::Ptr buf = io::Bytes::proxy(data, size);
            io::Reader::Ptr reader = io::Reader::create(buf);
            io::Bytes::Ptr docid;
            Entry::Ptr entry = this->visitor->reader->read_geo(reader, docid);
            geo::Geom::Ptr geom = entry->get_geometry();

            this->visitor->hits.push(docid, geom);
        }
    } else {
        // The sort here is so that we fill the page as quickly as
        // possible. We do this so that our checks for skipping
        // nodes starts as early as possible in our traversals.

        std::vector<std::pair<double, int64_t>> children;

        for(uint32_t i = 0; i < node->getChildrenCount(); i++) {

            SidxShapePtr c_mbr = sidx_get_child_shape(node, i);

            if(!this->should_visit(c_mbr.get())) {
                continue;
            }

            // using relation filter to exclude node which doesn't satisfy condition
            c_mbr->getMBR(mbr_region);
            geo::Ctx::Ptr geo_ctx = this->visitor->ctx;
            geo::Geom::Ptr c_geom = geo_ctx->make_rectangle((mbr_region.m_pLow),
                                                            (mbr_region.m_pHigh),
                                                            (mbr_region.m_dimension),
                                                            geo_ctx->get_srid());

            if (!this->visitor->hits.filt(c_geom)) {
                continue;
            }

            double d = this->nnc->getMinimumDistance(*query,*(c_mbr.get()));
            children.push_back(std::make_pair(d, node->getChildIdentifier(i)));
        }

        std::sort(children.begin(), children.end());

        // This iteration is a bit odd but just allows us to
        // place items on the stack in order of increasing
        // distance rather than figure out the template magic
        // to get the std::sort in the other direction.
        while(children.size()) {
            this->ids.push(children.back().second);
            children.pop_back();
        }
    }

done:
    if(this->ids.empty()) {
        hasNext = false;
    } else {
        hasNext = true;
        next_id = ids.top();
        ids.pop();
    }
}


bool
NNRTreePager::should_visit(const sidx::IShape* mbr)
{
    if(this->visitor->hits.size() < this->visitor->hits.limit) {
        return true;
    } else {
        // to be done
        return true;
    }
}


Index::Ptr
Index::create(io::Reader::Ptr reader)
{
    int32_t arity;
    if(!reader->read_list(arity)) {
        throw EastonException("Invalid index options.");
    }

    std::string dir;
    int64_t type = -1;
    int64_t dims = -1;
    geo::SRID::Ptr srid;

    for(int32_t i = 0; i < arity; i++) {
        if(!reader->read_tuple_n(2)) {
            throw EastonException("Invalid index option tuple.");
        }

        std::string optname;
        if(!reader->read(optname)) {
            throw EastonException("Invalid index option name.");
        }

        if(optname == "index_directory") {
            if(!reader->read(dir)) {
                throw EastonException("Invalid index directory value.");
            }
        } else if(optname == "index_type") {
            if(!reader->read(type)) {
                throw EastonException("Invalid index type value.");
            }
        } else if(optname == "dimensions") {
            if(!reader->read(dims)) {
                throw EastonException("Invalid index dimensions value.");
            }
        } else if(optname == "srid") {
            srid = geo::SRID::from_reader(reader);
            if(!srid) {
                throw EastonException("Invalid index SRID value.");
            }
        } else {
            throw EastonException("Unknown index option: " + optname);
        }
    }

    if(!reader->read_empty_list()) {
        throw EastonException("Improper index option list.");
    }

    if(!dir.size()) {
        throw EastonException("No index directory specified.");
    }

    if(type < 0) {
        throw EastonException("No index type specified.");
    }

    if(dims < 0) {
        throw EastonException("No index dimensions specified.");
    }

    if(!srid) {
        throw EastonException("No index SRID specified.");
    }


    return Ptr(new Index(dir, type, dims, srid));
}


Index::Index(std::string dir, int64_t type, int64_t dims, geo::SRID::Ptr srid)
{
    this->db_dir = dir;
    if(!io::is_dir(this->db_dir)) {
        throw EastonException("Index directory does not exist.");
    }

    this->idx_type = type;
    this->idx_dims = dims;

    this->init_storage();
    this->init_geo_idx();
    this->init_srid(srid);
    this->init_counts();

    this->geo_ctx = geo::Ctx::create(this->dimensions, this->srid);
    this->reader = EntryReader::create(this->geo_ctx, type);
}


Index::~Index()
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    Index_Destroy(this->geo_idx);
    this->store.reset();
}


void
Index::sync()
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    Index_Flush(this->geo_idx);
}


IndexH
Index::get_index()
{
    return this->geo_idx;
}


void
Index::reset_index(bool validate)
{
    io::Transaction::Ptr tx = io::Transaction::open(this->store);
    Index_Destroy(this->geo_idx);
    tx->commit();
    tx.reset();

    this->init_geo_idx(validate);
}


geo::Ctx::Ptr
Index::get_geo_ctx()
{
    return this->geo_ctx;
}


EntryReader::Ptr
Index::get_reader()
{
    return this->reader;
}


uint64_t
Index::curr_docid_num()
{
    return this->docid_num;
}


uint64_t
Index::get_doc_count()
{
    return this->doc_count;
}


uint64_t
Index::get_geom_count()
{
    return this->geom_count;
}


uint64_t
Index::data_size()
{
    return this->store->data_size();
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
Index::update(io::Bytes::Ptr docid, Entry::Vector entries)
{
    io::Transaction::Ptr tx = io::Transaction::open(this->store);
    io::Bytes::Ptr dockey = this->store->make_key("docid", docid);

    // Remove any results currently in the index for
    // this docid.
    this->remove_int(docid);

    // Write our ID info to the index.

    io::Writer::Ptr writer = io::Writer::create();
    uint64_t docnum = this->get_docid_num(dockey);
    uint64_t num_entries = entries.size();

    writer->start_tuple(2);
    writer->write(docnum);
    writer->start_list(num_entries);
    for(uint64_t i = 0; i < num_entries; i++) {
        entries.at(i)->write_id(writer);
    }
    writer->write_empty_list();

    io::Bytes::Ptr val = writer->serialize();
    this->store->put_kv(dockey, val);

    for(uint32_t i = 0; i < entries.size(); i++) {
        entries.at(i)->update(this, docid, docnum, this->dimensions);
    }

    this->doc_count += 1;
    this->geom_count += entries.size();

    this->store_counts();

    Index_Flush(this->geo_idx);

    tx->commit();
}


void
Index::remove(io::Bytes::Ptr docid)
{
    io::Transaction::Ptr tx = io::Transaction::open(this->store);
    this->remove_int(docid);
    this->store_counts();
    Index_Flush(this->geo_idx);
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

    io::Reader::Ptr reader = io::Reader::create(val);
    if(!reader->read_tuple_n(2)) {
        throw EastonException("Error reading id entry.");
    }

    uint64_t docnum;
    if(!reader->read(docnum)) {
        throw EastonException("Error reading id doc number.");
    }

    int32_t num_entries;
    if(!reader->read_list(num_entries)) {
        throw EastonException("Error reading id entry list.");
    }

    for(int32_t i = 0; i < num_entries; i++) {
        Entry::Ptr e = this->reader->read_id(reader);
        e->remove(this, docnum, this->dimensions);
    }

    this->store->del_kv(dockey);
    this->doc_count -= 1;
    this->geom_count -= num_entries;
}


void
Index::search(TopHits& collector, Entry::Ptr entry, bool nearest)
{
    try {
        entry->search(this, collector, nearest);
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
Index::init_geo_idx(bool validate)
{
    this->geo_idx = NULL;

    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    IndexPropertyH props = IndexProperty_Create();

    RTIndexType it;

    this->dimensions = this->idx_dims;

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

    if(this->idx_type == EASTON_INDEX_TYPE_RTREE) {
        it = RT_RTree;
    } else if(this->idx_type == EASTON_INDEX_TYPE_TPRTREE) {
        it = RT_TPRTree;
    } else if(this->idx_type == EASTON_INDEX_TYPE_MVRTREE) {
        it = RT_MVRTree;
    } else {
        throw EastonException("Invalid geo index type.");
    }

    if(IndexProperty_SetIndexType(props, it) != RT_None) {
        throw EastonException("Error setting geo index type.");
    }

    if(it == RT_RTree) {
        if(IndexProperty_SetIndexVariant(props, RT_Star) != RT_None) {
            throw EastonException("Error setting geo index variant");
        }
    }

    if(IndexProperty_SetDimension(props, (uint32_t)this->idx_dims) != RT_None) {
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

    if(validate && !Index_IsValid(this->geo_idx)) {
        throw IndexException("Invalid Geo Index: " + sidx_error());
    }

    IndexProperty_Destroy(props);

    // Verify the properties after open are the
    // same as requested.

    props = sidx_properties(this->geo_idx);

    if(IndexProperty_GetDimension(props) != this->idx_dims) {
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
Index::init_srid(geo::SRID::Ptr srid)
{
    io::Transaction::Ptr tx = io::Transaction::autocommit(this->store);
    this->srid = srid;

    // Verify that we have the same SRID if we're
    // reopening the index.
    io::Bytes::Ptr key = this->store->make_key("meta", "srid");
    io::Bytes::Ptr val = this->store->get_kv(key);

    if(!val) {
        // This is the first time we've opened the index.
        // Store the SRID and call it a day.
        io::Writer::Ptr writer = io::Writer::create();
        writer->write(this->srid->c_str());
        this->store->put_kv(key, writer->serialize());
        return;
    }

    io::Reader::Ptr reader = io::Reader::create(val);
    std::string old_srid;
    if(!reader->read(old_srid)) {
        throw EastonException("Error reading stored SRID.");
    }

    if(this->srid->str() != old_srid) {
        throw EastonException("Invalid SRID.");
    }
}


void
Index::init_counts()
{
    io::Bytes::Ptr key = this->store->make_key("meta", "counts");
    io::Bytes::Ptr val = this->store->get_kv(key);

    if(!val) {
        // This is the first time we've opened the index.
        this->doc_count = 0;
        this->geom_count = 0;
        return;
    }

    io::Reader::Ptr reader = io::Reader::create(val);

    if(!reader->read_tuple_n(2)) {
        throw EastonException("Invalid index counts value.");
    }

    if(!reader->read(this->doc_count)) {
        throw EastonException("Error reading stored document count.");
    }

    if(!reader->read(this->geom_count)) {
        throw EastonException("Error reading stored geometry count.");
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
        if(!reader->read_tuple_n(2)) {
            throw EastonException("Unable to read id entry tuple.");
        }
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


void
Index::store_counts()
{
    io::Bytes::Ptr key = this->store->make_key("meta", "counts");

    io::Writer::Ptr writer = io::Writer::create();
    writer->start_tuple(2);
    writer->write(this->doc_count);
    writer->write(this->geom_count);
    this->store->put_kv(key, writer->serialize());
}


NS_EASTON_END
