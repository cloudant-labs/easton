SRS/CRS - Spatial/Coordinate Reference Systems
==============================================

Need to implement these. Both setting one on index creation and then passing them in during queries.

Custom Vistor for Query Results
===============================

Currently libspatialindex loads the entire result set into memory before paging is applied. I'll need to write a thing that only keeps matches in RAM. I'll also need to figure out how to do the bookmarks/paging API.
