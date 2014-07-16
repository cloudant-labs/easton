Add Index Size
==============

Need to be able to report the approximate byte count for the index so bacon can do its thing.

Synchronous Start
=================

I should switch to using a synchronous startup using proc\_lib.


Custom Vistor for Query Results
===============================

Currently libspatialindex loads the entire result set into memory before paging is applied. I'll need to write a thing that only keeps matches in RAM. I'll also need to figure out how to do the bookmarks/paging API.


Index Arguments
===============

Make the command line parsing not suck so bad. Or alternatively pass all the arguments of stdin...


Index Dimensions
================

We need to figure out if we want to fill in default values for indices that have a higher dimension than the inserted or queried geometries.
