Add Index Size
==============

Need to be able to report the approximate byte count for the index so bacon can do its thing.

Synchronous Start
=================

I should switch to using a synchronous startup using proc\_lib.


Index Arguments
===============

Make the command line parsing not suck so bad. Or alternatively pass all the arguments of stdin...


Index Dimensions
================

We need to figure out if we want to fill in default values for indices that have a higher dimension than the inserted or queried geometries.
