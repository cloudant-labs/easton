SRS/CRS - Spatial/Coordinate Reference Systems
==============================================

Need to implement these. Both setting one on index creation and then passing them in during queries.

Custom Storage Managers
=======================

I need to write the CustomStorage manager code so that libspatialindex will store its data in TokyoCabinet so that our index udpates are atomic to avoid index corruption.

Error Messages
==============

I need to improve error handling in the port to return better error messages as well as not just die on any random issue. Might also need to figure out gettext. Though I'll probably just create an API to hide behind and do gettext later.

Replace TokyoCabinet with LevelDB
=================================

Not only is the licensing better, but LevelDB has background compactions which means we shouldn't need to worry about offline compaction which I haven't yet figured out for TokyoCabinet.

Replace Custom Marshaling with ei
=================================

I should've started with ei.