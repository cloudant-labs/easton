SRS/CRS - Spatial/Coordinate Reference Systems
==============================================

Need to implement these. Both setting one on index creation and then passing them in during queries.

Custom Storage Managers
=======================

I need to write the CustomStorage manager code so that libspatialindex will store its data in TokyoCabinet so that our index udpates are atomic to avoid index corruption.

Replace TokyoCabinet with LevelDB
=================================

Not only is the licensing better, but LevelDB has background compactions which means we shouldn't need to worry about offline compaction which I haven't yet figured out for TokyoCabinet.

