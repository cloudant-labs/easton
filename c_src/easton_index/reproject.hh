
#ifndef EASTON_REPROJECT_HH
#define EASTON_REPROJECT_HH


// Prevent the misuse of non thread-safe GEOS functions
#define GEOS_USE_ONLY_R_API


#include <geos_c.h>

#include "easton.hh"
#include "geo.hh"

using namespace easton;


NS_EASTON_BEGIN
NS_EASTON_GEO_BEGIN


GEOSGeometry* reproject(GEOSCtx c,
        const GEOSGeometry* g, SRID::Ptr src_srid, SRID::Ptr tgt_srid);

bool reproject(SRID::Ptr src, SRID::Ptr tgt, double xyzm[4]);

NS_EASTON_GEO_END
NS_EASTON_END


#endif