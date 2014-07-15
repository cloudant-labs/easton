
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
        const GEOSGeometry* g, int src_srid, int tgt_srid);

bool reproject(int32_t src_srid, int32_t tgt_srid, double xyzm[4]);
bool reproject(int32_t src_srid, const char* tgt_name, double xyzm[4]);
bool reproject(const char* src_name, int32_t tgt_srid, double xyzm[4]);
bool reproject(const char* src_name, const char* tgt_name, double xyzm[4]);


NS_EASTON_GEO_END
NS_EASTON_END


#endif