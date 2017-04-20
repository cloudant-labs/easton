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