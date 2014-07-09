% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-define(JSON_TYPE, <<"type">>).
-define(JSON_COORDINATES, <<"coordinates">>).
-define(JSON_GEOMETRIES, <<"geometries">>).

-define(JSON_POINT, <<"Point">>).
-define(JSON_LINESTRING, <<"LineString">>).
-define(JSON_POLYGON, <<"Polygon">>).
-define(JSON_MULTIPOINT, <<"MultiPoint">>).
-define(JSON_MULTILINESTRING, <<"MultiLineString">>).
-define(JSON_MULTIPOLYGON, <<"MultiPolygon">>).
-define(JSON_GEOMETRYCOLLECTION, <<"GeometryCollection">>).

-define(WKB_POINT, 1).
-define(WKB_LINESTRING, 2).
-define(WKB_POLYGON, 3).
-define(WKB_MULTIPOINT, 4).
-define(WKB_MULTILINESTRING, 5).
-define(WKB_MULTIPOLYGON, 6).
-define(WKB_GEOMETRYCOLLECTION, 7).

-define(WKB_Z, 16#80000000).
-define(WKB_M, 16#40000000).
