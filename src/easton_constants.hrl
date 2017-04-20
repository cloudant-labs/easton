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

-define(EASTON_STREAM_IN, 3).
-define(EASTON_STREAM_OUT, 4).


-define(EASTON_DEFAULT_CS_MAP_DIR, "/usr/share/CsMap/dict").


-define(EASTON_OK, 0).

-define(EASTON_ERROR_BAD_WRITE, 1).
-define(EASTON_ERROR_BAD_READ, 2).
-define(EASTON_ERROR_BAD_ALLOC, 3).
-define(EASTON_ERROR_BAD_ARGS, 4).
-define(EASTON_ERROR_BAD_DIRECTORY, 5).
-define(EASTON_ERROR_BAD_ID_IDX_INIT, 6).
-define(EASTON_ERROR_BAD_GEO_IDX_INIT, 7).
-define(EASTON_ERROR_CORRUPT_GEO_IDX, 8).
-define(EASTON_ERROR_BAD_GEO_IDX_CFG, 9).
-define(EASTON_ERROR_BAD_GEOS_INIT, 10).
-define(EASTON_ERROR_GEOS_EXCEPTION, 11).
-define(EASTON_ERROR_BAD_COMMAND, 12).
-define(EASTON_ERROR_TRAILING_DATA, 13).
-define(EASTON_ERROR_CLOSE_FAIL, 14).
-define(EASTON_ERROR_FLUSH_FAIL, 15).
-define(EASTON_ERROR_BAD_USER_KEY, 16).
-define(EASTON_ERROR_BAD_USER_VAL, 17).
-define(EASTON_ERROR_BAD_PUT_USER_KV, 18).
-define(EASTON_ERROR_BAD_GET_USER_KV, 19).
-define(EASTON_ERROR_BAD_DEL_USER_KV, 20).
-define(EASTON_ERROR_BAD_DOC_ID, 21).
-define(EASTON_ERROR_BAD_NUM_WKBS, 22).
-define(EASTON_ERROR_BAD_WKB, 23).
-define(EASTON_ERROR_BAD_DOC_ID_VAL, 24).
-define(EASTON_ERROR_BAD_DOC_NUM_INC, 25).
-define(EASTON_ERROR_BAD_FILTER, 26).
-define(EASTON_ERROR_BAD_NEAREST, 27).
-define(EASTON_ERROR_BAD_LIMIT, 28).
-define(EASTON_ERROR_BAD_OFFSET, 29).
-define(EASTON_ERROR_BAD_QUERY, 30).


-define(EASTON_INDEX_TYPE_RTREE, 1).
-define(EASTON_INDEX_TYPE_TPRTREE, 2).
-define(EASTON_INDEX_TYPE_MVRTREE, 3).


-define(EASTON_COMMAND_CLOSE, 1).
-define(EASTON_COMMAND_SYNC, 2).
-define(EASTON_COMMAND_GET_INDEX_INFO, 3).
-define(EASTON_COMMAND_PUT_USER_KV, 4).
-define(EASTON_COMMAND_GET_USER_KV, 5).
-define(EASTON_COMMAND_DEL_USER_KV, 6).
-define(EASTON_COMMAND_UPDATE_ENTRIES, 7).
-define(EASTON_COMMAND_UPDATE_TP_ENTRIES, 8).
-define(EASTON_COMMAND_REMOVE_ENTRIES, 9).
-define(EASTON_COMMAND_REMOVE_TP_ENTRIES, 10).
-define(EASTON_COMMAND_SEARCH, 11).
-define(EASTON_COMMAND_SEARCH_TP, 12).
-define(EASTON_COMMAND_INDEX_SIZE, 13).



-define(EASTON_FILTER_NONE, 0).
-define(EASTON_FILTER_CONTAINS, 1).
-define(EASTON_FILTER_CONTAINS_PROPERLY, 2).
-define(EASTON_FILTER_COVERED_BY, 3).
-define(EASTON_FILTER_COVERS, 4).
-define(EASTON_FILTER_CROSSES, 5).
-define(EASTON_FILTER_DISJOINT, 6).
-define(EASTON_FILTER_INTERSECTS, 7).
-define(EASTON_FILTER_OVERLAPS, 8).
-define(EASTON_FILTER_TOUCHES, 9).
-define(EASTON_FILTER_WITHIN, 10).
-define(EASTON_FILTER_MAX, 10).
