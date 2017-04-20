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

-module(easton_03_kv_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/03".


open_idx() ->
    case filelib:is_dir(idx_dir()) of
        true ->
            ok = easton_index:destroy(idx_dir());
        false ->
            ok
    end,
    {ok, Idx} = easton_index:open(idx_dir()),
    Idx.


close_idx(Idx) ->
    easton_index:close(Idx).


basic_test_() ->
    {"Basic KV tests",
        {foreach,
            fun open_idx/0,
            fun close_idx/1,
            [
                fun put_one_/1,
                fun put_put_/1,
                fun get_missing_/1,
                fun del_missing_/1,
                fun put_get_/1,
                fun put_get_put_get_/1,
                fun put_del_get_/1,
                fun put_get_del_/1
            ]
        }
    }.


put_one_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, key, val))
    ].


put_put_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, key, val)),
        ?_assertEqual(ok, easton_index:put(Idx, key, val))
    ].


get_missing_(Idx) ->
    [
        ?_assertEqual(false, easton_index:get(Idx, key)),
        ?_assertEqual(default, easton_index:get(Idx, key, default))
    ].


del_missing_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:del(Idx, key))
    ].


put_get_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, key, val)),
        ?_assertEqual({key, val}, easton_index:get(Idx, key)),
        ?_assertEqual(val, easton_index:get(Idx, key, key))
    ].


put_get_put_get_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, key, val1)),
        ?_assertEqual({key, val1}, easton_index:get(Idx, key)),
        ?_assertEqual(val1, easton_index:get(Idx, key, undefined)),
        ?_assertEqual(ok, easton_index:put(Idx, key, val2)),
        ?_assertEqual({key, val2}, easton_index:get(Idx, key))
    ].


put_del_get_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, key, val)),
        ?_assertEqual(ok, easton_index:del(Idx, key)),
        ?_assertEqual(false, easton_index:get(Idx, val))
    ].


put_get_del_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, key, val)),
        ?_assertEqual({key, val}, easton_index:get(Idx, key)),
        ?_assertEqual(ok, easton_index:del(Idx, key))
    ].
