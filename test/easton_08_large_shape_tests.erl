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

-module(easton_08_large_shape_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/08".


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


large_shape_test_() ->
    % This test can sometimes just exceed the default eunit
    % test timeout of 5 seconds, so force a higher timeout
    {timeout,
        10,
        [
            fun() ->
                Idx = open_idx(),
                try
                    run(Idx)
                after
                    close_idx(Idx)
                end
            end
        ]
    }.

run(Idx) ->
    Shape = circle(),
    QueryPoint = {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [0.0, 0.0]}
    ]},
    ?assertEqual(ok, easton_index:update(Idx, <<"foo">>, Shape)),
    ?assertEqual({ok, 1}, easton_index:doc_id_num(Idx)),
    ?assertEqual({ok, 1}, easton_index:doc_count(Idx)),
    ?assertEqual(
        {ok, [{<<"foo">>, 0.0, Shape}]},
        easton_index:search(Idx, Shape, [include_geom])
    ),
    ?assertMatch(
        {ok, [{<<"foo">>, _, Shape}]},
        easton_index:search(Idx, QueryPoint, [include_geom])
    ),
    ?assertEqual(ok, easton_index:remove(Idx, <<"foo">>)),
    ?assertEqual({ok, 1}, easton_index:doc_id_num(Idx)),
    ?assertEqual({ok, 0}, easton_index:doc_count(Idx)),
    ?assertEqual({ok, []}, easton_index:search(Idx, Shape)).


circle() ->
    NumPoints = 10000,
    Unit = 2.0 * math:pi() / NumPoints,
    Coords0 = lists:map(fun(I) ->
        [math:sin(I * Unit), math:cos(I * Unit)]
    end, lists:seq(0, NumPoints-1)),
    Coords = Coords0 ++ [hd(Coords0)],
    {[
        {<<"type">>, <<"Polygon">>},
        {<<"coordinates">>, [Coords]}
    ]}.