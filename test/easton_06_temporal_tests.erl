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

-module(easton_06_temporal_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/06".


open_idx() ->
    case filelib:is_dir(idx_dir()) of
        true ->
            ok = easton_index:destroy(idx_dir());
        false ->
            ok
    end,
    {ok, Idx} = easton_index:open(idx_dir(), [{type, tprtree}]),
    Idx.


close_idx(Idx) ->
    easton_index:close(Idx).


basic_test_() ->
    {"Basic Temporal Tests",
        {setup,
            fun open_idx/0,
            fun close_idx/1,
            fun(Idx) -> {with, Idx, [
                fun moving_shape_test/1
            ]} end
        }
    }.


moving_shape_test(Idx) ->
    Names = [
        <<"point">>,
        <<"linestring">>,
        <<"polygon">>,
        <<"polgyon_with_hole">>,
        <<"multipoint">>,
        <<"multilinestring">>,
        <<"multipolygon">>,
        <<"geometrycollection">>
    ],

    Shapes = [
        easton_shapes:point(),
        easton_shapes:linestring(),
        easton_shapes:polygon(),
        easton_shapes:polygon_with_hole(),
        easton_shapes:multipoint(),
        easton_shapes:multilinestring(),
        easton_shapes:multipolygon(),
        easton_shapes:geometrycollection()
    ],

    MovingShapes = [
        easton_shapes:moving(S, [1, 1], [1, 1], 0, 10) || S <- Shapes
    ],

    Triples = lists:zip3(Names, Shapes, MovingShapes),

    lists:foreach(fun({Name, Shape, MovingShape}) ->
        ?assertEqual(ok, easton_index:update(Idx, Name, MovingShape)),
        SearchOpts = [include_geom, {t_start, 0}, {t_end, 10}],
        ?assertMatch({ok, [_|_]}, easton_index:search(Idx, Shape, SearchOpts))
    end, Triples),

    NumShapes = length(Triples),
    ?assertEqual({ok, NumShapes}, easton_index:doc_count(Idx)),

    lists:foreach(fun({Name, _Shape, _MovingShape}) ->
        ?assertEqual(ok, easton_index:remove(Idx, Name))
    end, Triples),

    ?assertEqual({ok, 0}, easton_index:doc_count(Idx)).


% Apparently libspatialindex doesn't yet
% support nearest neighbor queries.
%
% moving_points_nearest_test(Idx) ->
%     lists:foreach(fun(X) ->
%         Id = iolist_to_binary(io_lib:format("~3..0b", [X])),
%         P = easton_shapes:point(X, X),
%         MP = easton_shapes:moving(P, [1, 1], [1, 1], 10, 20),
%         ?assertEqual(ok, easton_index:update(Idx, Id, MP))
%     end, lists:seq(1, 10)),
%
%     ExpectedOrder = [
%         <<"005">>,
%         <<"004">>,
%         <<"006">>,
%         <<"003">>,
%         <<"007">>,
%         <<"002">>,
%         <<"008">>,
%         <<"001">>,
%         <<"009">>,
%         <<"010">>
%     ],
%
%     QP = easton_shapes:point(5, 5),
%     QOpts = [nearest, {t_start, 10}, {t_end, 20}],
%     {ok, Hits} = easton_index:search(Idx, QP, QOpts),
%     HitIds = [Id || {Id, _} <- Hits],
%
%     lists:foreach(fun({Expected, Result}) ->
%         ?assertEqual(Expected, Result)
%     end, lists:zip(ExpectedOrder, HitIds)).
%
