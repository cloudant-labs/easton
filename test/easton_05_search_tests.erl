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

-module(easton_05_search_tests).

-include_lib("eunit/include/eunit.hrl").

idx_dir() ->
    "idx/05".

num_inner() ->
    1000.

num_outer() ->
    1000.

num_points() ->
    num_inner() + num_outer().


search_test_() ->
    {"Search Tests",
        {timeout, 10, [
            {setup,
                fun open_idx/0,
                fun close_idx/1,
                fun(Idx) -> {with, Idx, [
                    fun get_all_test/1,
                    fun get_limit_test/1,
                    fun get_nearest_test/1,
                    fun get_bookmark_test/1,
                    fun get_nearest_bookmark_test/1,
                    fun get_wkt_test/1,
                    fun get_bbox_test/1,
                    fun get_bbox_coord_test/1,
                    fun get_circle_test/1,
                    fun get_ellipse_test/1,
                    fun get_empty_shapes_test/1,
                    fun get_empty_wkt_test/1,
                    fun get_with_req_srid_test/1
                ]} end
            }
        ]}
    }.


get_all_test(Idx) ->
    Shape = easton_shapes:rectangle(-3, -3, 3, 3),
    {ok, Results} = easton_index:search(Idx, Shape, [{limit, num_points()}]),
    ?assertEqual(num_inner() + num_outer(), length(Results)),
    SortedResults = lists:sort(fun({Id1, D1}, {Id2, D2}) ->
        {D1, Id1} =< {D2, Id2}
    end, Results),
    ?assertEqual(SortedResults, Results).


get_limit_test(Idx) ->
    Shape = easton_shapes:rectangle(-3, -3, 3, 3),
    {ok, AllResults} = easton_index:search(Idx, Shape, [{limit, num_points()}]),
    lists:foreach(fun(I) ->
        {ok, Results} = easton_index:search(Idx, Shape, [{limit, I}]),
        ?assertEqual(true, lists:prefix(Results, AllResults))
    end, lists:seq(1, num_inner() + num_outer(), 250)).


get_nearest_test(Idx) ->
    Shape = easton_shapes:point(0, 0),
    {ok, Results} = easton_index:search(Idx, Shape, [nearest, {limit, 5}]),
    ?assertEqual(5, length(Results)),
    SortedResults = lists:sort(fun({Id1, D1}, {Id2, D2}) ->
        {D1, Id1} =< {D2, Id2}
    end, Results),
    ?assertEqual(SortedResults, Results).


get_bookmark_test(Idx) ->
    Shape = easton_shapes:rectangle(-3, -3, 3, 3),
    {ok, AllResults} = easton_index:search(Idx, Shape, [{limit, num_points()}]),
    lists:foreach(fun(I) ->
        Bookmark = lists:nth(I, AllResults),
        {ok, Results} = easton_index:search(Idx, Shape, [
                {limit, num_points()},
                {bookmark, Bookmark}
            ]),
        Tail = lists:nthtail(I, AllResults),
        ?assertEqual(Tail, Results)
    end, lists:seq(1, num_inner() + num_outer(), 250)).


get_nearest_bookmark_test(Idx) ->
    Shape = easton_shapes:rectangle(-3, -3, 3, 3),
    {ok, AllResults} = easton_index:search(Idx, Shape, [
            nearest,
            {limit, num_points()}
        ]),
    lists:foreach(fun(I) ->
        Bookmark = lists:nth(I, AllResults),
        {ok, Results} = easton_index:search(Idx, Shape, [
                nearest,
                {limit, num_points()},
                {bookmark, Bookmark}
            ]),
        Tail = lists:nthtail(I, AllResults),
        ?assertEqual(Tail, Results)
    end, lists:seq(1, num_inner() + num_outer(), 250)).


get_wkt_test(Idx) ->
    Query1 = <<"Point (0 0)">>,
    Query2 = easton_shapes:point(0, 0),

    {ok, Results1} = easton_index:search(Idx, Query1, [nearest, {limit, 10}]),
    {ok, Results2} = easton_index:search(Idx, Query2, [nearest, {limit, 10}]),

    ?assertEqual(10, length(Results1)),
    ?assertEqual(Results2, Results1).


get_bbox_test(Idx) ->
    Query = [-1, -1, 1, 1],
    {ok, Results1} = easton_index:search(Idx, Query, [{limit, num_points()}]),
    Shape = easton_shapes:rectangle(-1, -1, 1, 1),
    {ok, Results2} = easton_index:search(Idx, Shape, [{limit, num_points()}]),
    ?assertEqual(num_inner(), length(Results1)),
    ?assertEqual(num_inner(), length(Results2)),
    ?assertEqual(Results2, Results1).


get_bbox_coord_test(Idx) ->
    % Checking that coordinate ordering doesn't matter.
    Query1 = [-1, -1,  1,  1],
    Query2 = [ 1,  1, -1, -1],
    Query3 = [-1,  1,  1, -1],
    Query4 = [ 1, -1, -1,  1],

    {ok, Results1} = easton_index:search(Idx, Query1, [{limit, num_points()}]),
    {ok, Results2} = easton_index:search(Idx, Query2, [{limit, num_points()}]),
    {ok, Results3} = easton_index:search(Idx, Query3, [{limit, num_points()}]),
    {ok, Results4} = easton_index:search(Idx, Query4, [{limit, num_points()}]),

    ?assertEqual(num_inner(), length(Results1)),
    ?assertEqual(Results2, Results1),
    ?assertEqual(Results3, Results1),
    ?assertEqual(Results4, Results1).


get_circle_test(Idx) ->
    % Not a whole lot I can assert on here. I could try grabbing
    % the whole set and making sure only points inside the radius
    % are found but I hesitate to try that considering the
    % ellipsoidal calculations.
    Query = {0, 0, 1},
    {ok, Results} = easton_index:search(Idx, Query, [{limit, num_points()}]),
    ?assert(length(Results) < 1000).


get_ellipse_test(Idx) ->
    % Same as circle test
    Query = {0, 0, 1, 0.5},
    {ok, Results} = easton_index:search(Idx, Query, [{limit, num_points()}]),
    ?assert(length(Results) < 1000).


get_empty_shapes_test(Idx) ->
    EmptyBBox = [0, 0, 0, 0],
    EmptyCircle = {0, 0, 0},
    EmptyEllipse = {0, 0, 0, 0},
    ?assertMatch({ok, _}, easton_index:search(Idx, EmptyBBox)),
    ?assertMatch({ok, _}, easton_index:search(Idx, EmptyCircle)),
    ?assertMatch({ok, _}, easton_index:search(Idx, EmptyEllipse)).


get_empty_wkt_test(Idx) ->
    EmptyWKT = <<"POLYGON(EMPTY)">>,
    ?assertThrow({error, _}, easton_index:search(Idx, EmptyWKT)).


get_with_req_srid_test(Idx) ->
    % No idea how to assert that these are actually doing
    % anything but hopefully just making sure we don't
    % segfault is enough for now.
    %
    % Default SRID is 4326 which is WGS84. I picked
    % NAD83 scientifically by Googling for another
    % common SRID.
    NAD83 = 4269,
    % We're just going to create a list of queries and
    % then execute them all asserting that we got
    % results.
    Queries = [
        % WKB
        easton_shapes:point(),
        easton_shapes:linestring(),
        easton_shapes:polygon(),
        easton_shapes:polygon_with_hole(),
        easton_shapes:multipoint(),
        easton_shapes:multilinestring(),
        easton_shapes:multipolygon(),
        easton_shapes:geometrycollection(),
        % WKT
        <<"POINT (30 10)">>,
        <<"LINESTRING (30 10, 10 30, 40 40)">>,
        <<"POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))">>,
        <<"POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))">>,
        % BBox
        [-1, -1,  1,  1],
        [ 1,  1, -1, -1],
        [-1,  1,  1, -1],
        [ 1, -1, -1,  1],
        % Circles
        {0, 0, 1},
        {10, 12, 100},
        {-5, -2, 12},
        % Ellipses
        {0, 0, 1, 5},
        {10, 12, 50, 12},
        {5, 1, 240, 4}
    ],
    Options = [nearest, {limit, 1}, {req_srid, NAD83}],
    lists:map(fun(Q) ->
        ?assertMatch({ok, [_]}, easton_index:search(Idx, Q, Options))
    end, Queries).




% This function sets up an index containing
% 10K points that exist in two concentric
% squares centered on the origin. 5K points
% will exist between (-1,-1) and (1, 1). The
% second 5K points will exist between (-2, 2)
% and (2, 2) and will not be placed within
% the smaller square. It looks something like
% such:
%
%    O O O O
%    O I I O
%    O I I O
%    O O O O
%
% Ids for each will be autogenerated so that
% we can check queries after searching.
open_idx() ->
    case filelib:is_dir(idx_dir()) of
        true ->
            ok = easton_index:destroy(idx_dir());
        false ->
            ok
    end,
    {ok, Idx} = easton_index:open(idx_dir()),
    gen(Idx, num_inner(), inner),
    gen(Idx, num_outer(), outer),
    Idx.


close_idx(Idx) ->
    ok = easton_index:close(Idx).


gen(_Idx, 0, _Square) ->
    ok;
gen(Idx, Num, Square) ->
    {Id, Geom} = gen_point(Num, Square),
    ok = easton_index:update(Idx, Id, Geom),
    gen(Idx, Num - 1, Square).


gen_point(I, Square) ->
    IdNum = if Square == outer -> I + num_inner(); true -> I end,
    Id = iolist_to_binary(io_lib:format("~6..0b", [IdNum])),
    {X, Y} = gen_point(Square),
    {Id, easton_shapes:point(X, Y)}.


gen_point(inner) ->
    % Inner square is easy square. The
    % slightly-less than 1 and 2 numbers
    % are to avoid edge effects for queries.
    X = 1.999 * random:uniform() - 0.999,
    Y = 1.999 * random:uniform() - 0.999,
    {X, Y};
gen_point(outer) ->
    % Outer square is harder square.
    % This works by generating a random point
    % in a unit square and then transposing it
    % randomly to one of the twelve possible unit
    % squares that make up the outer ring.
    X = random:uniform(),
    Y = random:uniform(),
    S = random:uniform(12),
    % This numbering starts in the top left
    % and runs clockwise.
    {OffX, OffY} = case S of
        1  -> {-2.0,  1.0};
        2  -> {-1.0,  1.0};
        3  -> { 0.0,  1.0};
        4  -> { 1.0,  1.0};
        5  -> { 1.0,  0.0};
        6  -> { 1.0, -1.0};
        7  -> { 1.0, -2.0};
        8  -> { 0.0, -2.0};
        9  -> {-1.0, -2.0};
        10 -> {-2.0, -2.0};
        11 -> {-2.0, -1.0};
        12 -> {-2.0,  0.0}
    end,
    {X + OffX, Y + OffY}.
