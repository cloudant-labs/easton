-module(easton_04_indexing_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/04".


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
    {"Basic Indexing Tests",
        {foreach,
            fun open_idx/0,
            fun close_idx/1,
            [
                fun point_tests_/1,
                fun linestring_tests_/1,
                fun polygon_tests_/1,
                fun polygon_with_hole_tests_/1,
                fun multipoint_tests_/1,
                fun multilinestring_tests_/1,
                fun multipolygon_tests_/1,
                fun geometrycollection_tests_/1
            ]
        }
    }.


point_tests_(Idx) ->
    {"Point", shape_tests(Idx, point)}.


linestring_tests_(Idx) ->
    {"LineString", shape_tests(Idx, linestring)}.


polygon_tests_(Idx) ->
    {"Polygon - Simple", shape_tests(Idx, polygon)}.


polygon_with_hole_tests_(Idx) ->
    {"Polygon - With Hole", shape_tests(Idx, polygon_with_hole)}.


multipoint_tests_(Idx) ->
    {"MultiPoint", shape_tests(Idx, multipoint)}.


multilinestring_tests_(Idx) ->
    {"MultiLineString", shape_tests(Idx, multilinestring)}.


multipolygon_tests_(Idx) ->
    {"MultiPolygon", shape_tests(Idx, multipolygon)}.


geometrycollection_tests_(Idx) ->
    {"GeometryCollection", shape_tests(Idx, geometrycollection)}.


shape_tests(Idx, Name) ->
    Shape = easton_shapes:Name(),
    [
        ?_assertEqual(ok, easton_index:update(Idx, <<"foo">>, Shape)),
        ?_assertEqual({ok, 1}, easton_index:doc_id_num(Idx)),
        ?_assertEqual({ok, 1}, easton_index:doc_count(Idx)),
        ?_assertEqual(
            {ok, [{<<"foo">>, 0.0, Shape}]},
            easton_index:search(Idx, Shape, [include_geom])
        ),
        ?_assertEqual(ok, easton_index:remove(Idx, <<"foo">>)),
        ?_assertEqual({ok, 1}, easton_index:doc_id_num(Idx)),
        ?_assertEqual({ok, 0}, easton_index:doc_count(Idx)),
        ?_assertEqual({ok, []}, easton_index:search(Idx, Shape))
    ].
