-module(easton_03_indexing_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() -> "test/test_03".


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
                fun multipoint_tests_/1,
                fun multilinestring_tests_/1,
                fun multipolygon_tests_/1,
                fun geometrycollection_tests_/1
            ]
        }
    }.


point_tests_(Idx) ->
    Point = {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [100.0, 0.0]}
    ]},
    {"Point", shape_tests(Idx, Point)}.


linestring_tests_(Idx) ->
    LineString = {[
        {<<"type">>, <<"LineString">>},
        {<<"coordinates">>, [
            [100.0, 0.0], [101.0, 1.0]
        ]}
    ]},
    {"LineString", shape_tests(Idx, LineString)}.


polygon_tests_(Idx) ->
    Polygon = {[
        {<<"type">>, <<"Polygon">>},
        {<<"coordinates">>, [
            % Outer ring
            [
                [100.0, 0.0],
                [101.0, 0.0],
                [101.0, 1.0],
                [100.0, 1.0],
                [100.0, 0.0]
            ]
        ]}
    ]},
    {"Polygon - Simple", shape_tests(Idx, Polygon)}.


polygon_with_holes_tests_(Idx) ->
    PolygonWithHoles = {[
        {<<"type">>, <<"Polygon">>},
        {<<"coordinates">>, [
            % Outer ring
            [
                [100.0, 0.0],
                [101.0, 0.0],
                [101.0, 1.0],
                [100.0, 1.0],
                [100.0, 0.0]
            ],
            % Inner ring
            [
                [100.2, 0.2],
                [100.8, 0.2],
                [100.8, 0.8],
                [100.2, 0.8],
                [100.2, 0.2]
            ]
        ]}
    ]},
    {"Polygon - With Holes", shape_tests(Idx, PolygonWithHoles)}.


multipoint_tests_(Idx) ->
    MultiPoint = {[
        {<<"type">>, <<"MultiPoint">>},
        {<<"coordinates">>, [
            [100.0, 0.0],
            [101.0, 1.0]
        ]}
    ]},
    {"MultiPoint", shape_tests(Idx, MultiPoint)}.


multilinestring_tests_(Idx) ->
    MultiLineString = {[
        {<<"type">>, <<"MultiLineString">>},
        {<<"coordinates">>, [
            [[100.0, 0.0], [101.0, 1.0]],
            [[102.0, 2.0], [103.0, 3.0]]
        ]}
    ]},
    {"MultiLineString", shape_tests(Idx, MultiLineString)}.


multipolygon_tests_(Idx) ->
    MultiPolygon = {[
        {<<"type">>, <<"MultiPolygon">>},
        {<<"coordinates">>, [
            [
                [
                    [102.0, 2.0],
                    [103.0, 2.0],
                    [103.0, 3.0],
                    [102.0, 3.0],
                    [102.0, 2.0]
                ]
            ],
            [
                [
                    [100.0, 0.0],
                    [101.0, 0.0],
                    [101.0, 1.0],
                    [100.0, 1.0],
                    [100.0, 0.0]
                ],
                [
                    [100.2, 0.2],
                    [100.8, 0.2],
                    [100.8, 0.8],
                    [100.2, 0.8],
                    [100.2, 0.2]
                ]
            ]

        ]}
    ]},
    {"MultiPolygon", shape_tests(Idx, MultiPolygon)}.


geometrycollection_tests_(Idx) ->
    GeometryCollection = {[
        {<<"type">>, <<"GeometryCollection">>},
        {<<"geometries">>, [
            {[
                {<<"type">>, <<"Point">>},
                {<<"coordinates">>, [100.0, 0.0]}
            ]},
            {[
                {<<"type">>, <<"LineString">>},
                {<<"coordinates">>, [
                    [101.0, 0.0],
                    [102.0, 1.0]
                ]}
            ]}
        ]}
    ]},
    {"GeometryCollection", shape_tests(Idx, GeometryCollection)}.



shape_tests(Idx, Shape) ->
    [
        ?_assertEqual(ok, easton_index:update(Idx, <<"foo">>, Shape)),
        ?_assertEqual({ok, 1}, easton_index:doc_id_num(Idx)),
        ?_assertEqual({ok, 1}, easton_index:doc_count(Idx)),
        ?_assertEqual(ok, easton_index:remove(Idx, <<"foo">>)),
        ?_assertEqual({ok, 1}, easton_index:doc_id_num(Idx)),
        ?_assertEqual({ok, 0}, easton_index:doc_count(Idx))
    ].
