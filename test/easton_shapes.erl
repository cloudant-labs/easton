-module(easton_shapes).


-export([
    point/2,
    rectangle/4
]).


-export([
    moving/5,

    point/0,
    linestring/0,
    polygon/0,
    polygon_with_hole/0,
    multipoint/0,
    multilinestring/0,
    multipolygon/0,
    geometrycollection/0
]).


moving({ShapeProps}, LowV, HighV, StartTime, EndTime) ->
    TemporalProps = [
        {<<"lowV">>, LowV},
        {<<"highV">>, HighV},
        {<<"start">>, StartTime},
        {<<"end">>, EndTime}
    ],
    {ShapeProps ++ TemporalProps}.


point(X, Y) ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [X, Y]}
    ]}.


rectangle(X1, Y1, X2, Y2) ->
    {[
        {<<"type">>, <<"Polygon">>},
        {<<"coordinates">>, [
            [
                [X1, Y1],
                [X1, Y2],
                [X2, Y2],
                [X2, Y1],
                [X1, Y1]
            ]
        ]}
    ]}.


point() ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [100.0, 0.0]}
    ]}.


linestring() ->
    {[
        {<<"type">>, <<"LineString">>},
        {<<"coordinates">>, [
            [100.0, 0.0], [101.0, 1.0]
        ]}
    ]}.


polygon() ->
    {[
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
    ]}.


polygon_with_hole() ->
    {[
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
    ]}.


multipoint() ->
    {[
        {<<"type">>, <<"MultiPoint">>},
        {<<"coordinates">>, [
            [100.0, 0.0],
            [101.0, 1.0]
        ]}
    ]}.


multilinestring() ->
    {[
        {<<"type">>, <<"MultiLineString">>},
        {<<"coordinates">>, [
            [[100.0, 0.0], [101.0, 1.0]],
            [[102.0, 2.0], [103.0, 3.0]]
        ]}
    ]}.


multipolygon() ->
    {[
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
    ]}.


geometrycollection() ->
    {[
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
    ]}.
