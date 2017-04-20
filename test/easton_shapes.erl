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

-module(easton_shapes).


-export([
    point/2,
    point/3,
    point/4,
    rectangle/4
]).


-export([
    moving/5,
    historical/3,

    point/0,
    point3d/0,
    point4d/0,
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

historical({ShapeProps}, StartTime, EndTime) ->
    HistoricalProps = [
        {<<"start">>, StartTime},
        {<<"end">>, EndTime}
    ],
    {ShapeProps ++ HistoricalProps}.


point(X, Y) ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [X, Y]}
    ]}.

point(X, Y, Z) ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [X, Y, Z]}
    ]}.

point(X, Y, Z, M) ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [X, Y, Z, M]}
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

point3d() ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [100.0, 0.0, 1.0]}
    ]}.

point4d() ->
    {[
        {<<"type">>, <<"Point">>},
        {<<"coordinates">>, [100.0, 0.0, 1.0, 1.0]}
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
