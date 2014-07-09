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
-module(easton_geojson).

-include("easton_wkb.hrl").

-export([
    to_wkb/1
]).


to_wkb({Geom}) ->
    {_Dims, _Count, Bin} = convert({Geom}),
    {ok, Bin}.


convert({Props}) ->
    Type = case lists:keyfind(?JSON_TYPE, 1, Props) of
        {_, Type0} -> Type0;
        false -> throw({invalid_geojson, {missing_type, {Props}}})
    end,
    convert(Type, Props).


convert(?JSON_POINT, Props) ->
    Coord = get_coords(Props),
    {Dims, 1, Bin} = coord_to_wkb(Coord),
    {Dims, 1, mkbin(?WKB_POINT, Dims, Bin)};

convert(?JSON_LINESTRING, Props) ->
    Coords = get_coords(Props),
    {Dims, Count, Bin} = coords_to_wkb(Coords),
    {Dims, 1, mkbin(?WKB_LINESTRING, Dims, Count, Bin)};

convert(?JSON_POLYGON, Props) ->
    Coords = get_coords(Props),
    {Dims, Count, Bin} = rings_to_wkb(Coords),
    {Dims, 1, mkbin(?WKB_POLYGON, Dims, Count, Bin)};

convert(?JSON_MULTIPOINT, Props) ->
    Coords = get_coords(Props),
    {Dims, Count, Bin} = multi(fun coord_to_wkb/1, Coords),
    {Dims, 1, mkbin(?WKB_MULTIPOINT, Dims, Count, Bin)};

convert(?JSON_MULTILINESTRING, Props) ->
    Coords = get_coords(Props),
    {Dims, Count, Bin} = multi(fun coords_to_wkb/1, Coords),
    {Dims, 1, mkbin(?WKB_MULTILINESTRING, Dims, Count, Bin)};

convert(?JSON_MULTIPOLYGON, Props) ->
    Coords = get_coords(Props),
    {Dims, Count, Bin} = multi(fun rings_to_wkb/1, Coords),
    {Dims, 1, mkbin(?WKB_MULTIPOLYGON, Dims, Count, Bin)};

convert(?JSON_GEOMETRYCOLLECTION, Props) ->
    Geoms = get_geoms(Props),
    {Dims, Count, Bin} = multi(fun convert/1, Geoms),
    {Dims, 1, mkbin(?WKB_GEOMETRYCOLLECTION, Dims, Count, Bin)};

convert(_, Props) ->
    throw({invalid_geojson, {bad_type, {Props}}}).


get_coords(Props) ->
    case lists:keyfind(?JSON_COORDINATES, 1, Props) of
        {_, Coords} ->
            Coords;
        false ->
            throw({invalid_geojson, {missing_coordinates, {Props}}})
    end.


get_geoms(Props) ->
    case lists:keyfind(?JSON_GEOMETRIES, 1, Props) of
        {_, Geoms} ->
            Geoms;
        false ->
            throw({invalid_geojson, {missing_geometries, {Props}}})
    end.


multi(_ToWKB, []) ->
    throw({invalid_geojson, empty_multi_geometry});
multi(ToWKB, [Coords]) ->
    ToWKB(Coords);
multi(ToWKB, [Coords | Rest]) ->
    {Dims, Count, AccBin} = ToWKB(Rest),
    case ToWKB(Coords) of
        {Dims, 1, NewBin} ->
            {Dims, Count + 1, <<NewBin/binary, AccBin/binary>>};
        {_BadDims, _, _} ->
            throw({invalid_geojson, mismatched_dimensions})
    end.


rings_to_wkb([]) ->
    throw({invalid_geojson, empty_polygon});
rings_to_wkb([Ring]) ->
    ring_to_wkb(Ring);
rings_to_wkb([Ring | Rest]) ->
    {Dims, Count, AccBin} = rings_to_wkb(Rest),
    case ring_to_wkb(Ring) of
        {Dims, 1, NewBin} ->
            {Dims, Count + 1, <<NewBin/binary, AccBin/binary>>};
        {_BadDims, _, _} ->
            throw({invalid_geojson, mismatched_dimensions})
    end.


ring_to_wkb(Coords) ->
    {Dims, Count, Bin} = coords_to_wkb(Coords),
    {Dims, 1, <<Count:32/big-unsigned-integer, Bin/binary>>}.


coords_to_wkb([]) ->
    throw({invalid_geojson, empty_coordinates});
coords_to_wkb([Coord]) ->
    coord_to_wkb(Coord);
coords_to_wkb([Coord | Rest]) ->
    {Dims, Count, AccBin} = coords_to_wkb(Rest),
    case coord_to_wkb(Coord) of
        {Dims, 1, NewBin} ->
            {Dims, Count + 1, <<NewBin/binary, AccBin/binary>>};
        {_BadDims, _, _} ->
            throw({invalid_geojson, mismatched_dimensions})
    end.


coord_to_wkb([X, Y])
        when is_number(X), is_number(Y) ->
    {2, 1, <<X:64/float, Y:64/float>>};
coord_to_wkb([X, Y, Z])
        when is_number(X), is_number(Y), is_number(Z) ->
    {3, 1, <<X:64/float, Y:64/float, Z:64/float>>};
coord_to_wkb([X, Y, Z, M])
        when is_number(X), is_number(Y), is_number(Z), is_number(M) ->
    {4, 1, <<X:64/float, Y:64/float, Z:64/float, M:64/float>>};
coord_to_wkb(BadCoord) ->
    throw({invalid_geojson, {bad_coord, BadCoord}}).


mkbin(Type0, Dims, SubBin) ->
    Type = type_to_wkb(Type0, Dims),
    <<0:8/integer, Type:32/big-unsigned-integer, SubBin/binary>>.


mkbin(Type0, Dims, Count, SubBin) ->
    Type = type_to_wkb(Type0, Dims),
    <<0:8/integer, Type:32/big-unsigned-integer,
        Count:32/big-unsigned-integer, SubBin/binary>>.


type_to_wkb(Type, Dims) ->
	case Dims of
		2 -> Type;
		3 -> Type bor ?WKB_Z;
		4 -> (Type bor ?WKB_Z) bor ?WKB_M;
		_ -> throw({invalid_geojson, {bad_dimensions, Dims}})
	end.
