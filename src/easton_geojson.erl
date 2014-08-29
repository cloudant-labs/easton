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

-include("easton_geojson.hrl").

-export([
    to_wkb/1,
    from_wkb/1
]).


to_wkb({Geom}) ->
    {_Dims, _Count, Bin} = convert({Geom}),
    Bin.


from_wkb(Bin) when is_binary(Bin) ->
    {Json, _Dims, RestBin} = parse(Bin),
    if RestBin == <<>> -> ok; true ->
        throw({invalid_wkb, trailing_data})
    end,
    Json.


convert({Props}) ->
    Type = case lists:keyfind(?JSON_TYPE, 1, Props) of
        {_, Type0} -> Type0;
        false -> throw({invalid_geojson, {missing_type, {Props}}})
    end,
    convert(Type, Props).


convert(?JSON_POINT, Props) ->
    Coord = get_coords(Props),
    {Dims, 1, Bin} = coord_to_wkb(Coord),
    {Dims, 1, mkbin(?WKB_POINT, Dims, 0, Bin)};

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
    ToWKB = fun coord_to_wkb/1,
    {Dims, Count, Bin} = json_multi(?WKB_POINT, ToWKB, Coords),
    {Dims, 1, mkbin(?WKB_MULTIPOINT, Dims, Count, Bin)};

convert(?JSON_MULTILINESTRING, Props) ->
    Coords = get_coords(Props),
    ToWKB = fun coords_to_wkb/1,
    {Dims, Count, Bin} = json_multi(?WKB_LINESTRING, ToWKB, Coords),
    {Dims, 1, mkbin(?WKB_MULTILINESTRING, Dims, Count, Bin)};

convert(?JSON_MULTIPOLYGON, Props) ->
    Coords = get_coords(Props),
    ToWKB = fun rings_to_wkb/1,
    {Dims, Count, Bin} = json_multi(?WKB_POLYGON, ToWKB, Coords),
    {Dims, 1, mkbin(?WKB_MULTIPOLYGON, Dims, Count, Bin)};

convert(?JSON_GEOMETRYCOLLECTION, Props) ->
    Geoms = get_geoms(Props),
    {Dims, Count, AccBins} = lists:foldl(fun(G, {Dims, Count, Bins}) ->
        case convert(G) of
            {NewDims, 1, NewBin} when Dims == undefined, Bins == [] ->
                {NewDims, 1, [NewBin]};
            {Dims, 1, NewBin} ->
                {Dims, Count + 1, [NewBin | Bins]};
            {_, _, _} ->
                throw({invalid_geojson, mismatched_dimensions})
        end
    end, {undefined, 0, []}, Geoms),
    FinalBin = iolist_to_binary(lists:reverse(AccBins)),
    {Dims, 1, mkbin(?WKB_GEOMETRYCOLLECTION, Dims, Count, FinalBin)};

convert(_, Props) ->
    throw({invalid_geojson, {bad_type, {Props}}}).


parse(<<0:8/integer, WKBType:32/big-unsigned-integer, Rest/binary>>) ->
    parse(big, wkb_to_type(WKBType), Rest);
parse(<<1:8/integer, WKBType:32/little-unsigned-integer, Rest/binary>>) ->
    parse(little, wkb_to_type(WKBType), Rest).


parse(Endian, {Dims, ?WKB_POINT}, WKB) ->
    {Coord, RestWKB} = wkb_to_coord(Endian, Dims, WKB),
    Json = {[
        {?JSON_TYPE, ?JSON_POINT},
        {?JSON_COORDINATES, Coord}
    ]},
    {Json, Dims, RestWKB};

parse(Endian, {Dims, ?WKB_LINESTRING}, WKB) ->
    {Coords, RestWKB} = wkb_to_coords(Endian, Dims, WKB),
    Json = {[
        {?JSON_TYPE, ?JSON_LINESTRING},
        {?JSON_COORDINATES, Coords}
    ]},
    {Json, Dims, RestWKB};

parse(Endian, {Dims, ?WKB_POLYGON}, WKB) ->
    {Rings, RestWKB} = wkb_to_rings(Endian, Dims, WKB),
    Json = {[
        {?JSON_TYPE, ?JSON_POLYGON},
        {?JSON_COORDINATES, Rings}
    ]},
    {Json, Dims, RestWKB};

parse(Endian, {Dims, ?WKB_MULTIPOINT}, WKB) ->
    FromWKB = fun wkb_to_coord/3,
    {Coords, RestWKB} = wkb_multi(Endian, ?WKB_POINT, Dims, FromWKB, WKB),
    Json = {[
        {?JSON_TYPE, ?JSON_MULTIPOINT},
        {?JSON_COORDINATES, Coords}
    ]},
    {Json, Dims, RestWKB};

parse(Endian, {Dims, ?WKB_MULTILINESTRING}, WKB) ->
    FromWKB = fun wkb_to_coords/3,
    {Coords, RestWKB} = wkb_multi(Endian, ?WKB_LINESTRING, Dims, FromWKB, WKB),
    Json = {[
        {?JSON_TYPE, ?JSON_MULTILINESTRING},
        {?JSON_COORDINATES, Coords}
    ]},
    {Json, Dims, RestWKB};

parse(Endian, {Dims, ?WKB_MULTIPOLYGON}, WKB) ->
    FromWKB = fun wkb_to_rings/3,
    {Coords, RestWKB} = wkb_multi(Endian, ?WKB_POLYGON, Dims, FromWKB, WKB),
    Json = {[
        {?JSON_TYPE, ?JSON_MULTIPOLYGON},
        {?JSON_COORDINATES, Coords}
    ]},
    {Json, Dims, RestWKB};

parse(Endian, {Dims, ?WKB_GEOMETRYCOLLECTION}, WKB) ->
    {Count, RestWKB} = wkb_to_count(Endian, WKB),
    {FinalGeoms, FinalWKB} = lists:foldl(fun(_, {Geoms, WKBTail}) ->
        case parse(WKBTail) of
            {G, Dims, NewTail} ->
                {[G | Geoms], NewTail};
            {_, _BadDims, _} ->
                throw({invalid_wkb, mismatched_dimensions})
        end
    end, {[], RestWKB}, lists:seq(1, Count)),
    Json = {[
        {?JSON_TYPE, ?JSON_GEOMETRYCOLLECTION},
        {?JSON_GEOMETRIES, lists:reverse(FinalGeoms)}
    ]},
    {Json, Dims, FinalWKB};

parse(_Endian, {_Dims, Type}, _WKB) ->
    throw({invalid_wkb, {bad_type, Type}}).


get_coords(Props) ->
    case lists:keyfind(?JSON_COORDINATES, 1, Props) of
        {_, Coords} when is_list(Coords) ->
            Coords;
        {_, _} ->
            throw({invalid_geojson, {bad_coordinates, {Props}}});
        false ->
            throw({invalid_geojson, {missing_coordinates, {Props}}})
    end.


get_geoms(Props) ->
    case lists:keyfind(?JSON_GEOMETRIES, 1, Props) of
        {_, Geoms} when is_list(Geoms) ->
            Geoms;
        {_, _} ->
            throw({invalid_geojson, {bad_geometries, {Props}}});
        false ->
            throw({invalid_geojson, {missing_geometries, {Props}}})
    end.


json_multi(_Type, _ToWKB, []) ->
    throw({invalid_geojson, empty_multi_geometry});
json_multi(Type, ToWKB, [Coords]) ->
    {Dims, Count, AccBin} = ToWKB(Coords),
    {Dims, 1, mkbin(Type, Dims, Count, AccBin)};
json_multi(Type, ToWKB, [Coords | Rest]) ->
    {Dims, Count, AccBin} = json_multi(Type, ToWKB, Rest),
    case json_multi(Type, ToWKB, [Coords]) of
        {Dims, 1, NewBin} ->
            {Dims, Count + 1, <<NewBin/binary, AccBin/binary>>};
        {_BadDims, _, _} ->
            throw({invalid_geojson, mismatched_dimensions})
    end.


wkb_multi(Endian, Type, Dims, FromWKB, WKB) ->
    {Count, RestWKB} = wkb_to_count(Endian, WKB),
    {FinalCoords, FinalTail} = lists:foldl(fun(_, {CoordAcc, WKBTail}) ->
        {SubEndian, NewTail1} = wkb_check_type(Type, Dims, WKBTail),
        {Coords, NewTail2} = FromWKB(SubEndian, Dims, NewTail1),
        {[Coords | CoordAcc], NewTail2}
    end, {[], RestWKB}, lists:seq(1, Count)),
    {lists:reverse(FinalCoords), FinalTail}.


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


wkb_to_rings(Endian, Dims, WKB) ->
    {Count, RestWKB} = wkb_to_count(Endian, WKB),
    {FinalRings, FinalWKB} = lists:foldl(fun(_, {Rings, WKBTail}) ->
        {Ring, NewTail} = wkb_to_coords(Endian, Dims, WKBTail),
        {[Ring | Rings], NewTail}
    end, {[], RestWKB}, lists:seq(1, Count)),
    {lists:reverse(FinalRings), FinalWKB}.


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


wkb_to_coords(Endian, Dims, WKB) ->
    {Count, RestWKB} = wkb_to_count(Endian, WKB),
    {FinalCoords, FinalWKB} = lists:foldl(fun(_, {Coords, WKBTail}) ->
        {C, NewTail} = wkb_to_coord(Endian, Dims, WKBTail),
        {[C | Coords], NewTail}
    end, {[], RestWKB}, lists:seq(1, Count)),
    {lists:reverse(FinalCoords), FinalWKB}.


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


wkb_to_coord(big, 2, <<X:64/big-float, Y:64/big-float, R/binary>>) ->
    {[X, Y], R};
wkb_to_coord(big, 3, <<X:64/big-float, Y:64/big-float, Z:64/big-float,
        R/binary>>) ->
    {[X, Y, Z], R};
wkb_to_coord(big, 4, <<X:64/big-float, Y:64/big-float, Z:64/big-float,
        M:64/big-float, R/binary>>) ->
    {[X, Y, Z, M], R};
wkb_to_coord(little, 2, <<X:64/little-float, Y:64/little-float, R/binary>>) ->
    {[X, Y], R};
wkb_to_coord(little, 3, <<X:64/little-float, Y:64/little-float,
        Z:64/little-float, R/binary>>) ->
    {[X, Y, Z], R};
wkb_to_coord(little, 4, <<X:64/little-float, Y:64/little-float,
        Z:64/little-float, M:64/little-float, R/binary>>) ->
    {[X, Y, Z, M], R};
wkb_to_coord(_Endian, _Dims, WKB) ->
    throw({invalid_wkb, {bad_coord, WKB}}).


mkbin(?WKB_POINT = Type0, Dims, _Count, SubBin) ->
    Type = type_to_wkb(Type0, Dims),
    <<0:8/integer, Type:32/big-unsigned-integer, SubBin/binary>>;

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


wkb_to_type(Type) ->
    case Type of
        _ when ((Type band ?WKB_Z) == ?WKB_Z) and ((Type band ?WKB_M) == ?WKB_M) ->
            {4, Type band ?WKB_TYPE_FILTER};
        _ when (Type band ?WKB_Z) == ?WKB_Z orelse (Type band ?WKB_M) == ?WKB_M ->
            {3, Type band ?WKB_TYPE_FILTER};
        _ ->
            {2, Type}
    end.


wkb_to_count(big, <<Count:32/big-unsigned-integer, Rest/binary>>) ->
    if Count > 0 -> ok; true ->
        throw({invalid_wkb, bad_count})
    end,
    {Count, Rest};
wkb_to_count(little, <<Count:32/little-unsigned-integer, Rest/binary>>) ->
    if Count > 0 -> ok; true ->
        throw({invalid_wkb, bad_count})
    end,
    {Count, Rest};
wkb_to_count(_Endian, WKB) ->
    throw({invalid_wkb, {bad_count, WKB}}).


wkb_check_type(Type, Dims, <<0:8/integer, WKBType:32/big-unsigned-integer,
        Rest/binary>>) ->
    case wkb_to_type(WKBType) of
        {Dims, Type} ->
            {big, Rest};
        {Dims, _BadType} ->
            throw({invalid_wkb, multi_type_mismatch});
        {_BadDims, _} ->
            throw({invalid_wkb, multi_dimension_mismatch})
    end;
wkb_check_type(Type, Dims, <<1:8/integer, WKBType:32/little-unsigned-integer,
        Rest/binary>>) ->
    case wkb_to_type(WKBType) of
        {Dims, Type} ->
            {little, Rest};
        {Dims, _BadType} ->
            throw({invalid_wkb, multi_type_mismatch});
        {_BadDims, _} ->
            throw({invalid_wkb, multi_dimension_mismatch})
    end.
