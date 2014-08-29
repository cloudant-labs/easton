-module(easton_01_wkb_tests).

-include_lib("eunit/include/eunit.hrl").


point_test_() ->
    {"Point", roundtrip(point)}.

point3d_test_() ->
    {"Point3D", roundtrip(point3d)}.

point4d_test_() ->
    {"Point4D", roundtrip(point4d)}.

linestring_test_() ->
    {"LineString", roundtrip(linestring)}.


polygon_test_() ->
    {"Polygon", roundtrip(polygon)}.


polygon_with_hole_test_() ->
    {"Polygon With Hole", roundtrip(polygon_with_hole)}.


multipoint_test_() ->
    {"MultiPoint", roundtrip(multipoint)}.


multilinestring_test_() ->
    {"MultiLineString", roundtrip(multilinestring)}.


multipolygon_test_() ->
    {"MultiPolygon", roundtrip(multipolygon)}.


geometrycollection_test_() ->
    {"GeometryCollection", roundtrip(geometrycollection)}.


roundtrip(Name) ->
    Shape = easton_shapes:Name(),
    WKB = easton_geojson:to_wkb(Shape),
    GeoJson = easton_geojson:from_wkb(WKB),
    [?_assertEqual(Shape, GeoJson)].
