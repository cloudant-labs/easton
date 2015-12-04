-module(easton_99_fb_56316).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/99_fb_56316".


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


bookmark_test_() ->
    {"Regression: FB 56316",
        {timeout, 10, [
            {setup,
                fun open_idx/0,
                fun close_idx/1,
                fun(Idx) -> {with, Idx, [
                    fun multi_geometry_test/1
                ]} end
            }
        ]}
    }.


multi_geometry_test(Idx) ->
    lists:foreach(fun({Id, Geoms}) ->
        ?assertEqual(ok, easton_index:update(Idx, Id, Geoms))
    end, geometries()),
    BBox = easton_shapes:rectangle(-180, -90, 180, 90),
    {ok, Results} = easton_index:search(Idx, BBox),
    ?assertEqual(4, length(Results)),
    lists:foreach(fun({Id, _Geoms}) ->
        ?assertEqual(ok, easton_index:remove(Idx, Id))
    end, geometries()),
    ?assertEqual({ok, []}, easton_index:search(Idx, BBox)).


geometries() ->
    [
        {<<"10">>, [
            {[
                {<<"type">>, <<"Point">>},
                {<<"coordinates">>, [-78.8234112, 35.7637407]}
            ]},
            {[
                {<<"type">>, <<"Point">>},
                {<<"coordinates">>, [-78.777361, 35.9045565]}
            ]}
        ]},
        {<<"14">>, [
            {[
                {<<"type">>, <<"Point">>},
                {<<"coordinates">>, [-78.794028, 35.8497739]}
            ]},
            {[
                {<<"type">>, <<"Point">>},
                {<<"coordinates">>, [-78.9488454, 35.9652997]}
            ]}
        ]}
    ].
