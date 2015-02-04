-module(easton_07_mvr_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/07".


open_idx() ->
    case filelib:is_dir(idx_dir()) of
        true ->
            ok = easton_index:destroy(idx_dir());
        false ->
            ok
    end,
    {ok, Idx} = easton_index:open(idx_dir(), [{type, mvrtree}]),
    Idx.


close_idx(Idx) ->
    easton_index:close(Idx).


basic_test_() ->
    {"Basic MVR Tests",
        {timeout, 10, [
            {setup,
                fun open_idx/0,
                fun close_idx/1,
                fun(Idx) -> {with, Idx, [
                    fun historical_shape_test/1
                ]} end
            }
        ]}
    }.


historical_shape_test(Idx) ->
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

    HistoricalShapes = [
        easton_shapes:historical(S, 0, 10) || S <- Shapes
    ],

    Triples = lists:zip3(Names, Shapes, HistoricalShapes),

    lists:foreach(fun({Name, Shape, HistoricalShape}) ->
        ?assertEqual(ok, easton_index:update(Idx, Name, HistoricalShape)),
        SearchOpts = [include_geom, {t_start, 0}, {t_end, 10}],
        ?assertMatch({ok, [_|_]}, easton_index:search(Idx, Shape, SearchOpts))
    end, Triples),

    NumShapes = length(Triples),
    ?assertEqual({ok, NumShapes}, easton_index:doc_count(Idx)),

    lists:foreach(fun({Name, Shape, HistoricalShape}) ->
        ?assertEqual(ok, easton_index:remove(Idx, Name))
    end, Triples),

    ?assertEqual({ok, 0}, easton_index:doc_count(Idx)).
