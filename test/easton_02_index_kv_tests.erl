-module(easton_02_index_kv_tests).

-include_lib("eunit/include/eunit.hrl").


open_idx() ->
    Files = filelib:wildcard("test/test02/*"),
    lists:foreach(fun file:delete/1, Files),
    {ok, Idx} = easton_index:open("test/test_02"),
    Idx.


close_idx(Idx) ->
    easton_index:close(Idx).


basic_test_() ->
    {"Basic KV tests",
        {foreach,
            fun open_idx/0,
            fun close_idx/1,
            [
                fun put_one_/1,
                fun put_put_/1,
                fun get_missing_/1,
                fun del_missing_/1,
                fun put_get_/1,
                fun put_get_put_get_/1,
                fun put_del_get_/1,
                fun put_get_del_/1
            ]
        }
    }.


put_one_(Idx) ->
    ?_assertEqual(ok, easton_index:put(Idx, p, bar)).


put_put_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, pp, bar)),
        ?_assertEqual(ok, easton_index:put(Idx, pp, baz))
    ].


get_missing_(Idx) ->
    [
        ?_assertEqual(false, easton_index:get(Idx, not_a_key)),
        ?_assertEqual(default, easton_index:get(Idx, not_a_key, default))
    ].


del_missing_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:del(Idx, not_a_key))
    ].


put_get_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, pg, ohai)),
        ?_assertEqual({pg, ohai}, easton_index:get(Idx, pg)),
        ?_assertEqual(ohai, easton_index:get(Idx, pg, undefined))
    ].


put_get_put_get_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, pgpg, v1)),
        ?_assertEqual({pgpg, v1}, easton_index:get(Idx, pgpg)),
        ?_assertEqual(v1, easton_index:get(Idx, pgpg, undefined)),
        ?_assertEqual(ok, easton_index:put(Idx, pgpg, v2)),
        ?_assertEqual({pgpg, v2}, easton_index:get(Idx, pgpg))
    ].


put_del_get_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, pdg, thing)),
        ?_assertEqual(ok, easton_index:del(Idx, pdg)),
        ?_assertEqual(false, easton_index:get(Idx, pdg))
    ].


put_get_del_(Idx) ->
    [
        ?_assertEqual(ok, easton_index:put(Idx, pgd, newthing)),
        ?_assertEqual({pgd, newthing}, easton_index:get(Idx, pgd)),
        ?_assertEqual(ok, easton_index:del(Idx, pgd))
    ].

