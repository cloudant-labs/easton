-module(easton_02_open_close_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/02".

open_close_test() ->
    {ok, Idx} = easton_index:open(idx_dir()),
    ?assert(is_pid(Idx)),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(Idx)).


open_close_sync_test() ->
    {ok, Idx} = easton_index:open(idx_dir()),
    ?assert(is_pid(Idx)),
    ok = easton_index:sync(Idx),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(Idx)).