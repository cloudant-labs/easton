-module(easton_01_index_open_close_tests).

-include_lib("eunit/include/eunit.hrl").


open_close_test() ->
    {ok, Idx} = easton_index:open("test/test_01_idx_02"),
    ?assert(is_pid(Idx)),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(Idx)).


open_close_sync_test() ->
    {ok, Idx} = easton_index:open("test/test_01_idx_02"),
    ?assert(is_pid(Idx)),
    ok = easton_index:sync(Idx),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(Idx)).