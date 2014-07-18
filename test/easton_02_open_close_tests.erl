-module(easton_02_open_close_tests).

-include_lib("eunit/include/eunit.hrl").


idx_dir() ->
    "idx/02".


open_index() ->
    case filelib:is_dir(idx_dir()) of
        true ->
            ok = easton_index:destroy(idx_dir());
        false ->
            ok
    end,
    easton_index:open(idx_dir()).


is_os_pid_alive(OsPid) ->
    Fmt = "kill -0 ~b",
    Cmd = lists:flatten(io_lib:format(Fmt, [OsPid])),
    case os:cmd(Cmd) of
        [] -> true;
        _ -> false
    end.


open_close_test() ->
    {ok, Idx} = open_index(),
    ?assert(is_pid(Idx)),
    OsPid = easton_index:os_pid(Idx),
    ?assertEqual(true, is_os_pid_alive(OsPid)),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(Idx)),
    ?assertEqual(false, is_os_pid_alive(OsPid)).


open_close_sync_test() ->
    {ok, Idx} = open_index(),
    ?assert(is_pid(Idx)),
    OsPid = easton_index:os_pid(Idx),
    ?assertEqual(true, is_os_pid_alive(OsPid)),
    ok = easton_index:sync(Idx),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(Idx)),
    ?assertEqual(false, is_os_pid_alive(OsPid)).


persistance_test() ->
    Id = <<"foo">>,
    Point = easton_shapes:point(0.0, 0.0),

    % Open an index and store a point
    {ok, Idx1} = open_index(),
    OsPid1 = easton_index:os_pid(Idx1),
    ?assertEqual(true, is_os_pid_alive(OsPid1)),
    ok = easton_index:update(Idx1, Id, Point),
    ?assertEqual({ok, [{Id, 0.0}]}, easton_index:search(Idx1, Point)),
    ok = easton_index:close(Idx1),
    ?assertEqual(false, is_process_alive(Idx1)),
    ?assertEqual(false, is_os_pid_alive(OsPid1)),

    % Reopen the index and see if the point
    % still exists.
    {ok, Idx2} = easton_index:open(idx_dir()),
    OsPid2 = easton_index:os_pid(Idx2),
    ?assertEqual(true, is_os_pid_alive(OsPid2)),
    ?assert(OsPid2 /= OsPid1),
    ?assertEqual({ok, [{Id, 0.0}]}, easton_index:search(Idx2, Point)),
    ok = easton_index:close(Idx2).
