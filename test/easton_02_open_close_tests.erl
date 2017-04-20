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
    {ok, {_IdxType, IdxPid} = Idx} = open_index(),
    ?assertEqual(true, is_pid(IdxPid)),
    OsPid = easton_index:os_pid(Idx),
    ?assertEqual(true, is_os_pid_alive(OsPid)),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(IdxPid)),
    ?assertEqual(false, is_os_pid_alive(OsPid)).


open_close_sync_test() ->
    {ok, {_IdxType, IdxPid} = Idx} = open_index(),
    ?assert(is_pid(IdxPid)),
    OsPid = easton_index:os_pid(Idx),
    ?assertEqual(true, is_os_pid_alive(OsPid)),
    ok = easton_index:sync(Idx),
    ok = easton_index:close(Idx),
    ?assertEqual(false, is_process_alive(IdxPid)),
    ?assertEqual(false, is_os_pid_alive(OsPid)).


persistance_test() ->
    Id = <<"foo">>,
    Point = easton_shapes:point(0.0, 0.0),

    % Open an index and store a point
    {ok, {_IdxType1, IdxPid1} = Idx1} = open_index(),
    OsPid1 = easton_index:os_pid(Idx1),
    ?assertEqual(true, is_os_pid_alive(OsPid1)),
    ok = easton_index:update(Idx1, Id, Point),
    ?assertEqual({ok, [{Id, 0.0}]}, easton_index:search(Idx1, Point)),
    ok = easton_index:close(Idx1),
    ?assertEqual(false, is_process_alive(IdxPid1)),
    ?assertEqual(false, is_os_pid_alive(OsPid1)),

    % Reopen the index and see if the point
    % still exists.
    {ok, Idx2} = easton_index:open(idx_dir()),
    OsPid2 = easton_index:os_pid(Idx2),
    ?assertEqual(true, is_os_pid_alive(OsPid2)),
    ?assert(OsPid2 /= OsPid1),
    ?assertEqual({ok, [{Id, 0.0}]}, easton_index:search(Idx2, Point)),
    ok = easton_index:close(Idx2).
