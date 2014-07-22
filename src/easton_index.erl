-module(easton_index).
-behavior(gen_server).

-export([
    open/1,
    open/2,
    close/1,
    sync/1,

    destroy/1,
    destroy/2,

    put/3,
    get/2,
    get/3,
    del/2,

    info/1,
    doc_id_num/1,
    doc_count/1,
    os_pid/1,

    update/3,
    remove/2,
    search/2,
    search/3
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    kill_monitor/2,
    kill_monitor/1
]).


-include("easton_constants.hrl").


-record(st, {
    parent,
    port,
    os_pid,
    killer,
    idx_dir,
    closing = false
}).


-define(EXE_NAME, "easton_index").
-define(PORT_OPTS, [{packet, 4}, binary, exit_status, nouse_stdio, hide]).


open(Directory) ->
    open(Directory, []).


open(Directory, Opts) ->
    ensure_idx_dir(Directory),

    CsMapDir = get_cs_map_dir(Opts),
    Env = {env, [{"EASTON_CS_MAP_DIR", CsMapDir}]},

    Cmd = {run, [
        {index_directory, iolist_to_binary(Directory)},
        {index_type, get_index_type(Opts)},
        {dimensions, get_index_dimensions(Opts)},
        {srid, get_index_srid(Opts)}
    ]},

    Arg = {self(), Directory, Cmd, [Env]},
    proc_lib:start_link(?MODULE, init, [Arg]).


close(Index) ->
    Ref = erlang:monitor(process, Index),
    case gen_server:call(Index, close) of
        BinResp when is_binary(BinResp) ->
            case binary_to_term(BinResp) of
                {ok, true} ->
                    receive
                        {'DOWN', Ref, process, Index, _} ->
                            ok
                    after 5000 ->
                        erlang:demonitor(Ref, [flush]),
                        throw({timeout, close})
                    end;
                Else ->
                    throw(Else)
                end;
        Else ->
            erlang:demonitor(Ref, [flush]),
            throw(Else)
    end.


sync(Index) ->
    case cmd(Index, ?EASTON_COMMAND_SYNC, true) of
        {ok, true} ->
            ok;
        Else ->
            throw(Else)
    end.


destroy(Directory) ->
    destroy(Directory, []).


destroy(Directory, Opts) ->
    case filelib:is_dir(Directory) of
        true ->
            ok;
        false ->
            throw({invalid_index, Directory})
    end,

    CsMapDir = get_cs_map_dir(Opts),
    Env = {env, [{"EASTON_CS_MAP_DIR", CsMapDir}]},
    Cmd = {destroy, iolist_to_binary(Directory)},

    {ok, Port, OsPid} = open_index(Cmd, [Env]),
    erlang:spawn(?MODULE, kill_monitor, [self(), OsPid]),
    receive
        {Port, {exit_status, 0}} ->
            ok;
        {Port, Else} ->
            throw(Else)
        after 5000 ->
            throw(timeout)
    end.


info(Index) ->
    case cmd(Index, ?EASTON_COMMAND_GET_INDEX_INFO, true) of
        {ok, Info} ->
            DiskSize = get_disk_size(Index),
            {ok, [{disk_size, DiskSize} | Info]};
        Else ->
            throw(Else)
    end.


doc_id_num(Index) ->
    {ok, Info} = info(Index),
    {_, DocIdNum} = lists:keyfind(doc_id_num, 1, Info),
    {ok, DocIdNum}.


doc_count(Index) ->
    {ok, Info} = info(Index),
    {_, DocCount} = lists:keyfind(doc_count, 1, Info),
    {ok, DocCount}.


os_pid(Index) ->
    gen_server:call(Index, os_pid, infinity).


put(Index, Key, Value) ->
    case cmd(Index, ?EASTON_COMMAND_PUT_USER_KV, {t2b(Key), t2b(Value)}) of
        ok ->
            ok;
        Else ->
            throw(Else)
    end.


get(Index, Key) ->
    case cmd(Index, ?EASTON_COMMAND_GET_USER_KV, t2b(Key)) of
        {ok, VBin} ->
            {Key, binary_to_term(VBin, [safe])};
        false ->
            false;
        Else ->
            throw(Else)
    end.


get(Index, Key, Default) ->
    case ?MODULE:get(Index, Key) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.


del(Index, Key) ->
    case cmd(Index, ?EASTON_COMMAND_DEL_USER_KV, t2b(Key)) of
        ok ->
            ok;
        Else ->
            throw(Else)
    end.


update(Index, DocId, {_Props} = Geometry) ->
    update(Index, DocId, [Geometry]);

update(Index, DocId, Geometries) ->
    WKBs = [easton_geojson:to_wkb(G) || G <- Geometries],
    case cmd(Index, ?EASTON_COMMAND_UPDATE_ENTRIES, {DocId, WKBs}) of
        ok ->
            ok;
        Else ->
            throw(Else)
    end.


remove(Index, DocId) ->
    case cmd(Index, ?EASTON_COMMAND_REMOVE_ENTRIES, DocId) of
        ok ->
            ok;
        Else ->
            throw(Else)
    end.


search(Index, Geometry) ->
    search(Index, Geometry, []).


search(Index, Query, Opts) ->
    Arg = [
        get_req_srid(Opts),
        get_resp_srid(Opts),
        fmt_query(Query),
        get_filter(Opts),
        get_nearest(Opts),
        get_limit(Opts),
        get_include_geom(Opts)
    ] ++ get_bookmark(Opts),
    case cmd(Index, ?EASTON_COMMAND_SEARCH, Arg) of
        {ok, Results} ->
            {ok, lists:map(fun fmt_result/1, Results)};
        Else ->
            throw(Else)
    end.


init({Parent, Directory, Cmd, PortOpts}) ->
    erlang:monitor(process, Parent),
    try
        {ok, Port, OsPid} = open_index(Cmd, PortOpts),
        proc_lib:init_ack({ok, self()}),
        St = #st{
            parent = Parent,
            port = Port,
            os_pid = OsPid,
            killer = erlang:spawn(?MODULE, kill_monitor, [self(), OsPid]),
            idx_dir = Directory
        },
        gen_server:enter_loop(?MODULE, [], St)
    catch throw:Error ->
        proc_lib:init_ack(Error)
    end.


terminate(_Reason, St) ->
    exit(St#st.killer, kill),
    catch erlang:close_port(St#st.port),
    ok.


handle_call(os_pid, _From, St) ->
    {reply, St#st.os_pid, St};

handle_call(idx_dir, _From, St) ->
    {reply, St#st.idx_dir, St};

handle_call(close, _From, #st{closing = true} = St) ->
    {reply, ok, St};

handle_call(close, From, #st{closing = false} = St) ->
    Cmd = {cmd, t2b({?EASTON_COMMAND_CLOSE, true})},
    handle_call(Cmd, From, St#st{closing = true});

handle_call({cmd, C}, _From, #st{port = Port} = St) ->
    %io:format(standard_error, "~nPacket: ~p: ~p~n", [size(P), P]),
    true = erlang:port_command(St#st.port, C),
    receive
        {Port, {data, Resp}} ->
            {reply, Resp, St};
        Else ->
            throw({error, Else})
    after 5000 ->
            exit({timeout, C})
    end;

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', _, _, Pid, _}, #st{parent = Pid} = St) ->
    {stop, normal, St};

handle_info({Port, {exit_status, 0}}, #st{port = Port, closing = true} = St) ->
    {stop, normal, St};

handle_info({Port, {exit_status, N}}, #st{port = Port} = St) ->
    {stop, {index_exited, N}, St};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_Vsn, St, _Extra) ->
    {ok, St}.


cmd(Index, OpCode, Args) ->
    case gen_server:call(Index, {cmd, t2b({OpCode, Args})}, infinity) of
        BinResp when is_binary(BinResp) ->
            binary_to_term(BinResp);
        Else ->
            Else
    end.


kill_monitor(Pid, OsPid) ->
    erlang:monitor(process, Pid),
    kill_monitor(OsPid).


kill_monitor(OsPid) ->
    receive
        {'DOWN', _Ref, process, _Pid, _Reason} ->
            os:cmd(kill_cmd(OsPid))
    after 300000 ->
        kill_monitor(OsPid)
    end.


ensure_idx_dir(Directory) ->
    case filelib:is_dir(Directory) of
        false ->
            LibPath = filename:join([Directory, "foo"]),
            filelib:ensure_dir(LibPath);
        true ->
            ok
    end.


open_index(Cmd, PortOpts0) ->
    PortOpts = ?PORT_OPTS ++ PortOpts0,
    Port = erlang:open_port({spawn_executable, exe_name()}, PortOpts),
    receive
        {Port, {data, Bin}} ->
            case binary_to_term(Bin, [safe]) of
                {ok, OsPid} ->
                    init_index(Cmd, Port, OsPid);
                Else ->
                    throw(Else)
            end;
        Else ->
            throw(Else)
    after 1000 ->
        throw({timeout, open_index})
    end.


init_index(Cmd, Port, OsPid) ->
    C = term_to_binary(Cmd),
    true = erlang:port_command(Port, C),
    receive
        {Port, {data, Resp}} ->
            case binary_to_term(Resp) of
                ok ->
                    {ok, Port, OsPid};
                Else ->
                    throw({error, Else})
            end;
        Else ->
            throw({error, Else})
    after 5000 ->
            exit({timeout, Cmd})
    end.


exe_name() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    filename:join(PrivDir, ?EXE_NAME).


get_index_type(Opts) ->
    case lists:keyfind(type, 1, Opts) of
        {_, <<"rtree">>} -> ?EASTON_INDEX_TYPE_RTREE;
        {_, rtree} -> ?EASTON_INDEX_TYPE_RTREE;
        {_, <<"tprtree">>} -> ?EASTON_INDEX_TYPE_TPRTREE;
        {_, tprtree} -> ?EASTON_INDEX_TYPE_TPRTREE;
        {_, Else} -> throw({invalid_index_type, Else});
        false -> ?EASTON_INDEX_TYPE_RTREE
    end.


get_index_dimensions(Opts) ->
    case lists:keyfind(dimensions, 1, Opts) of
        {_, N} when N == 2 orelse N == 3 orelse N == 4 ->
            N;
        {_, Else} ->
            throw({invalid_index_dimensions, Else});
        false ->
            2
    end.


get_index_srid(Opts) ->
    case lists:keyfind(srid, 1, Opts) of
        {_, N} when is_integer(N), N > 0 ->
            {epsg, N};
        {_, <<"urn:ogc:def:crs:EPSG::", Tail/binary>> = Val} ->
            try
                SRID = integer_to_list(binary_to_list(Tail)),
                if SRID > 0 -> ok; true ->
                    throw({error, {invalid_epsg, Val}})
                end,
                {epsg, 4326}
            catch _:_ ->
                throw({error, {invalid_srid, Val}})
            end;
        {_, Else} ->
            throw({invalid_srid, Else});
        false ->
            {epsg, 4326}
    end.


get_cs_map_dir(Opts) ->
    case lists:keyfind(cs_map_dir, 1, Opts) of
        {_, Path0} ->
            Path = binary_to_list(iolist_to_binary(Path0)),
            case filelib:is_dir(Path) of
                true ->
                    Path;
                false ->
                    throw({invalid_cs_map_dir, Path0})
            end;
        false ->
            ?EASTON_DEFAULT_CS_MAP_DIR
    end.


fmt_query({_}=GeoJson) ->
    {wkb, easton_geojson:to_wkb(GeoJson)};
fmt_query(<<E:8/integer, _/binary>> = WKB) when E == 0 orelse E == 1 ->
    {wkb, WKB};
fmt_query(Bin) when is_binary(Bin) ->
    {wkt, Bin};
fmt_query(Coords) when is_list(Coords) ->
    {bbox, [float(C) || C <- Coords]};
fmt_query({X, Y, Radius}) ->
    {circle, {float(X), float(Y), float(Radius)}};
fmt_query({X, Y, XRange, YRange}) ->
    {ellipse, {float(X), float(Y), float(XRange), float(YRange)}};
fmt_query(Else) ->
    Else.


get_req_srid(Opts) ->
    case lists:keyfind(req_srid, 1, Opts) of
        {_, Value} ->
            get_index_srid([{srid, Value}]);
        false ->
            {epsg, 4326}
    end.


get_resp_srid(Opts) ->
    case lists:keyfind(resp_srid, 1, Opts) of
        {_, Value} ->
            get_index_srid([{srid, Value}]);
        false ->
            {epsg, 4326}
    end.


get_filter(Opts) ->
    case lists:keyfind(filter, 1, Opts) of
        {_, F} ->
            filter(F);
        false ->
            ?EASTON_FILTER_NONE
    end.


get_nearest(Opts) ->
    proplists:get_bool(nearest, Opts).


get_limit(Opts) ->
    case lists:keyfind(limit, 1, Opts) of
        {_, N} when is_integer(N), N > 0 ->
            N;
        {_, Else} ->
            throw({invalid_limit, Else});
        false ->
            200
    end.


get_include_geom(Opts) ->
    case proplists:get_bool(include_geom, Opts) of
        true ->
            true;
        false ->
            proplists:get_bool(include_geoms, Opts)
    end.


get_bookmark(Opts) ->
    case lists:keyfind(bookmark, 1, Opts) of
        {_, {DocId, Dist}} when is_binary(DocId), is_number(Dist) ->
            [{DocId, float(Dist)}];
        {_, undefined} ->
            [];
        {_, Else} ->
            throw({invalid_bookmark, Else});
        false ->
            []
    end.


fmt_result({DocId, Dist}) ->
    {DocId, Dist};
fmt_result({DocId, Dist, WKB}) ->
    {DocId, Dist, easton_geojson:from_wkb(WKB)}.


filter(none) ->
    ?EASTON_FILTER_NONE;
filter(undefined) ->
    ?EASTON_FILTER_NONE;
filter(<<"none">>) ->
    ?EASTON_FILTER_NONE;
filter(contains) ->
    ?EASTON_FILTER_CONTAINS;
filter(<<"contains">>) ->
    ?EASTON_FILTER_CONTAINS;
filter(contains_properly) ->
    ?EASTON_FILTER_CONTAINS_PROPERLY;
filter(<<"contains_properly">>) ->
    ?EASTON_FILTER_CONTAINS_PROPERLY;
filter(covered_by) ->
    ?EASTON_FILTER_COVERED_BY;
filter(<<"covered_by">>) ->
    ?EASTON_FILTER_COVERED_BY;
filter(covers) ->
    ?EASTON_FILTER_COVERS;
filter(<<"covers">>) ->
    ?EASTON_FILTER_COVERS;
filter(crosses) ->
    ?EASTON_FILTER_CROSSES;
filter(<<"crosses">>) ->
    ?EASTON_FILTER_CROSSES;
filter(disjoint) ->
    ?EASTON_FILTER_DISJOINT;
filter(<<"disjoint">>) ->
    ?EASTON_FILTER_DISJOINT;
filter(intersects) ->
    ?EASTON_FILTER_INTERSECTS;
filter(<<"intersects">>) ->
    ?EASTON_FILTER_INTERSECTS;
filter(overlaps) ->
    ?EASTON_FILTER_OVERLAPS;
filter(<<"overlaps">>) ->
    ?EASTON_FILTER_OVERLAPS;
filter(touches) ->
    ?EASTON_FILTER_TOUCHES;
filter(<<"touches">>) ->
    ?EASTON_FILTER_TOUCHES;
filter(within) ->
    ?EASTON_FILTER_WITHIN;
filter(<<"within">>) ->
    ?EASTON_FILTER_WITHIN;
filter(N) when N >= ?EASTON_FILTER_NONE, N =< ?EASTON_FILTER_MAX ->
    N.


kill_cmd(OsPid) ->
    Fmt = case os:type() of
        {unix, _} -> "kill -9 ~b";
        {win32, _} -> "taskkill /PID ~b"
    end,
    lists:flatten(io_lib:format(Fmt, [OsPid])).


get_disk_size(Idx) ->
    IdxDir = gen_server:call(Idx, idx_dir, infinity),
    Pattern0 = filename:join(IdxDir, "*"),
    Pattern = case Pattern0 of
        _ when is_binary(Pattern0) ->
            binary_to_list(Pattern0);
        _ ->
            Pattern0
    end,
    FileNames = filelib:wildcard(Pattern),
    lists:foldl(fun(FName, Acc) ->
        Acc + filelib:file_size(FName)
    end, 0, FileNames).


t2b(T) ->
    term_to_binary(T, [{minor_version, 1}]).
