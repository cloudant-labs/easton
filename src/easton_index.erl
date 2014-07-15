-module(easton_index).
-behavior(gen_server).

-export([
    open/1,
    open/2,
    close/1,
    flush/1,

    put/3,
    get/2,
    get/3,
    del/2,

    doc_id_num/1,
    doc_count/1,

    update/3,
    delete/2

    % search/3,
    % search/4
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
    port,
    killer,
    closing = false
}).


-define(EXE_NAME, "easton_index").
-define(PORT_OPTS, [{packet, 4}, binary, exit_status, nouse_stdio, hide]).


open(Directory) ->
    open(Directory, []).


open(Directory, Opts) ->
    ensure_idx_dir(Directory),

    IndexType = get_index_type(Opts),
    Dimensions = get_index_dimensions(Opts),
    Limit = get_limit(Opts),
    Args = {args, [Directory, IndexType, Dimensions, Limit]},

    CsMapDir = get_cs_map_dir(Opts),
    Env = {env, [{"EASTON_CS_MAP_DIR", CsMapDir}]},

    gen_server:start_link(?MODULE, [Args, Env], []).


close(Index) ->
    Ref = erlang:monitor(process, Index),
    case gen_server:call(Index, close) of
        {ok, <<>>} ->
            receive
                {'DOWN', Ref, process, Index, _} ->
                    ok
            after 5000 ->
                erlang:demonitor(Ref, [flush]),
                throw({timeout, close})
            end;
        Else ->
            erlang:demonitor(Ref, [flush]),
            throw({bad_close, Else})
    end.


flush(Index) ->
    case cmd(Index, ?EASTON_COMMAND_FLUSH, <<>>) of
        {ok, <<>>} ->
            ok;
        Else ->
            throw({bad_flush, Else})
    end.


doc_id_num(Index) ->
    case cmd(Index, ?EASTON_COMMAND_GET_DOC_ID_NUM, <<>>) of
        {ok, <<DocIdNum:64/integer>>} ->
            {ok, DocIdNum};
        Else ->
            throw({bad_doc_id_num, Else})
    end.


doc_count(Index) ->
    case cmd(Index, ?EASTON_COMMAND_GET_DOC_COUNT, <<>>) of
        {ok, <<DocCount:64/integer>>} ->
            {ok, DocCount};
        Else ->
            throw({bad_doc_count, Else})
    end.


put(Index, Key, Value) ->
    Payload = to_payload([ukey(Key), t2b(Value)]),
    case cmd(Index, ?EASTON_COMMAND_PUT_USER_KV, Payload) of
        {ok, <<>>} ->
            ok;
        Else ->
            throw({bad_put, Else})
    end.


get(Index, Key) ->
    Payload = to_payload([ukey(Key)]),
    case cmd(Index, ?EASTON_COMMAND_GET_USER_KV, Payload) of
        {ok, VBin} ->
            {Key, binary_to_term(VBin, [safe])};
        {error, <<>>} ->
            false;
        Else ->
            throw({bad_get, Else})
    end.


get(Index, Key, Default) ->
    case ?MODULE:get(Index, Key) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.


del(Index, Key) ->
    Payload = to_payload([ukey(Key)]),
    case cmd(Index, ?EASTON_COMMAND_DEL_USER_KV, Payload) of
        {ok, <<>>} ->
            true;
        {error, <<>>} ->
            false;
        Else ->
            throw({bad_del, Else})
    end.


update(Index, DocId, Geometries) ->
    WKBs = [easton_geojson:to_wkb(G) || G <- Geometries],
    Payload = to_payload([DocId, length(WKBs)] ++ WKBs),
    case cmd(Index, ?EASTON_COMMAND_UPDATE_ENTRIES, Payload) of
        {ok, <<>>} ->
            ok;
        Else ->
            throw({bad_update, Else})
    end.


delete(Index, DocId) ->
    Payload = to_payload([DocId]),
    case cmd(Index, ?EASTON_COMMAND_DELETE_ENTRIES, Payload) of
        {ok, <<>>} ->
            ok;
        Else ->
            throw({bad_delete, Else})
    end.


init(Opts) ->
    {ok, Port, OsPid} = open_index(Opts),
    {ok, #st{
        port = Port,
        killer = erlang:spawn(?MODULE, kill_monitor, [self(), OsPid])
    }}.


terminate(_Reason, St) ->
    exit(St#st.killer, kill),
    catch erlang:close_port(St#st.port),
    ok.


handle_call(close, _From, #st{closing = true} = St) ->
    {reply, ok, St};

handle_call(close, From, #st{closing = false} = St) ->
    Packet = <<?EASTON_COMMAND_CLOSE:16/integer>>,
    handle_call({packet, Packet}, From, St#st{closing = true});

handle_call({packet, P}, _From, #st{port = Port} = St) ->
    %io:format(standard_error, "~nPacket: ~p: ~p~n", [size(P), P]),
    true = erlang:port_command(St#st.port, P),
    receive
        {Port, {data, <<0:8/integer, Bin/binary>>}} ->
            {reply, {ok, Bin}, St};
        {Port, {data, <<1:8/integer, Bin/binary>>}} ->
            {reply, {error, Bin}, St};
        Else ->
            throw({error, Else})
    after 5000 ->
            exit({timeout, P})
    end;

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({Port, {exit_status, 0}}, #st{port = Port, closing = true} = St) ->
    {stop, normal, St};

handle_info({Port, {exit_status, N}}, #st{port = Port} = St) ->
    {stop, {index_exited, N}, St};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_Vsn, St, _Extra) ->
    {ok, St}.


cmd(Index, OpCode, Payload) ->
    Packet = <<OpCode:16/integer, Payload/binary>>,
    gen_server:call(Index, {packet, Packet}, infinity).


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


open_index(Opts) ->
    PortOpts = ?PORT_OPTS ++ Opts,
    Port = erlang:open_port({spawn_executable, exe_name()}, PortOpts),
    receive
        {Port, {data, <<OsPid:32/big-unsigned-integer>>}} ->
            {ok, Port, OsPid};
        Else ->
            throw({bad_os_pid, Else})
    after 1000 ->
        throw({timeout, index_open})
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
    Type = case lists:keyfind(index_type, 1, Opts) of
        {_, <<"rtree">>} -> ?EASTON_INDEX_TYPE_RTREE;
        {_, rtree} -> ?EASTON_INDEX_TYPE_RTREE;
        {_, <<"tprtree">>} -> ?EASTON_INDEX_TYPE_TPRTREE;
        {_, tprtree} -> ?EASTON_INDEX_TYPE_TPRTREE;
        {_, Else} -> throw({invalid_index_type, Else});
        false -> ?EASTON_INDEX_TYPE_RTREE
    end,
    integer_to_list(Type).


get_index_dimensions(Opts) ->
    case lists:keyfind(dimensions, 1, Opts) of
        {_, N} when is_integer(N), N > 0 ->
            integer_to_list(N);
        {_, Else} ->
            throw({invalid_index_dimensions, Else});
        false ->
            "2"
    end.


get_limit(Opts) ->
    case lists:keyfind(limit, 1, Opts) of
        {_, N} when is_integer(N), N > 0 ->
            integer_to_list(N);
        {_, Else} ->
            throw({invalid_index_limit, Else});
        false ->
            "200"
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


kill_cmd(OsPid) ->
    Fmt = case os:type() of
        {unix, _} -> "kill -9 ~b";
        {win32, _} -> "taskkill /PID ~b"
    end,
    lists:flatten(io_lib:format(Fmt, [OsPid])).


ukey(Term) ->
    Bin = t2b(Term),
    Len = size(Bin),
    <<Len:32/big-unsigned-integer, Bin/binary>>.


to_payload(Parts) ->
    to_payload(Parts, []).


to_payload([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_payload([Part | Rest], Acc) ->
    Bin = part_to_payload(Part),
    to_payload(Rest, [Bin | Acc]).


part_to_payload(Bin) when is_binary(Bin) ->
    Size = size(Bin),
    <<Size:32/big-unsigned-integer, Bin/binary>>;
part_to_payload(Int) when is_integer(Int), Int >= 0 ->
    <<Int:32/big-unsigned-integer>>.


t2b(T) ->
    term_to_binary(T, [{minor_version, 1}]).
