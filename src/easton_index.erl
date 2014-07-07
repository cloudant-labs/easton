-module(easton_index).
-behavior(gen_server).

-export([
    open/1,
    open/2,
    close/1,
    flush/1

    % insert/3,
    % delete/2,
    %
    % doc_count/1,
    %
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
    Packet = <<?EASTON_COMMAND_CLOSE:32/integer>>,
    handle_call({packet, Packet}, From, St#st{closing = true});

handle_call({packet, P}, _From, #st{port = Port} = St) ->
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
    Packet = <<OpCode:32/integer, Payload/binary>>,
    gen_server:call(Index, {packet, Packet}).


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


