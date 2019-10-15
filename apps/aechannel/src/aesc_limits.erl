-module(aesc_limits).

-behavior(gen_server).

-export([ allow_new/0
        , register_returning/0 ]).

-export([ start_link/0
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(st, { }).

-define(PIDS, aesc_limits_pids).

allow_new() ->
    gen_server:call(?MODULE, allow_new).

register_returning() ->
    gen_server:call(?MODULE, register_returning).

start_link() ->
    ets:new(?PIDS, [set, public, named_table]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #st{}}.


handle_call(register_returning, {Pid, _}, S) ->
    MRef = monitor(process, Pid),
    case ets:insert_new(?PIDS, {Pid, MRef}) of
        true ->
            lager:debug("Returning session (~p) allowed", [Pid]),
            {reply, ok, S};
        false ->
            lager:debug("Returning session (~p) denied: already exists", [Pid]),
            demonitor(MRef),
            {reply, {error, exists}, S}
    end;
handle_call(allow_new, {Pid,_}, S) ->
    Limit = get_limit(),
    case ets:info(?PIDS, size) of
        Sz when Sz >= Limit ->
            lager:debug("New session (~p) denied; Sz = ~p, Limit = ~p",
                        [Pid, Sz, Limit]),
            {reply, {error, channel_count_limit_exceeded}, S};
        Sz ->
            MRef = monitor(process, Pid),
            case ets:insert_new(?PIDS, {Pid, MRef}) of
                true ->
                    lager:debug("New session (~p) allowed; Size = ~p, Limit = ~p",
                                [Pid, Sz, Limit]),
                    {reply, ok, S};
                false ->
                    lager:debug("New session (~p) denied: already exists", [Pid]),
                    demonitor(MRef),
                    {reply, {error, exists}, S}
            end
    end;
handle_call(_, _, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', MRef, process, Pid, _Reason}, S) ->
    NDeleted = ets:select_delete(?PIDS, [{ {Pid, MRef}, [], [true] }]),
    lager:debug("'DOWN' received; ~p entries deleted for ~p", [NDeleted, Pid]),
    {noreply, S};
handle_info(_, S) ->
    {noreply, S}.

terminate(_, _) ->
    ok.

code_change(_FromVsn, S, _Extra) ->
    {ok, S}.


get_limit() ->
    {ok, Max} = aeu_env:find_config([ <<"channels">>, <<"max_count">> ] , [ user_config
                                                                          , schema_default
                                                                          , {value, 1000} ]),
    Max.
