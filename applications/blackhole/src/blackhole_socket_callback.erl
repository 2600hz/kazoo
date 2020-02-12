%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_socket_callback).

-include("blackhole.hrl").

-export([open/3
        ,recv/2
        ,close/1

        ,active_sessions/0, active_sessions_count/0
        ,active_sessions/1, active_sessions_count/1
        ]).

-define(SESSION_KEY(SessionId), {'session', ?MODULE, SessionId}).

-type cb_return() :: {'ok', bh_context:context()}.

-record(cache_info
       ,{start_time_s = kz_time:now_s() :: kz_time:gregorian_seconds()
        ,socket_pid :: pid()
        ,ip :: kz_term:ne_binary()
        }
       ).

-spec open(pid(), binary(), inet:ip_address()) -> cb_return().
open(Pid, SessionId, IPAddr) ->
    IPBin = kz_network_utils:iptuple_to_binary(IPAddr),
    lager:debug("opening socket (~p) ~p, peer: ~p", [Pid, SessionId, IPBin]),

    Context = bh_context:set_source(bh_context:new(Pid, SessionId), IPBin),

    kz_cache:store_local(?CACHE_NAME
                        ,?SESSION_KEY(SessionId)
                        ,#cache_info{'start_time_s' = kz_time:now_s()
                                    ,'socket_pid' = self()
                                    ,'ip' = IPBin
                                    }
                        ,[{'expires', ?SECONDS_IN_HOUR}]
                        ),

    Routing = <<"blackhole.session.open">>,
    Ctx = blackhole_bindings:fold(Routing, Context),
    {'ok', Ctx}.

-spec recv({binary(), kz_json:object()}, bh_context:context()) -> cb_return() | 'error'.
recv({Action, Payload}, Context) ->
    lager:debug("received ~s with payload ~s",[Action, kz_json:encode(kz_json:delete_key(<<"auth_token">>, Payload))]),
    Routines = [fun rate/3
               ,fun authenticate/3
               ,fun validate/3
               ,fun authorize/3
               ,fun limits/3
               ,fun command/3
               ,fun finish/3
               ],
    Ctx = bh_context:from_json(Context, Payload),
    exec(Ctx, Action, Payload, Routines).

exec(Context, _Action, _Payload, []) ->
    {'ok', Context};
exec(Context, Action, Payload, [Fun | Funs]) ->
    lager:debug("executing ~p for ~s with payload ~s"
               ,[Fun, Action, kz_json:encode(kz_json:delete_key(<<"auth_token">>, Payload))]
               ),
    Ctx = Fun(Context, Action, Payload),
    case bh_context:success(Ctx) of
        'true' -> exec(Ctx, Action, Payload, Funs);
        'false' -> send_error(Ctx)
    end.

send_error(#bh_context{websocket_pid=SessionPid
                      ,req_id=RequestId
                      ,errors=Errors
                      }) ->
    Data = kz_json:from_list([{<<"errors">>, Errors}]),
    blackhole_data_emitter:reply(SessionPid, RequestId, <<"error">>, Data),
    'error'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec close(bh_context:context()) -> bh_context:context().
close(HandlerOpts) when is_list(HandlerOpts) ->
    lager:info("closing conn: handler opts: ~p", [HandlerOpts]);
close(Context) ->
    kz_cache:erase_local(?CACHE_NAME, ?SESSION_KEY(bh_context:websocket_session_id(Context))),
    Routing = <<"blackhole.session.close">>,
    blackhole_bindings:fold(Routing, Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
rate(Context, _Action, _Payload) ->
    Bucket = bh_context:websocket_session_id(Context),
    case kz_buckets:consume_token(?APP_NAME, Bucket) of
        'true' -> Context;
        'false' ->
            Msg = io_lib:format("rate limiting threshold hit for ~s!", [Bucket]),
            lager:warning(Msg),
            bh_context:add_error(Context, Msg)
    end.

authenticate(Context, Action, Payload) ->
    case bh_context:is_authenticated(Context) of
        'true' -> Context;
        'false' ->
            Routing = <<"blackhole.authenticate.", Action/binary>>,
            handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload]))
    end.

validate(Context, Action, Payload) ->
    Routing = <<"blackhole.validate.", Action/binary>>,
    handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])).

authorize(Context, Action, Payload) ->
    Routing = <<"blackhole.authorize.", Action/binary>>,
    handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])).


limits(Context, Action, Payload) ->
    Routing = <<"blackhole.limits.", Action/binary>>,
    handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])).

command(Context, Action, Payload) ->
    Routing = <<"blackhole.command.", Action/binary>>,
    Ctx = handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])),
    case bh_context:success(Ctx) of
        'true' ->
            SessionPid = bh_context:websocket_pid(Ctx),
            RequestId = bh_context:req_id(Ctx),
            Data = bh_context:resp_data(Ctx),
            blackhole_data_emitter:reply(SessionPid, RequestId, <<"success">>, Data),
            Ctx;
        'false' -> Ctx
    end.

finish(Context, Action, _Payload) ->
    Routing = <<"blackhole.finish.", Action/binary>>,
    blackhole_bindings:fold(Routing, Context).

handle_result(Context, []) -> Context;
handle_result(Context, Res) ->
    case blackhole_bindings:failed(Res) of
        [Ctx | _] -> Ctx;
        _ -> handle_success(Context, Res)
    end.

handle_success(Context, Res) ->
    case blackhole_bindings:succeeded(Res) of
        [Ctx | _] -> Ctx;
        _ -> Context
    end.

-type active_sessions() :: [{?SESSION_KEY(kz_term:ne_binary()), pos_integer()}].

-spec active_sessions() -> active_sessions().
active_sessions() ->
    kz_cache:filter_local(?CACHE_NAME, fun is_active_session/2).

-spec active_sessions_count() -> non_neg_integer().
active_sessions_count() ->
    kz_cache:count_local(?CACHE_NAME, ?SESSION_KEY('_')).

-spec active_sessions(kz_term:ne_binary() | inet:ip_address()) -> active_sessions().
active_sessions(<<PeerIP/binary>>) ->
    kz_cache:filter_local(?CACHE_NAME, fun(K, V) -> is_active_peer(K, V, PeerIP) end);
active_sessions(PeerIP) ->
    active_sessions(kz_network_utils:iptuple_to_binary(PeerIP)).

-spec active_sessions_count(kz_term:ne_binary() | inet:ip_address()) -> non_neg_integer().
active_sessions_count(<<PeerIP/binary>>) ->
    CacheKey = ?SESSION_KEY('_'),
    Conditions = #cache_info{ip=PeerIP, _='_'},
    kz_cache:count_local(?CACHE_NAME, CacheKey, Conditions);
active_sessions_count(PeerIP) ->
    active_sessions_count(kz_network_utils:iptuple_to_binary(PeerIP)).

-spec is_active_session(any(), any()) -> boolean().
is_active_session(?SESSION_KEY(_SessionId), #cache_info{socket_pid=Pid}) -> is_process_alive(Pid);
is_active_session(_K, _V) -> 'false'.

-spec is_active_peer(any(), any(), kz_term:ne_binary()) -> boolean().
is_active_peer(?SESSION_KEY(SessionId), #cache_info{socket_pid=Pid}, PeerIP) ->
    case is_process_alive(Pid) of
        'false' -> 'false';
        'true' ->
            Size = byte_size(PeerIP),
            case SessionId of
                <<PeerIP:Size/binary, _Rest/binary>> -> 'true';
                _Id -> 'false'
            end
    end;
is_active_peer(_K, _V, _PeerIP) -> 'false'.
