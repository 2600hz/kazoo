%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Receive route(dialplan) requests from FS, request routes and respond
%%%
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_dialplan).

-export([init/0]).
-export([dialplan/1]).
-export([route_winner/1]).

-include("ecallmgr.hrl").


-define(ROUTE_WINNER_TIMEOUT, 60 * ?MILLISECONDS_IN_SECOND).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.dialplan.*.route_req.context_2">>, ?MODULE, 'dialplan'),
    kazoo_bindings:bind(<<"fetch.dialplan.*.route_req.default">>, ?MODULE, 'dialplan'),
    kazoo_bindings:bind(<<"event_stream.event.dialplan.ROUTE_WINNER">>, ?MODULE, 'route_winner'),
    'ok'.

-spec dialplan(map()) -> fs_sendmsg_ret().
dialplan(#{fetch_id := FetchId, payload := JObj}=Map) ->
    lager:debug_unsafe("ROUTE REQ : ~s", [kz_json:encode(JObj, ['pretty'])]),
    lager:debug("start dialplan fetch ~s for ~s", [FetchId, kzd_fetch:call_id(JObj)]),
    Routines = [fun call_id/1
               ,fun timeout/1
               ,{fun add_time_marker/2, start_processing}
               ,fun(M) -> M#{callback => fun process/1} end
               ,fun(M) -> M#{options => []} end
               ],
    ecallmgr_call_control_sup:start_proc(kz_maps:exec(Routines, Map)).

process(#{payload := JObj}=Map) ->
    kz_util:put_callid(JObj),
    Routines = [{fun add_time_marker/2, request_ready}
               ,fun request/1
               ,fun control_p/1
               ,fun timeout_reply/1
               ,fun maybe_authz/1
               ],
    maybe_expired(kz_maps:exec(Routines, Map)).

request(#{request := _Request}=Map) -> Map;
request(#{control_q := ControlQ, payload := JObj}=Map) ->
    Map#{request => kz_json:set_value(?KEY_SERVER_ID, ControlQ, JObj)}.

control_p(#{control_p := _Pid}=Map) -> Map;
control_p(#{request := Request}=Map) ->
    Map#{request => kz_json:set_value(?KEY_REQUEST_FROM_PID, kz_term:to_binary(self()), Request), control_p => self()}.

timeout(#{timeout := _Timeout}=Map) -> Map;
timeout(#{payload := JObj}=Map) ->
    T0 = kz_json:get_integer_value(<<"Fetch-Timestamp-Micro">>, JObj),
    T1 = kz_json:get_integer_value(<<"Fetch-Timeout">>, JObj),
    T3 = erlang:system_time('micro_seconds'),
    T4 = T3 - T0,
    T5 = T1 - T4,
    T6 = T5 div 1000,
    Map#{timeout => T6 - 750}.

call_id(#{call_id := _CallId}=Map) -> Map;
call_id(#{payload := JObj}=Map) ->
    Map#{call_id => kzd_fetch:call_id(JObj)}.

add_time_marker(Name, Map) ->
    add_time_marker(Name, kz_time:now_us(), Map).

add_time_marker(Name, Value, #{timer := Timer}= Map) ->
    Map#{timer => Timer#{Name => Value}};
add_time_marker(Name, Value, #{}= Map) ->
    add_time_marker(Name, Value, Map#{timer => #{}}).

maybe_authz(#{authz_worker := _Authz}=Map) -> Map;
maybe_authz(#{}=Map) ->
    case kapps_config:is_true(?APP_NAME, <<"authz_enabled">>, 'false') of
        true -> spawn_authorize_call_fun(Map);
        false -> Map
    end.

maybe_expired(#{timeout := Timeout}=Map)
  when Timeout =< 0 ->
    lager:warning("timeout before sending route request : ~B", [Timeout]),
    send_reply(Map);
maybe_expired(#{request := Request}=Map) ->
    kapi_route:publish_req(Request),
    wait_for_route_resp(add_time_marker(request_sent, Map)).

wait_for_route_resp(#{timeout := Timeout}=Map) ->
    lager:debug("waiting ~B ms for route response", [Timeout]),
    Now = kz_time:now_ms(),
    receive
        {'route_resp', Resp, Props} ->
            case kz_api:defer_response(Resp) of
                true ->
                    NewTimeout = Timeout - kz_time:elapsed_ms(Now),
                    lager:debug("received deferred reply - waiting for others for ~B ms", [NewTimeout]),
                    wait_for_route_resp(Map#{timeout => NewTimeout, reply => #{payload => Resp, props => Props}});
                false ->
                    lager:info("received route reply"),
                    NewTimeout = Timeout - kz_time:elapsed_ms(Now),
                    maybe_wait_for_authz(Map#{reply => #{payload => Resp, props => Props}, authz_timeout => NewTimeout})
            end
    after Timeout ->
            lager:warning("timeout after ~B receiving route response", [Timeout]),
            send_reply(Map)
    end.

spawn_authorize_call_fun(#{node := Node, call_id := CallId, payload := JObj}=Map) ->
    Ref = make_ref(),
    Pid = kz_util:spawn(fun authorize_call_fun/5, [self(), Ref, Node, CallId, JObj]),
    Map#{authz_worker => {Pid, Ref}}.

authorize_call_fun(Parent, Ref, Node, CallId, JObj) ->
    kz_util:put_callid(CallId),
    Parent ! {'authorize_reply', Ref, ecallmgr_fs_authz:authorize(JObj, CallId, Node)}.

maybe_wait_for_authz(#{authz_worker := _AuthzWorker, reply := #{payload := Reply}}=Map) ->
    case kz_json:get_value(<<"Method">>, Reply) =/= <<"error">> of
        'true' -> wait_for_authz(Map);
        'false' -> send_reply(Map)
    end;
maybe_wait_for_authz(#{}=Map) ->
    send_reply(Map).

wait_for_authz(#{authz_worker := {Pid, Ref}, authz_timeout := Timeout, reply := #{payload := JObj}=Reply}=Map) ->
    lager:info("waiting for authz reply from worker ~p", [Pid]),
    receive
        {'authorize_reply', Ref, 'false'} -> send_reply(forbidden_reply(Map));
        {'authorize_reply', Ref, 'true'} -> send_reply(Map);
        {'authorize_reply', Ref, {'true', AuthzCCVs}} ->
            CCVs = kz_json:get_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
            J = kz_json:set_value(<<"Custom-Channel-Vars">>
                                 ,kz_json:merge_jobjs(CCVs, AuthzCCVs)
                                 ,JObj
                                 ),
            send_reply(Map#{reply => Reply#{payload => J}})
    after Timeout ->
            lager:warning("timeout waiting for authz reply from worker ~p", [Pid])
    end.

send_reply(#{node := Node, fetch_id := FetchId, reply := #{payload := Reply}}=Ctx) ->
    Props = maps:to_list(Ctx),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml('dialplan', Reply, Props),
    lager:debug("sending xml dialplan reply for request ~s tp ~s",[FetchId, Node]),
    freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(XML)}),
    case kz_api:defer_response(Reply)
        orelse kz_json:get_ne_binary_value(<<"Method">>, Reply) /= <<"park">>
    of
        true -> ok;
        false -> wait_for_route_winner(Ctx)
    end.

wait_for_route_winner(Ctx) ->
    receive
        {'route_winner', JObj, Props} ->
            activate_call_control(Ctx#{winner => #{payload => JObj, props => Props}})
    after ?ROUTE_WINNER_TIMEOUT ->
            lager:warning("timeout after ~B receiving route winner", [?ROUTE_WINNER_TIMEOUT])
    end.

-spec activate_call_control(map()) -> {'ok', map()}.
activate_call_control(#{call_id := CallId, winner := #{payload := JObj}} = Map) ->
    kz_util:put_callid(CallId),
    CCVs = kzd_fetch:ccvs(JObj),
    ControllerQ = kzd_fetch:controller_queue(JObj),
    ControllerP = kzd_fetch:controller_pid(JObj),
    ecallmgr_fs_channels:deferred_update(CallId, #channel.handling_locally, 'true'),
    Args = Map#{controller_q => ControllerQ
               ,controller_p => ControllerP
               ,initial_ccvs => CCVs
               },
    {'ok', Args}.

error_message() ->
    error_message(<<"no available handlers">>).

error_message(ErrorMsg) ->
    error_message(<<"604">>, ErrorMsg).

error_message(ErrorCode, ErrorMsg) ->
    kz_json:from_list([{<<"Method">>, <<"error">>}
                      ,{<<"Route-Error-Code">>, ErrorCode}
                      ,{<<"Route-Error-Message">>, ErrorMsg}
                      ]).

timeout_reply(Map) ->
    Map#{reply => #{payload => error_message(), props => []}}.


-spec forbidden_reply(map()) -> map().
forbidden_reply(#{fetch_id := FetchId}=Map) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    Map#{reply => #{payload => error_message(<<"403">>, <<"Incoming call barred">>), props => []}}.

-spec route_winner(map()) -> any().
route_winner(#{payload := JObj}) ->
    NodeWinner = kzd_fetch:ccv(JObj, <<"Ecallmgr-Node">>),
    case NodeWinner =:= kz_term:to_binary(node()) of
        true ->
            Pid = kz_term:to_pid(kz_api:reply_to(JObj)),
            Pid ! {'route_winner', JObj, []};
        false ->
            lager:debug("route winner handled by other node : ~s", [NodeWinner])
    end.
