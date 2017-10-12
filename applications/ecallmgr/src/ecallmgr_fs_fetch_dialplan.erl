%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Receive route(dialplan) requests from FS, request routes and respond
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_dialplan).

-export([init/0]).
-export([dialplan/1]).
-export([route_winner/1]).

-include("ecallmgr.hrl").


-define(ROUTE_WINNER_TIMEOUT, 60 * ?MILLISECONDS_IN_SECOND).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.dialplan.context_2">>, ?MODULE, 'dialplan'),
    kazoo_bindings:bind(<<"event_stream.event.dialplan.ROUTE_WINNER">>, ?MODULE, 'route_winner'),
    'ok'.

-spec dialplan(map()) -> fs_sendmsg_ret().
dialplan(#{}=Map) ->
    Routines = [fun call_id/1
               ,fun timeout/1
               ,{fun add_time_marker/2, start_processing}
               ,fun(M) -> M#{callback => fun process/1} end
               ],
    ecallmgr_call_control_sup:start_proc(kz_maps:exec(Routines, Map)).

process(#{}=Map) ->
    Routines = [{fun add_time_marker/2, request_ready}
               ,fun request/1
               ,fun control_p/1
               ,fun timeout_reply/1
               ,fun maybe_authz/1
               ],
    maybe_expired(kz_maps:exec(Routines, Map)).

request(#{request := _Request}=Map) -> Map;
request(#{node := Node, fetch_id := FetchId, call_id := UUID, control_q := ControlQ, jobj := JObj}=Map) ->
    Map#{request => ecallmgr_fs_router_util:route_req(ControlQ, UUID, FetchId, kz_json:to_proplist(JObj), Node)}.

control_p(#{control_p := _Pid}=Map) -> Map;
control_p(#{request := Request}=Map) ->
    Map#{request => [{<<"Request-From-PID">>, kz_term:to_binary(self())} | Request], control_p => self()}.
    
timeout(#{timeout := _Timeout}=Map) -> Map;
timeout(#{jobj := JObj}=Map) ->
    T0 = kz_json:get_integer_value(<<"Fetch-Timestamp-Micro">>, JObj),
    T1 = kz_json:get_integer_value(<<"Fetch-Timeout">>, JObj),
    T3 = kz_time:now_us(),
    T4 = T3 - T0,
    T5 = T1 - T4,
    T6 = T5 div 1000,
    Map#{timeout => T6 - 100}.
              
call_id(#{call_id := _CallId}=Map) -> Map;
call_id(#{jobj := JObj}=Map) ->
    Map#{call_id => kzd_fetch:call_id(JObj)}.

add_time_marker(Name, #{timer := Timer}= Map) ->
    Map#{timer => Timer#{Name => kz_time:now_us()}};
add_time_marker(Name, #{}= Map) ->
    add_time_marker(Name, Map#{timer => #{}}).

%% add_time_marker(Name, Value, #{timer := Timer}= Map) ->
%%     Map#{timer => Timer#{Name => Value}};
%% add_time_marker(Name, Value, #{}= Map) ->
%%     add_time_marker(Name, Value, Map#{timer => #{}}).

maybe_authz(#{authz_worker := _Authz}=Map) -> Map;
maybe_authz(#{}=Map) ->
    case ecallmgr_config:is_true(<<"authz_enabled">>, 'false') of
        true -> spawn_authorize_call_fun(Map);
        false -> Map
    end.

maybe_expired(#{timeout := Timeout}=Map)
  when Timeout =< 0 ->
    lager:warning("timeout before sending route request"),
    send_reply(Map);
maybe_expired(#{request := Request}=Map) ->
    kapi_route:publish_req(Request),
    wait_for_route_resp(add_time_marker(request_sent, Map)).

wait_for_route_resp(#{timeout := Timeout}=Map) ->
    Now = kz_time:current_tstamp(),
    receive
        {'route_resp', Resp, Props} ->
            case kz_api:defer_response(Resp) of
                true ->
                    lager:notice("received deferred reply"),
                    NewTimeout = Timeout - kz_time:elapsed_ms(Now),
                    wait_for_route_resp(Map#{timeout => NewTimeout, reply => #{payload => Resp, props => Props}});
                false ->
                    lager:info("received route reply"),
                    maybe_wait_for_authz(Map#{reply => #{payload => Resp, props => Props}})
            end
    after Timeout ->
            lager:warning("timeout after ~B receiving route response", [Timeout]),
            send_reply(Map)
    end.

spawn_authorize_call_fun(#{node := Node, call_id := CallId, jobj := JObj}=Map) ->
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

wait_for_authz(#{authz_worker := {Pid, Ref}, reply := #{payload := JObj}=Reply}=Map) ->
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
    after 5000 ->
            lager:warning("timeout waiting for authz reply from worker ~p", [Pid])
    end.

send_reply(#{node := Node, fetch_id := FetchId, reply := #{payload := Reply}}=Map) ->
    Props = maps:to_list(Map),
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml('dialplan', Reply, Props),
    lager:debug("sending xml dialplan reply for request ~s",[FetchId]),
    freeswitch:fetch_reply(Node, FetchId, 'dialplan', iolist_to_binary(XML)),
    case kz_api:defer_response(Reply)
        orelse kz_json:get_ne_binary_value(<<"Method">>, Reply) /= <<"park">>
    of
        true -> ok;
        false -> wait_for_route_win(Map)
    end.

wait_for_route_win(Map) ->
    receive
        {'route_winner', JObj, Props} ->
            start_call_control(Map#{winner => #{payload => JObj, props => Props}})
    after ?ROUTE_WINNER_TIMEOUT ->
            lager:warning("timeout after ~B receiving route winner", [?ROUTE_WINNER_TIMEOUT])
    end.

-spec start_call_control(map()) -> 'ok'.
start_call_control(#{call_id := CallId, winner := #{payload := JObj}} = Map) ->
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

%% error_message() ->
%%     error_message(<<"not enough resources">>).

error_message(ErrorMsg) ->
    error_message(<<"604">>, ErrorMsg).

error_message(ErrorCode, ErrorMsg) ->
    kz_json:from_list([{<<"Method">>, <<"error">>}
              ,{<<"Route-Error-Code">>, ErrorCode}
              ,{<<"Route-Error-Message">>, ErrorMsg}
              ]).

timeout_reply(Map) ->
    Map#{reply => #{payload => error_message(<<"no available handlers">>), props => []}}.


-spec forbidden_reply(map()) -> map().
forbidden_reply(#{fetch_id := FetchId}=Map) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    Map#{reply => #{payload => error_message(<<"403">>, <<"Incoming call barred">>), props => []}}.

-spec route_winner(map()) -> fs_sendmsg_ret().
route_winner({_Node, _UUID, _Category, _Event, JObj}) ->
    NodeWinner = kzd_fetch:ccv(JObj, <<"Ecallmgr-Node">>),
    case NodeWinner =:= kz_term:to_binary(node()) of
        true ->
            Pid = kz_term:to_pid(kz_api:reply_to(JObj)),
            Pid ! {'route_winner', JObj, []};
        false ->
            lager:debug("route winner handled by other node : ~s", [NodeWinner])
    end.
