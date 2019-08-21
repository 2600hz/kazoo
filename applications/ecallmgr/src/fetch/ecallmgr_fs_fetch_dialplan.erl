%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Receive route(dialplan) requests from FS, request routes and respond
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    _ = kazoo_bindings:bind(<<"fetch.dialplan.*.route_req.*">>, ?MODULE, 'dialplan'),
    _ = kazoo_bindings:bind(<<"event_stream.event.dialplan.ROUTE_WINNER">>, ?MODULE, 'route_winner'),
    'ok'.

-spec dialplan(dialplan_context()) -> {'ok', dialplan_context()}.
dialplan(#{fetch_id := FetchId, payload := FetchJObj}=Map) ->
    lager:debug("start dialplan fetch ~s for ~s", [FetchId, kzd_fetch:call_id(FetchJObj)]),
    %% lager:debug_unsafe("dialplan request => ~s", [kz_json:encode(JObj, ['pretty'])]),
    case ecallmgr_call_sup:control_context() of
        {'ok', Context} -> run(maps:merge(Map, Context));
        _ -> send_reply(timeout_reply(Map))
    end.

-spec run(dialplan_context()) -> {'ok', dialplan_context()}.
run(#{}=Map) ->
    Routines = [fun call_id/1
               ,fun timeout/1
               ,{fun add_time_marker/2, 'start_processing'}
               ,fun(M) -> M#{callback => fun process/1} end
               ,fun(M) -> M#{options => []} end
               ,fun(M) -> M#{start_result => ecallmgr_call_control_sup:start_proc(M)} end
               ],
    Execed = kz_maps:exec(Routines, Map),
    wait_for_exit(Execed),
    {'ok', Execed}.

wait_for_exit(#{start_result := Result}=M) ->
    ecallmgr_call_sup:wait_for_exit(Result, M).

-spec process(dialplan_context()) -> {'ok', dialplan_context()}.
process(#{payload := FetchJObj}=Map) ->
    kz_util:put_callid(FetchJObj),
    Routines = [{fun add_time_marker/2, 'request_ready'}
               ,fun control_p/1
               ,fun request/1
               ,fun timeout_reply/1
               ,fun maybe_authz/1
               ],
    maybe_expired(kz_maps:exec(Routines, Map)).

request(#{request := _Request}=Map) -> Map;
request(#{control_q := ControlQ, control_p := ControlP, payload := FetchJObj}=Map) ->
    Map#{request => kz_json:set_value(?KEY_SERVER_ID, kapi:encode_pid(ControlQ, ControlP), FetchJObj)}.

control_p(#{control_p := _Pid}=Map) -> Map;
control_p(Map) ->
    Map#{control_p => self()}.

timeout(#{timeout := _Timeout}=Map) -> Map;
timeout(#{payload := FetchJObj}=Map) ->
    NowUs = erlang:system_time('micro_seconds'),
    T0 = kzd_fetch:fetch_timestamp_micro(FetchJObj, NowUs),
    T1 = kzd_fetch:fetch_timeout(FetchJObj, 3500000),
    T4 = NowUs - T0,
    T5 = T1 - T4,
    T6 = T5 div 1000,
    Map#{timeout => T6 - 750}.

call_id(#{call_id := _CallId}=Map) -> Map;
call_id(#{payload := JObj}=Map) ->
    Map#{call_id => kzd_fetch:call_id(JObj)}.

-spec add_time_marker(atom(), dialplan_context()) -> dialplan_context().
add_time_marker(Name, Map) ->
    add_time_marker(Name, kz_time:now_us(), Map).

-spec add_time_marker(atom(), pos_integer(), dialplan_context()) -> dialplan_context().
add_time_marker(Name, Value, #{timer := Timer}= Map) ->
    Map#{timer => Timer#{Name => Value}};
add_time_marker(Name, Value, #{}= Map) ->
    add_time_marker(Name, Value, Map#{timer => #{}}).

-spec maybe_authz(dialplan_context()) -> dialplan_context().
maybe_authz(#{authz_worker := _Authz}=Map) -> Map;
maybe_authz(#{}=Map) ->
    case kapps_config:is_true(?APP_NAME, <<"authz_enabled">>, 'false') of
        'true' -> spawn_authorize_call_fun(Map);
        'false' -> Map
    end.

-spec maybe_expired(dialplan_context()) -> {'ok', dialplan_context()}.
maybe_expired(#{timeout := Timeout}=Map)
  when Timeout =< 0 ->
    maybe_expired(Map#{timeout => 5 * ?MILLISECONDS_IN_SECOND});
%%     lager:warning("timeout before sending route request : ~B", [Timeout]),
%%     send_reply(Map);
maybe_expired(#{request := Request}=Map) ->
    kapi_route:publish_req(Request),
    wait_for_route_resp(add_time_marker('request_sent', Map)).

-spec wait_for_route_resp(dialplan_context()) -> {'ok', dialplan_context()}.
wait_for_route_resp(#{timeout := TimeoutMs}=Map) ->
    lager:debug("waiting ~B ms for route response", [TimeoutMs]),
    StartTime = kz_time:start_time(),
    receive
        {'kapi', {{_, _, {Basic, _Deliver}}, {'dialplan', 'route_resp'}, Resp}} ->
            Props = [{'basic', Basic}],
            case kz_api:defer_response(Resp) of
                'true' ->
                    NewTimeoutMs = TimeoutMs - kz_time:elapsed_ms(StartTime),
                    lager:debug("received deferred reply - waiting for others for ~B ms", [NewTimeoutMs]),
                    wait_for_route_resp(Map#{timeout => NewTimeoutMs, reply => #{payload => Resp, props => Props}});
                'false' ->
                    lager:info("received route reply"),
                    NewTimeoutMs = TimeoutMs - kz_time:elapsed_ms(StartTime),
                    maybe_wait_for_authz(Map#{reply => #{payload => Resp, props => Props}, authz_timeout => NewTimeoutMs})
            end
    after TimeoutMs ->
            lager:warning("timeout after ~B receiving route response", [TimeoutMs]),
            send_reply(Map)
    end.

-spec spawn_authorize_call_fun(dialplan_context()) -> dialplan_context().
spawn_authorize_call_fun(#{node := Node, call_id := CallId, payload := JObj}=Map) ->
    Ref = make_ref(),
    Pid = kz_util:spawn(fun authorize_call_fun/5, [self(), Ref, Node, CallId, JObj]),
    Map#{authz_worker => {Pid, Ref}}.

-spec authorize_call_fun(pid(), Ref, atom(), kz_term:ne_binary(), kz_json:object()) ->
                                {'authorize_reply', Ref, ecallmgr_fs_authz:authz_reply()}
                                    when Ref :: reference().
authorize_call_fun(Parent, Ref, Node, CallId, JObj) ->
    kz_util:put_callid(CallId),
    Parent ! {'authorize_reply', Ref, ecallmgr_fs_authz:authorize(JObj, CallId, Node)}.

-spec maybe_wait_for_authz(dialplan_context()) -> {'ok', dialplan_context()}.
maybe_wait_for_authz(#{authz_worker := _AuthzWorker, reply := #{payload := Reply}}=Map) ->
    case kz_json:get_value(<<"Method">>, Reply) =/= <<"error">> of
        'true' -> wait_for_authz(Map);
        'false' -> wait_and_ignore_authz(Map)
    end;
maybe_wait_for_authz(#{}=Map) ->
    send_reply(Map).

-spec wait_and_ignore_authz(dialplan_context()) -> {'ok', dialplan_context()}.
wait_and_ignore_authz(#{authz_worker := {Pid, Ref}
                       ,authz_timeout := Timeout
                       }=Map) ->
    %% If we don't wait for an authz worker and just send an error reply
    %% as soon as the route_resp comes in then FreeSWITCH will terminate
    %% the call and Jonny5 will still be processing the authz_req.
    %% If Jonny5 authorizes the now dead call then it will add it to
    %% the j5_channels but FreeSWITCH will have already destroyed
    %% it and the CHANNEL_DESTROY will have already been sent
    %% to j5_channels so the authorization gets stuck.
    lager:info("waiting for authz reply from worker ~p", [Pid]),
    receive
        {'authorize_reply', Ref, _} ->
            lager:debug("got authz reply, but sending error response so ignoring"),
            send_reply(Map)
    after Timeout ->
            lager:warning("timeout waiting for authz reply from worker ~p but sending error so ignoring"
                         ,[Pid]
                         ),
            send_reply(Map)
    end.

-spec wait_for_authz(dialplan_context()) -> {'ok', dialplan_context()}.
wait_for_authz(#{authz_worker := {Pid, Ref}
                ,authz_timeout := Timeout
                ,reply := #{payload := JObj}=Reply
                }=Map) ->
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
            lager:warning("timeout waiting for authz reply from worker ~p", [Pid]),
            {'ok', Map}
    end.

-spec send_reply(dialplan_context()) -> {'ok', dialplan_context()}.
send_reply(#{node := Node, fetch_id := FetchId, reply := #{payload := Reply}}=Ctx) ->
    {'ok', XML} = ecallmgr_fs_xml:route_resp_xml('dialplan', Reply, Ctx),
    lager:debug("sending xml dialplan reply for request ~s tp ~s", [FetchId, Node]),
    _ = freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(XML)}),
    case kz_api:defer_response(Reply)
        orelse kz_json:get_ne_binary_value(<<"Method">>, Reply) =/= <<"park">>
    of
        'true' -> {'ok', Ctx};
        'false' -> wait_for_route_winner(Ctx)
    end.

-spec wait_for_route_winner(dialplan_context()) -> {'ok', dialplan_context()}.
wait_for_route_winner(Ctx) ->
    receive
        {'kapi', {_, {'dialplan', 'ROUTE_WINNER'}, JObj}} ->
            activate_call_control(Ctx#{winner => #{payload => JObj}});
        {'route_winner', JObj, Props} ->
            activate_call_control(Ctx#{winner => #{payload => JObj, props => Props}})
    after ?ROUTE_WINNER_TIMEOUT ->
            lager:warning("timeout after ~B receiving route winner", [?ROUTE_WINNER_TIMEOUT]),
            {'ok', Ctx}
    end.

-spec activate_call_control(dialplan_context()) -> {'ok', dialplan_context()}.
activate_call_control(#{call_id := CallId, winner := #{payload := JObj}} = Map) ->
    kz_util:put_callid(CallId),
    CCVs = kzd_fetch:ccvs(JObj),
    ControllerQ = kzd_fetch:controller_queue(JObj),
    ecallmgr_fs_channels:update(CallId, #channel.handling_locally, 'true'),
    Args = Map#{controller_q => ControllerQ
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

-spec timeout_reply(dialplan_context()) -> dialplan_context().
timeout_reply(Map) ->
    Map#{reply => #{payload => error_message(), props => []}}.

-spec forbidden_reply(dialplan_context()) -> dialplan_context().
forbidden_reply(#{fetch_id := FetchId}=Map) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    Map#{reply => #{payload => error_message(<<"403">>, <<"Incoming call barred">>), props => []}}.

-spec route_winner(dialplan_context()) -> 'ok'.
route_winner(#{payload := JObj}=_Map) ->
    NodeWinner = kzd_fetch:ccv(JObj, <<"Ecallmgr-Node">>),
    case NodeWinner =:= kz_term:to_binary(node()) of
        'true' ->
            Pid = kz_term:to_pid(kz_api:reply_to(JObj)),
            Pid ! {'route_winner', JObj, []},
            lager:debug("we are the route winner handling node");
        'false' ->
            lager:debug("route winner handled by other node : ~s", [NodeWinner])
    end.
