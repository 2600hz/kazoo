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
    Routines = [fun call_id/1
               ,fun timeout/1
               ,{fun add_time_marker/2, 'start_processing'}
               ,fun(M) -> M#{channel => kz_amqp_channel:consumer_channel()} end
               ,fun(M) -> M#{callback => fun process/1} end
               ,fun(M) -> M#{options => []} end
               ,fun(M) -> M#{start_result => ecallmgr_call_control_sup:start_proc(M)} end
               ],
    {'ok', kz_maps:exec(Routines, Map)}.

-spec process(dialplan_context()) -> {'ok', dialplan_context()}.
process(#{payload := FetchJObj, channel := Channel}=Map) ->
    kz_log:put_callid(FetchJObj),
    _ = kz_amqp_channel:consumer_channel(Channel),
    Routines = [{fun add_time_marker/2, 'request_ready'}
               ,fun control_p/1
               ,fun request/1
               ,fun block_call_routines/1
               ,fun apply_formatters/1
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
maybe_authz(#{blocked := 'true'}=Map) -> Map;
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
maybe_expired(Map) ->
    maybe_blocked(Map).

-spec maybe_blocked(dialplan_context()) -> {'ok', dialplan_context()}.
maybe_blocked(#{blocked := 'true'}=Map) ->
    send_reply(Map);
maybe_blocked(#{request := Request}=Map) ->
    kapi_route:publish_req(Request),
    wait_for_route_resp(add_time_marker('request_sent', Map)).

-spec wait_for_route_resp(dialplan_context()) -> {'ok', dialplan_context()}.
wait_for_route_resp(#{timeout := TimeoutMs}=Map) ->
    lager:debug("waiting ~B ms for route response", [TimeoutMs]),
    StartTime = kz_time:start_time(),
    receive
        {'kapi', {_, {'dialplan', 'route_resp'}, Resp}} ->
            case kz_api:defer_response(Resp) of
                'true' ->
                    NewTimeoutMs = TimeoutMs - kz_time:elapsed_ms(StartTime),
                    lager:debug("received deferred reply - waiting for others for ~B ms", [NewTimeoutMs]),
                    wait_for_route_resp(Map#{timeout => NewTimeoutMs, reply => #{payload => Resp}});
                'false' ->
                    lager:info("received route reply"),
                    NewTimeoutMs = TimeoutMs - kz_time:elapsed_ms(StartTime),
                    maybe_wait_for_authz(Map#{reply => #{payload => Resp}, authz_timeout => NewTimeoutMs})
            end
    after TimeoutMs ->
            lager:warning("timeout after ~B receiving route response", [TimeoutMs]),
            send_reply(Map)
    end.

-spec spawn_authorize_call_fun(dialplan_context()) -> dialplan_context().
spawn_authorize_call_fun(#{node := Node, call_id := CallId, payload := JObj}=Map) ->
    Ref = make_ref(),
    Pid = kz_process:spawn(fun authorize_call_fun/5, [self(), Ref, Node, CallId, JObj]),
    Map#{authz_worker => {Pid, Ref}}.

-spec authorize_call_fun(pid(), Ref, atom(), kz_term:ne_binary(), kz_json:object()) ->
                                {'authorize_reply', Ref, ecallmgr_fs_authz:authz_reply()}
                                    when Ref :: reference().
authorize_call_fun(Parent, Ref, Node, CallId, JObj) ->
    kz_log:put_callid(CallId),
    Parent ! {'authorize_reply', Ref, ecallmgr_fs_authz:authorize(JObj, CallId, Node)}.

-spec maybe_wait_for_authz(dialplan_context()) -> {'ok', dialplan_context()}.
maybe_wait_for_authz(#{authz_worker := _AuthzWorker, reply := #{payload := Reply}}=Map) ->
    case kz_json:get_value(<<"Method">>, Reply) =/= <<"error">> of
        'true' -> wait_for_authz(Map);
        'false' -> send_reply(Map)
    end;
maybe_wait_for_authz(#{}=Map) ->
    send_reply(Map).

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
        {'route_winner', JObj, _Props} ->
            activate_call_control(Ctx#{winner => #{payload => JObj}})
    after ?ROUTE_WINNER_TIMEOUT ->
            lager:warning("timeout after ~B receiving route winner", [?ROUTE_WINNER_TIMEOUT]),
            {'ok', Ctx}
    end.

-spec activate_call_control(dialplan_context()) -> {'ok', dialplan_context()}.
activate_call_control(#{call_id := CallId, winner := #{payload := JObj}} = Map) ->
    lager:info("we are the route winner handling node"),
    kz_log:put_callid(CallId),
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
timeout_reply(#{blocked := 'true'} = Map) -> Map;
timeout_reply(Map) ->
    Map#{reply => #{payload => error_message()}}.

-spec forbidden_reply(dialplan_context()) -> dialplan_context().
forbidden_reply(#{fetch_id := FetchId}=Map) ->
    lager:info("received forbidden route response for ~s, sending 403 Incoming call barred", [FetchId]),
    Map#{reply => #{payload => error_message(<<"403">>, <<"Incoming call barred">>)}}.

-spec route_winner(dialplan_context()) -> 'ok'.
route_winner(#{payload := JObj}=_Map) ->
    NodeWinner = kzd_fetch:ccv(JObj, <<"Ecallmgr-Node">>),
    case NodeWinner =:= kz_term:to_binary(node()) of
        'true' ->
            Pid = kz_term:to_pid(kz_api:reply_to(JObj)),
            Pid ! {'route_winner', JObj, []};
        'false' ->
            lager:info("route winner handled by other node : ~s", [NodeWinner])
    end.


-spec block_call_routines(dialplan_context()) -> dialplan_context().
block_call_routines(Map) ->
    Routines = [{fun should_block_anonymous/1, {<<"433">>, <<"Anonymity Disallowed">>}}
               ,{fun is_blacklisted/1, {<<"603">>, <<"Decline">>}}
               ],
    lists:foldl(fun block_call_routine/2, Map, Routines).

-type block_call_fun() :: fun((kz_json:object()) -> boolean()).
-type block_call_resp() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-type block_call_arg() :: {block_call_fun(), block_call_resp()}.

-spec block_call_routine(block_call_arg(), dialplan_context()) -> dialplan_context().
block_call_routine({_Fun, {_Code, _Msg}}, #{blocked := 'true'}=Map) -> Map;
block_call_routine({Fun, {Code, Msg}}, #{request := JObj}=Map) ->
    case Fun(JObj) of
        'true' -> Map#{reply => #{payload => error_message(Code, Msg)}
                      ,blocked => 'true'
                      };
        'false' -> Map
    end.

-spec should_block_anonymous(kz_json:object()) -> boolean().
should_block_anonymous(JObj) ->
    kz_privacy:should_block_anonymous(JObj)
        orelse (kz_privacy:is_anonymous(JObj)
                andalso kz_json:is_true(<<"should_block_anonymous">>, get_blacklist(JObj))
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_blacklisted(kz_json:object()) -> boolean().
is_blacklisted(JObj) ->
    is_number_blacklisted(get_blacklist(JObj), JObj).

-spec is_number_blacklisted(kz_json:object(), kz_json:object()) -> boolean().
is_number_blacklisted(Blacklist, JObj) ->
    Number = kz_json:get_value(<<"Caller-ID-Number">>, JObj, kz_privacy:anonymous_caller_id_number()),
    Normalized = knm_converters:normalize(Number),
    case kz_json:get_value(Normalized, Blacklist) of
        'undefined' -> 'false';
        _ -> lager:info("~s(~s) is blacklisted", [Number, Normalized]),
             'true'
    end.

-spec get_blacklists(kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binaries()} |
                            {'error', any()}.
get_blacklists(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'error', _R}=E ->
            lager:error("could not open account doc ~s : ~p", [AccountId, _R]),
            E;
        {'ok', Doc} ->
            case kz_json:get_value(<<"blacklists">>, Doc, []) of
                [] -> {'error', 'undefined'};
                [_|_]=Blacklists-> {'ok', Blacklists};
                _ -> {'error', 'miss_configured'}
            end
    end.

-spec get_blacklist(kz_json:object()) -> kz_json:object().
get_blacklist(JObj) ->
    AccountId = kzd_fetch:account_id(JObj),
    case get_blacklists(AccountId) of
        {'error', _R} -> kz_json:new();
        {'ok', Blacklists} -> get_blacklist(AccountId, Blacklists)
    end.

-spec get_blacklist(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_json:object().
get_blacklist(AccountId, Blacklists) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    lists:foldl(fun(BlacklistId, Acc) ->
                        case kz_datamgr:open_cache_doc(AccountDb, BlacklistId) of
                            {'error', _R} ->
                                lager:error("could not open ~s in ~s: ~p", [BlacklistId, AccountDb, _R]),
                                Acc;
                            {'ok', Doc} ->
                                Numbers = kz_json:get_value(<<"numbers">>, Doc, kz_json:new()),
                                BlackList = maybe_set_block_anonymous(Numbers, kz_json:is_true(<<"should_block_anonymous">>, Doc)),
                                kz_json:merge_jobjs(Acc, BlackList)
                        end
                end
               ,kz_json:new()
               ,Blacklists
               ).

-spec maybe_set_block_anonymous(kz_json:object(), boolean()) -> kz_json:object().
maybe_set_block_anonymous(JObj, 'false') -> JObj;
maybe_set_block_anonymous(JObj, 'true') ->
    kz_json:set_value(<<"should_block_anonymous">>, 'true', JObj).

-spec apply_formatters(dialplan_context()) -> dialplan_context().
apply_formatters(#{request := JObj}=Map) ->
    case kzd_fetch:formatters(JObj) of
        'undefined' -> Map;
        Formatters -> apply_formatters(Formatters, Map)
    end.

-spec apply_formatters(kz_json:object(), dialplan_context()) -> dialplan_context().
apply_formatters(Formatters, #{request := JObj}=Map) ->
    Map#{request => kz_formatters:apply(JObj, Formatters, 'inbound')}.
