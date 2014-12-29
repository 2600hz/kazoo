%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whapps_sms_command).

-include("./whapps_call_command.hrl").

-export([send_sms/2, send_sms/3, send_sms/4]).
-export([b_send_sms/2, b_send_sms/3, b_send_sms/4]).

-export([default_collect_timeout/0
         ,default_message_timeout/0
         ,default_application_timeout/0
        ]).

-define(CONFIG_CAT, <<"sms_command">>).

-define(DEFAULT_COLLECT_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"collect_timeout">>, 60000)).

-define(DEFAULT_MESSAGE_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"message_timeout">>, 60000)).
-define(DEFAULT_APPLICATION_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, <<"application_timeout">>, 500000)).

-spec default_collect_timeout() -> pos_integer().
default_collect_timeout() ->
    ?DEFAULT_COLLECT_TIMEOUT.

-spec default_message_timeout() -> pos_integer().
default_message_timeout() ->
    ?DEFAULT_MESSAGE_TIMEOUT.

-spec default_application_timeout() -> pos_integer().
default_application_timeout() ->
    ?DEFAULT_APPLICATION_TIMEOUT.

-type whapps_api_sms_return() :: {'error', 'timeout' | wh_json:object() }
                                   | {'fail', wh_json:object()}
                                   | {'ok', wh_json:object()}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec send_sms(wh_json:objects(), whapps_call:call()) -> 'ok'.
-spec send_sms(wh_json:objects(), integer(), whapps_call:call()) -> 'ok'.
-spec send_sms(wh_json:objects(), integer(), api_binary(), whapps_call:call()) -> 'ok'.

-spec b_send_sms(wh_json:objects(), whapps_call:call()) -> whapps_api_sms_return().
-spec b_send_sms(wh_json:objects(), integer(), whapps_call:call()) -> whapps_api_sms_return().
-spec b_send_sms(wh_json:objects(), integer(), api_binary(), whapps_call:call()) -> whapps_api_sms_return().

send_sms(Endpoints, Call) -> send_sms(Endpoints, ?DEFAULT_MESSAGE_TIMEOUT, Call).
send_sms(Endpoints, Timeout, Call) -> send_sms(Endpoints, Timeout, 'undefined', Call).
send_sms(Endpoints, _Timeout, _SIPHeaders, Call) ->
    API = create_sms(Call, Endpoints),
    whapps_util:amqp_pool_send(API, fun wapi_sms:publish_message/1),
    'ok'.

b_send_sms(Endpoints, Call) -> b_send_sms(Endpoints, ?DEFAULT_MESSAGE_TIMEOUT, Call).
b_send_sms(Endpoints, Timeout, Call) -> b_send_sms(Endpoints, Timeout, 'undefined', Call).
b_send_sms(Endpoints, Timeout, _SIPHeaders, Call) ->
    CallId = whapps_call:call_id(Call),
    API = create_sms(Call, Endpoints),
    whapps_util:amqp_pool_send(API, fun wapi_sms:publish_message/1),
    ReqResp = wait_for_correlated_message(CallId, <<"delivery">>, <<"message">>, Timeout),
    lager:debug("sending sms and waiting for response ~s", [CallId]),
    case ReqResp of
        {'error', _R}=E ->
            lager:info("recieved error while sending msg ~s: ~-800p", [CallId, _R]),
            E;
        {_, _JObjs} = Ret ->
             lager:debug("received sms delivery result for msg ~s", [CallId]),
             Ret
    end.

-spec create_sms(whapps_call:call(), wh_proplist()) -> wh_proplist().
create_sms(Call, TargetEndpoints) ->
    Endpoints = create_sms_endpoints(Call, TargetEndpoints, []),
    [Endpoint] = Endpoints,
    AccountId = whapps_call:account_id(Call),
    AccountRealm =  whapps_call:to_realm(Call),
    CCVUpdates = props:filter_undefined(
                   [{<<"Ignore-Display-Updates">>, <<"true">>}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Account-Realm">>, AccountRealm}
                    ,{<<"From-User">>, whapps_call:from_user(Call)}
                    ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
                    ,{<<"From-URI">>, whapps_call:from(Call)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),
    [{<<"Message-ID">>, whapps_call:kvs_fetch(<<"Message-ID">>, Call)}
     ,{<<"Call-ID">>, whapps_call:call_id(Call)}
     ,{<<"Body">>, whapps_call:kvs_fetch(<<"Body">>, Call)}
     ,{<<"From">>, whapps_call:from(Call)}
     ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
     ,{<<"Route-ID">>, wh_json:get_value(<<"Route-ID">>, Endpoint)}
     ,{<<"To">>, whapps_call:to(Call)}
     ,{<<"Request">>, whapps_call:request(Call) }
     ,{<<"Endpoints">>, Endpoints}
     ,{<<"Application-Name">>, <<"send">>}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, wh_json:new())}
     | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)
    ].

-spec create_sms_endpoints(whapps_call:call(), wh_proplist(), wh_proplist()) -> wh_proplist().
create_sms_endpoints(Call, [], Endpoints) -> Endpoints;
create_sms_endpoints(Call, [Endpoint | Others], Endpoints) ->
    Realm = wh_json:get_value(<<"To-Realm">>, Endpoint),
    Username = wh_json:get_value(<<"To-User">>, Endpoint),
    case lookup_reg(Username, Realm) of
        {'ok', Node} ->
            List = [ wh_json:set_value(<<"Route-ID">>, Node, Endpoint) | Endpoints],
            create_sms_endpoints(Call, Others, List);
        {'error', _E} ->            
            create_sms_endpoints(Call, Others, Endpoints)
    end.
            

                                                       

-spec lookup_reg(ne_binary(), ne_binary()) -> wh_json:objects().
lookup_reg(Username, Realm) ->
    Req = [{<<"Realm">>, Realm}
           ,{<<"Username">>, Username}
           ,{<<"Fields">>, [<<"Registrar-Node">>]}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_registration:publish_query_req/1
                                       ,{'ecallmgr', 'true'}
                                      )
    of
        {'error', _E}=E ->
            lager:debug("error getting registration: ~p", [_E]),
            E;
        {_, JObjs} ->
            [FirstNode] = [Node || Node <- extract_device_registrations(JObjs)],
            {'ok', FirstNode}
    end.

-spec extract_device_registrations(wh_json:objects()) -> ne_binaries().
extract_device_registrations(JObjs) ->
    sets:to_list(extract_device_registrations(JObjs, sets:new())).

-spec extract_device_registrations(wh_json:objects(), set()) -> set().
extract_device_registrations([], Set) -> Set;
extract_device_registrations([JObj|JObjs], Set) ->
    Fields = wh_json:get_value(<<"Fields">>, JObj, []),
    S = lists:foldl(fun(J, S) ->
                            case wh_json:get_ne_value(<<"Registrar-Node">>, J) of
                                'undefined' -> S;
                                AuthId -> sets:add_element(AuthId, S)
                            end
                    end, Set, Fields),
    extract_device_registrations(JObjs, S).


-spec get_correlated_msg_type(wh_json:object()) ->
                                     {api_binary(), api_binary(), api_binary()}.
-spec get_correlated_msg_type(ne_binary(), wh_json:object()) ->
                                     {api_binary(), api_binary(), api_binary()}.
get_correlated_msg_type(JObj) ->
    get_correlated_msg_type(<<"Call-ID">>, JObj).
get_correlated_msg_type(Key, JObj) ->
    {C, N} = wh_util:get_event_type(JObj),
    {C, N, wh_json:get_value(Key, JObj)}.

-spec wait_for_correlated_message(ne_binary() | whapps_call:call(), ne_binary(), ne_binary(), wh_timeout()) ->
                                         whapps_api_std_return().
wait_for_correlated_message(CallId, Event, Type, Timeout) when is_binary(CallId) ->
    Start = os:timestamp(),
    case whapps_call_command:receive_event(Timeout) of
        {'error', 'timeout'}=E -> E;
        {'ok', JObj} ->
            case get_correlated_msg_type(JObj) of
                {<<"error">>, _, CallId} ->
                    lager:debug("channel execution error while waiting for ~s", [CallId]),
                    {'error', JObj};
                {Type, Event, CallId } ->
                    {'ok', JObj};
                {_Type, _Event, _CallId} ->
                    lager:debug("received message (~s , ~s, ~s)",[_Type, _Event, _CallId]),
                    wait_for_correlated_message(CallId, Event, Type, wh_util:decr_timeout(Timeout, Start))
            end
    end;
wait_for_correlated_message(Call, Event, Type, Timeout) ->
    CallId = whapps_call:call_id(Call),
    wait_for_correlated_message(CallId, Event, Type, Timeout).
