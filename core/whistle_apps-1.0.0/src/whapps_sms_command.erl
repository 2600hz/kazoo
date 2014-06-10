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
    _CallId = whapps_call:call_id(Call),
    API = create_sms(Call, Endpoints),
    whapps_util:amqp_pool_send(API, fun wapi_sms:publish_message/1),
    'ok'.

b_send_sms(Endpoints, Call) -> b_send_sms(Endpoints, ?DEFAULT_MESSAGE_TIMEOUT, Call).
b_send_sms(Endpoints, Timeout, Call) -> b_send_sms(Endpoints, Timeout, 'undefined', Call).
b_send_sms(Endpoints, Timeout, _SIPHeaders, Call) ->
    CallId = whapps_call:call_id(Call),
    API = create_sms(Call, Endpoints),
    whapps_util:amqp_pool_send(API, fun wapi_sms:publish_message/1),
    ReqResp = wait_for_correlated_message(Call, <<"delivery">>, <<"message">>, Timeout),
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
create_sms(Call, Endpoints) ->
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
     ,{<<"To">>, whapps_call:to(Call)}
     ,{<<"Request">>, whapps_call:request(Call) }
     ,{<<"Endpoints">>, Endpoints}
     ,{<<"Application-Name">>, <<"send">>}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, wh_json:new())}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec get_correlated_msg_type(wh_json:object()) ->
                                     {api_binary(), api_binary(), api_binary()}.
-spec get_correlated_msg_type(ne_binary(), wh_json:object()) ->
                                     {api_binary(), api_binary(), api_binary()}.
get_correlated_msg_type(JObj) ->
    get_correlated_msg_type(<<"Call-ID">>, JObj).
get_correlated_msg_type(Key, JObj) ->
    {C, N} = wh_util:get_event_type(JObj),
    {C, N, wh_json:get_value(Key, JObj)}.

-spec wait_for_correlated_message(whapps_call:call(), ne_binary(), ne_binary(), wh_timeout()) ->
                                         whapps_api_std_return().
wait_for_correlated_message(Call, Event, Type, Timeout) ->
    Start = os:timestamp(),
    CallId = whapps_call:call_id(Call),
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
                    wait_for_correlated_message(Call, Event, Type, whapps_util:decr_timeout(Timeout, Start))
            end
    end.
