%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_call_response).

-export([send/3, send/4, send/5]).
-export([send_default/2]).
-export([get_response/2]).
-export([config_doc_id/0]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(CALL_RESPONSE_CONF, <<"call_response">>).

-spec config_doc_id() -> kz_term:ne_binary().
config_doc_id() ->
    ?CALL_RESPONSE_CONF.

%%------------------------------------------------------------------------------
%% @doc Create a call response, as a queue of events when media should be
%% played as part of the error.
%% @end
%%------------------------------------------------------------------------------

-spec send(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_response'}.
send(CallId, CtrlQ, Code) ->
    send(CallId, CtrlQ, Code, 'undefined').

-spec send(kz_term:ne_binary() | kapps_call:call(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_response'}.
send(<<_/binary>> = CallId, CtrlQ, Code, Cause) ->
    send(CallId, CtrlQ, Code, Cause, 'undefined');
send(Call, Code, Cause, Media) ->
    CallId = kapps_call:call_id(Call),
    CtrlQ = kapps_call:control_queue(Call),
    send(CallId, CtrlQ, Code, Cause, Media).

-spec send(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_response'}.
send(_, _, 'undefined', 'undefined', 'undefined') ->
    {'error', 'no_response'};
send(CallId, CtrlQ, Code, Cause, Media) ->
    NoopId = kz_datamgr:get_uuid(),

    SpecifcCommands = specific_commands(CallId, NoopId, Code, Cause, Media),

    Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ,{<<"Call-ID">>, CallId}
                                  ])
                | SpecifcCommands
               ],
    do_send(CallId, CtrlQ, Commands),
    {'ok', NoopId}.

-spec specific_commands(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_json:objects().
specific_commands(CallId, NoopId, 'undefined'=_Code, 'undefined'=_Cause, Media) ->
    [kz_json:from_list([{<<"Application-Name">>, <<"play">>}
                       ,{<<"Msg-ID">>, NoopId}
                       ,{<<"Media-Name">>, Media}
                       ,{<<"Call-ID">>, CallId}
                       ])
    ,kz_json:from_list([{<<"Application-Name">>, <<"progress">>}
                       ,{<<"Msg-ID">>, NoopId}
                       ,{<<"Call-ID">>, CallId}
                       ])
    ];
specific_commands(CallId, NoopId, Code, Cause, 'undefined'=_Media) ->
    [kz_json:from_list([{<<"Application-Name">>, <<"respond">>}
                       ,{<<"Msg-ID">>, NoopId}
                       ,{<<"Response-Code">>, Code}
                       ,{<<"Response-Message">>, Cause}
                       ,{<<"Call-ID">>, CallId}
                       ])
    ];
specific_commands(CallId, NoopId, Code, Cause, Media) ->
    [kz_json:from_list([{<<"Application-Name">>, <<"respond">>}
                       ,{<<"Response-Code">>, Code}
                       ,{<<"Response-Message">>, Cause}
                       ,{<<"Msg-ID">>, NoopId}
                       ,{<<"Call-ID">>, CallId}
                       ])
    ,kz_json:from_list([{<<"Application-Name">>, <<"play">>}
                       ,{<<"Media-Name">>, Media}
                       ,{<<"Msg-ID">>, NoopId}
                       ,{<<"Call-ID">>, CallId}
                       ])
    ,kz_json:from_list([{<<"Application-Name">>, <<"progress">>}
                       ,{<<"Msg-ID">>, NoopId}
                       ,{<<"Call-ID">>, CallId}
                       ])
    ].

-spec do_send(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) -> 'ok'.
do_send(CallId, CtrlQ, Commands) ->
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Call-ID">>, CallId}
              ,{<<"Commands">>, Commands}
              ,{<<"Msg-ID">>, kz_binary:rand_hex(6)}
               | kz_api:default_headers(<<"call">>, <<"command">>, <<"call_response">>, <<"0.1.0">>)
              ],
    kz_amqp_worker:cast(Command
                       ,fun(C) ->
                                {'ok', Payload} = kapi_dialplan:queue(C),
                                kapi_dialplan:publish_action(CtrlQ, Payload)
                        end
                       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec send_default(kapps_call:call(), kz_term:api_binary()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_response'}.
send_default(_Call, 'undefined') ->
    {'error', 'no_response'};
send_default(Call, Cause) ->
    case get_response(Cause, Call) of
        'undefined' ->
            lager:debug("no default response for '~s' found", [Cause]),
            {'error', 'no_response'};
        Response ->
            lager:debug("sending default response for '~s': ~p", [Cause, Response]),
            send_default_response(Call, Response)
    end.

-spec send_default_response(kapps_call:call(), kz_json:object()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', 'no_response'}.
send_default_response(Call, Response) ->
    Media = kz_json:get_ne_binary_value(<<"Media">>, Response),

    send(kapps_call:call_id(Call)
        ,kapps_call:control_queue(Call)
        ,kz_json:get_value(<<"Code">>, Response)
        ,kz_json:get_value(<<"Message">>, Response)
        ,kapps_call:get_prompt(Call, Media)
        ).

%%------------------------------------------------------------------------------
%% @doc Returns the configured response proplist.
%% @end
%%------------------------------------------------------------------------------
-spec get_response(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_object().
get_response(Cause, Call) ->
    Default = case default_response(Cause) of
                  'undefined' -> 'undefined';
                  Props -> kz_json:from_list(Props)
              end,
    AccountId = kapps_call:account_id(Call),
    kapps_account_config:get_global(AccountId, ?CALL_RESPONSE_CONF, Cause, Default).

%%------------------------------------------------------------------------------
%% @doc Returns the default action given the error.
%% @end
%%------------------------------------------------------------------------------
-spec default_response(kz_term:ne_binary()) -> 'undefined' | kz_term:proplist().
default_response(<<"RESPONSE_TO_STATUS_ENQUIRY">>) -> 'undefined';
default_response(<<"FACILITY_NOT_SUBSCRIBED">>) -> 'undefined';
default_response(<<"INVALID_MSG_UNSPECIFIED">>) -> 'undefined';

default_response(<<"INVALID_CALL_REFERENCE">>) -> 'undefined';
default_response(<<"CALL_AWARDED_DELIVERED">>) -> 'undefined';
default_response(<<"ACCESS_INFO_DISCARDED">>) -> 'undefined';
default_response(<<"MESSAGE_TYPE_NONEXIST">>) -> 'undefined';
default_response(<<"CHANNEL_UNACCEPTABLE">>) -> 'undefined';
default_response(<<"CHAN_NOT_IMPLEMENTED">>) -> 'undefined';

default_response(<<"INVALID_IE_CONTENTS">>) -> 'undefined';
default_response(<<"USER_NOT_REGISTERED">>) -> 'undefined';
default_response(<<"SERVICE_UNAVAILABLE">>) -> 'undefined';

default_response(<<"ATTENDED_TRANSFER">>) -> 'undefined';
default_response(<<"ALLOTTED_TIMEOUT">>) -> 'undefined';
default_response(<<"WRONG_CALL_STATE">>) -> 'undefined';
default_response(<<"MANAGER_REQUEST">>) -> 'undefined';
default_response(<<"SYSTEM_SHUTDOWN">>) -> 'undefined';

default_response(<<"PROTOCOL_ERROR">>) -> 'undefined';
default_response(<<"USER_CHALLENGE">>) -> 'undefined';
default_response(<<"BLIND_TRANSFER">>) -> 'undefined';
default_response(<<"WRONG_MESSAGE">>) -> 'undefined';
default_response(<<"INTERWORKING">>) -> 'undefined';
default_response(<<"UNSPECIFIED">>) -> 'undefined';
default_response(<<"IE_NONEXIST">>) -> 'undefined';
default_response(<<"PICKED_OFF">>) -> 'undefined';
default_response(<<"PRE_EMPTED">>) -> 'undefined';
default_response(<<"LOSE_RACE">>) -> 'undefined';
default_response(<<"CRASH">>) -> 'undefined';

default_response(<<"UNALLOCATED_NUMBER">>) ->
    [{<<"Code">>, <<"404">>}
    ,{<<"Message">>, <<"No route to destination">>}
    ,{<<"Media">>, <<"fault-can_not_be_completed_as_dialed">>}
    ];
default_response(<<"NO_ROUTE_TRANSIT_NET">>) ->
    [{<<"Code">>, <<"404">>}
    ,{<<"Message">>, <<"Invalid number">>}
    ];
default_response(<<"NO_ROUTE_DESTINATION">>) ->
    [{<<"Code">>, <<"404">>}
    ,{<<"Message">>, <<"No route to destination">>}
    ,{<<"Media">>, <<"fault-can_not_be_completed_as_dialed">>}
    ];
default_response(<<"NORMAL_CLEARING">>) ->
    [{<<"Media">>, <<"tone_stream://%(500,500,480,620);loops=25">>}];
default_response(<<"USER_BUSY">>) ->
    [{<<"Code">>, <<"486">>}
    ,{<<"Message">>, <<"Number busy">>}
    ,{<<"Media">>, <<"tone_stream://%(500,500,480,620);loops=25">>}
    ];
default_response(<<"NO_USER_RESPONSE">>) ->
    [{<<"Code">>, <<"408">>}
    ,{<<"Message">>, <<"No response">>}
    ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(<<"NO_ANSWER">>) ->
    [{<<"Code">>, <<"480">>}
    ,{<<"Message">>, <<"No answer">>}
    ];
default_response(<<"SUBSCRIBER_ABSENT">>) ->
    [{<<"Code">>, <<"480">>}
    ,{<<"Message">>, <<"Subscriber absent">>}
    ];
default_response(<<"CALL_REJECTED">>) ->
    [{<<"Code">>, <<"603">>}
    ,{<<"Message">>, <<"Call Rejected">>}
    ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(<<"NUMBER_CHANGED">>) ->
    [{<<"Code">>, <<"410">>}
    ,{<<"Message">>, <<"Number changed">>}
    ];
default_response(<<"REDIRECTION_TO_NEW_DESTINATION">>) ->
    [{<<"Code">>, <<"410">>}
    ,{<<"Message">>, <<"Redirection to new destination">>}
    ];
default_response(<<"EXCHANGE_ROUTING_ERROR">>) ->
    [{<<"Code">>, <<"483">>}
    ,{<<"Message">>, <<"Exchange routing error">>}
    ,{<<"Media">>, <<"fault-facility_trouble">>}
    ];
default_response(<<"DESTINATION_OUT_OF_ORDER">>) ->
    [{<<"Code">>, <<"502">>}
    ,{<<"Message">>, <<"Destination out of order">>}
    ,{<<"Media">>, <<"fault-can_not_be_completed_as_dialed">>}
    ];
default_response(<<"INVALID_NUMBER_FORMAT">>) ->
    [{<<"Code">>, <<"484">>}
    ,{<<"Message">>, <<"Invalid number format">>}
    ,{<<"Media">>, <<"fault-can_not_be_completed_as_dialed">>}
    ];
default_response(<<"FACILITY_REJECTED">>) ->
    [{<<"Code">>, <<"510">>}
    ,{<<"Message">>, <<"Facility rejected">>}
    ];
default_response(<<"NORMAL_UNSPECIFIED">>) ->
    [{<<"Code">>, <<"480">>}
    ,{<<"Message">>, <<"Normal unspecified">>}
    ];
default_response(<<"NORMAL_CIRCUIT_CONGESTION">>) ->
    [{<<"Code">>, <<"503">>}
    ,{<<"Message">>, <<"Normal circuit congestion">>}
    ];
default_response(<<"NETWORK_OUT_OF_ORDER">>) ->
    [{<<"Code">>, <<"503">>}
    ,{<<"Message">>, <<"Network out of order">>}
    ];
default_response(<<"NORMAL_TEMPORARY_FAILURE">>) ->
    [{<<"Code">>, <<"503">>}
    ,{<<"Message">>, <<"Normal temporary failure">>}
    ,{<<"Media">>, <<"fault-can_not_be_completed_at_this_time">>}
    ];
default_response(<<"SWITCH_CONGESTION">>) ->
    [{<<"Code">>, <<"503">>}
    ,{<<"Message">>, <<"Switch congestion">>}
    ];
default_response(<<"REQUESTED_CHAN_UNAVAIL">>) ->
    [{<<"Code">>, <<"503">>}
    ,{<<"Message">>, <<"Requested channel unavailable">>}
    ];
default_response(<<"OUTGOING_CALL_BARRED">>) ->
    [{<<"Code">>, <<"403">>}
    ,{<<"Message">>, <<"Outgoing call barred">>}
    ];
default_response(<<"INCOMING_CALL_BARRED">>) ->
    [{<<"Code">>, <<"403">>}
    ,{<<"Message">>, <<"Incoming call barred">>}
    ];
default_response(<<"BEARERCAPABILITY_NOTAUTH">>) ->
    [{<<"Code">>, <<"403">>}
    ,{<<"Message">>, <<"Bearer capability not authorized">>}
    ];
default_response(<<"BEARERCAPABILITY_NOTAVAIL">>) ->
    [{<<"Code">>, <<"403">>}
    ,{<<"Message">>, <<"Bearer capability not presently available">>}
    ];
default_response(<<"BEARERCAPABILITY_NOTIMPL">>) ->
    [{<<"Code">>, <<"488">>}
    ,{<<"Message">>, <<"Bearer capability not implemented">>}
    ];
default_response(<<"UNKNOWN">>) ->
    [{<<"Code">>, <<"500">>}
    ,{<<"Message">>, <<"Unknown error happened">>}
    ];
default_response(<<"FACILITY_NOT_IMPLEMENTED">>) ->
    [{<<"Code">>, <<"501">>}
    ,{<<"Message">>, <<"Facility not implemented">>}
    ];
default_response(<<"SERVICE_NOT_IMPLEMENTED">>) ->
    [{<<"Code">>, <<"501">>}
    ,{<<"Message">>, <<"Service not implemented">>}
    ,{<<"Media">>, <<"fault-facility_trouble">>}
    ];
default_response(<<"INCOMPATIBLE_DESTINATION">>) ->
    [{<<"Code">>, <<"488">>}
    ,{<<"Message">>, <<"Incompatible destination">>}
    ,{<<"Media">>, <<"fault-facility_trouble">>}
    ];
default_response(<<"MANDATORY_IE_MISSING">>) ->
    [{<<"Code">>, <<"400">>}
    ,{<<"Message">>, <<"Mandatory information missing">>}
    ,{<<"Media">>, <<"fault-facility_trouble">>}
    ];
default_response(<<"RECOVERY_ON_TIMER_EXPIRE">>) ->
    [{<<"Code">>, <<"504">>}
    ,{<<"Message">>, <<"Recovery on timer expire">>}
    ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(<<"MANDATORY_IE_LENGTH_ERROR">>) ->
    [{<<"Code">>, <<"400">>}
    ,{<<"Message">>, <<"Mandatory information missing">>}
    ,{<<"Media">>, <<"fault-facility_trouble">>}
    ];
default_response(<<"ORIGINATOR_CANCEL">>) ->
    [{<<"Code">>, <<"487">>}
    ,{<<"Message">>, <<"Originator cancel">>}
    ];
default_response(<<"MEDIA_TIMEOUT">>) ->
    [{<<"Code">>, <<"504">>}
    ,{<<"Message">>, <<"Media timeout">>}
    ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(<<"PROGRESS_TIMEOUT">>) ->
    [{<<"Code">>, <<"486">>}
    ,{<<"Message">>, <<"Progress timeout">>}
    ,{<<"Media">>, <<"fault-can_not_be_completed_at_this_time">>}
    ].
