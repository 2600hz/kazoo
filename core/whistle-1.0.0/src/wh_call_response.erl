%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_call_response).

-export([send/3, send/4, send/5]).
-export([send_default/2]).
-export([get_response/2]).
-export([default_response/2]).
-export([config_doc_id/0]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(CALL_RESPONSE_CONF, <<"call_response">>).

-spec config_doc_id() -> ne_binary().
config_doc_id() ->
    ?CALL_RESPONSE_CONF.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a call response, as a queue of events when media should be
%% played as part of the error.
%% @end
%%--------------------------------------------------------------------
-spec send(ne_binary(), ne_binary(), api_binary()) ->
                  {'ok', ne_binary()} |
                  {'error', 'no_response'}.
-spec send(ne_binary() | whapps_call:call(), ne_binary(), api_binary(), 'undefined' | binary()) ->
                  {'ok', ne_binary()} |
                  {'error', 'no_response'}.
-spec send(ne_binary(), ne_binary(), api_binary(), 'undefined' | binary(), 'undefined' | binary()) ->
                  {'ok', ne_binary()} |
                  {'error', 'no_response'}.

send(CallId, CtrlQ, Code) ->
    send(CallId, CtrlQ, Code, 'undefined').

send(<<_/binary>> = CallId, CtrlQ, Code, Cause) ->
    send(CallId, CtrlQ, Code, Cause, 'undefined');
send(Call, Code, Cause, Media) ->
    CallId = whapps_call:call_id(Call),
    CtrlQ = whapps_call:control_queue(Call),
    send(CallId, CtrlQ, Code, Cause, Media).

send(_, _, 'undefined', 'undefined', 'undefined') ->
    {'error', 'no_response'};
send(CallId, CtrlQ, 'undefined', 'undefined', Media) ->
    NoopId = couch_mgr:get_uuid(),
    Commands = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                   ,{<<"Msg-ID">>, NoopId}
                                   ,{<<"Call-ID">>, CallId}
                                  ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"play">>}
                                    ,{<<"Msg-ID">>, NoopId}
                                    ,{<<"Media-Name">>, Media}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"progress">>}
                                    ,{<<"Msg-ID">>, NoopId}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
               ],
    do_send(CallId, CtrlQ, Commands),
    {'ok', NoopId};
send(CallId, CtrlQ, Code, Cause, 'undefined') ->
    NoopId = couch_mgr:get_uuid(),
    Commands = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                   ,{<<"Msg-ID">>, NoopId}
                                   ,{<<"Call-ID">>, CallId}
                                  ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"respond">>}
                                    ,{<<"Msg-ID">>, NoopId}
                                    ,{<<"Response-Code">>, Code}
                                    ,{<<"Response-Message">>, Cause}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
               ],
    do_send(CallId, CtrlQ, Commands),
    {'ok', NoopId};
send(CallId, CtrlQ, Code, Cause, Media) ->
    NoopId = couch_mgr:get_uuid(),
    Commands = [wh_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                   ,{<<"Msg-ID">>, NoopId}
                                   ,{<<"Call-ID">>, CallId}
                                  ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"respond">>}
                                    ,{<<"Response-Code">>, Code}
                                    ,{<<"Response-Message">>, Cause}
                                    ,{<<"Msg-ID">>, NoopId}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"play">>}
                                    ,{<<"Media-Name">>, Media}
                                    ,{<<"Msg-ID">>, NoopId}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
                ,wh_json:from_list([{<<"Application-Name">>, <<"progress">>}
                                    ,{<<"Msg-ID">>, NoopId}
                                    ,{<<"Call-ID">>, CallId}
                                   ])
               ],
    do_send(CallId, CtrlQ, Commands),
    {'ok', NoopId}.

-spec do_send(ne_binary(), ne_binary(), wh_json:objects()) -> 'ok'.
do_send(CallId, CtrlQ, Commands) ->
    Command = [{<<"Application-Name">>, <<"queue">>}
               ,{<<"Call-ID">>, CallId}
               ,{<<"Commands">>, Commands}
               ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
               | wh_api:default_headers(<<"call">>, <<"command">>, <<"call_response">>, <<"0.1.0">>)
              ],
    wh_amqp_worker:cast(Command
                        ,fun(C) ->
                                 {'ok', Payload} = wapi_dialplan:queue(C),
                                 wapi_dialplan:publish_action(CtrlQ, Payload)
                         end
                       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_default(whapps_call:call(), api_binary()) ->
                          {'ok', ne_binary()} |
                          {'error', 'no_response'}.
send_default(_Call, 'undefined') ->
    {'error', 'no_response'};
send_default(Call, Cause) ->
    lager:debug("attempting to send default response for ~s", [Cause]),
    Response = get_response(Cause, Call),
    send(whapps_call:call_id(Call)
         ,whapps_call:control_queue(Call)
         ,wh_json:get_value(<<"Code">>, Response)
         ,wh_json:get_value(<<"Message">>, Response)
         ,wh_json:get_value(<<"Media">>, Response)
        ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% returns the configured response proplist
%% @end
%%--------------------------------------------------------------------
-spec get_response(ne_binary(), whapps_call:call()) -> wh_proplist() | 'undefined'.
get_response(Cause, Call) ->
    case default_response(Call, Cause) of
        'undefined' -> whapps_config:get(?CALL_RESPONSE_CONF, Cause);
        Else -> whapps_config:get(?CALL_RESPONSE_CONF, Cause, wh_json:from_list(Else))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% returns the default action given the error
%% @end
%%--------------------------------------------------------------------
-spec default_response(whapps_call:call(), ne_binary()) -> 'undefined' | wh_proplist().
default_response(_Call, <<"RESPONSE_TO_STATUS_ENQUIRY">>) -> 'undefined';
default_response(_Call, <<"FACILITY_NOT_SUBSCRIBED">>) -> 'undefined';
default_response(_Call, <<"INVALID_MSG_UNSPECIFIED">>) -> 'undefined';

default_response(_Call, <<"INVALID_CALL_REFERENCE">>) -> 'undefined';
default_response(_Call, <<"CALL_AWARDED_DELIVERED">>) -> 'undefined';
default_response(_Call, <<"ACCESS_INFO_DISCARDED">>) -> 'undefined';
default_response(_Call, <<"MESSAGE_TYPE_NONEXIST">>) -> 'undefined';
default_response(_Call, <<"CHANNEL_UNACCEPTABLE">>) -> 'undefined';
default_response(_Call, <<"CHAN_NOT_IMPLEMENTED">>) -> 'undefined';

default_response(_Call, <<"INVALID_IE_CONTENTS">>) -> 'undefined';
default_response(_Call, <<"USER_NOT_REGISTERED">>) -> 'undefined';
default_response(_Call, <<"SERVICE_UNAVAILABLE">>) -> 'undefined';

default_response(_Call, <<"ATTENDED_TRANSFER">>) -> 'undefined';
default_response(_Call, <<"ALLOTTED_TIMEOUT">>) -> 'undefined';
default_response(_Call, <<"WRONG_CALL_STATE">>) -> 'undefined';
default_response(_Call, <<"MANAGER_REQUEST">>) -> 'undefined';
default_response(_Call, <<"SYSTEM_SHUTDOWN">>) -> 'undefined';

default_response(_Call, <<"PROTOCOL_ERROR">>) -> 'undefined';
default_response(_Call, <<"USER_CHALLENGE">>) -> 'undefined';
default_response(_Call, <<"BLIND_TRANSFER">>) -> 'undefined';
default_response(_Call, <<"WRONG_MESSAGE">>) -> 'undefined';
default_response(_Call, <<"INTERWORKING">>) -> 'undefined';
default_response(_Call, <<"UNSPECIFIED">>) -> 'undefined';
default_response(_Call, <<"IE_NONEXIST">>) -> 'undefined';
default_response(_Call, <<"PICKED_OFF">>) -> 'undefined';
default_response(_Call, <<"PRE_EMPTED">>) -> 'undefined';
default_response(_Call, <<"LOSE_RACE">>) -> 'undefined';
default_response(_Call, <<"CRASH">>) -> 'undefined';

default_response(Call, <<"UNALLOCATED_NUMBER">>) ->
    [{<<"Code">>, <<"404">>}
     ,{<<"Message">>, <<"No route to destination">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-can_not_be_completed_as_dialed">>, Call)}
    ];
default_response(_Call, <<"NO_ROUTE_TRANSIT_NET">>) ->
    [{<<"Code">>, <<"404">>}
     ,{<<"Message">>, <<"Invalid number">>}
    ];
default_response(Call, <<"NO_ROUTE_DESTINATION">>) ->
    [{<<"Code">>, <<"404">>}
     ,{<<"Message">>, <<"No route to destination">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-can_not_be_completed_as_dialed">>, Call)}
    ];
default_response(_Call, <<"NORMAL_CLEARING">>) ->
    [{<<"Media">>, <<"tone_stream://%(500,500,480,620);loops=25">>}];
default_response(_Call, <<"USER_BUSY">>) ->
    [{<<"Code">>, <<"486">>}
     ,{<<"Message">>, <<"Number busy">>}
     ,{<<"Media">>, <<"tone_stream://%(500,500,480,620);loops=25">>}
    ];
default_response(_Call, <<"NO_USER_RESPONSE">>) ->
    [{<<"Code">>, <<"408">>}
     ,{<<"Message">>, <<"No response">>}
     ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(_Call, <<"NO_ANSWER">>) ->
    [{<<"Code">>, <<"480">>}
     ,{<<"Message">>, <<"No answer">>}
    ];
default_response(_Call, <<"SUBSCRIBER_ABSENT">>) ->
    [{<<"Code">>, <<"480">>}
     ,{<<"Message">>, <<"Subscriber absent">>}
    ];
default_response(_Call, <<"CALL_REJECTED">>) ->
    [{<<"Code">>, <<"603">>}
     ,{<<"Message">>, <<"Call Rejected">>}
     ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(_Call, <<"NUMBER_CHANGED">>) ->
    [{<<"Code">>, <<"410">>}
     ,{<<"Message">>, <<"Number changed">>}
    ];
default_response(_Call, <<"REDIRECTION_TO_NEW_DESTINATION">>) ->
    [{<<"Code">>, <<"410">>}
     ,{<<"Message">>, <<"Redirection to new destination">>}
    ];
default_response(Call, <<"EXCHANGE_ROUTING_ERROR">>) ->
    [{<<"Code">>, <<"483">>}
     ,{<<"Message">>, <<"Exchange routing error">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-facility_trouble">>, Call)}
    ];
default_response(Call, <<"DESTINATION_OUT_OF_ORDER">>) ->
    [{<<"Code">>, <<"502">>}
     ,{<<"Message">>, <<"Destination out of order">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-can_not_be_completed_as_dialed">>, Call)}
    ];
default_response(Call, <<"INVALID_NUMBER_FORMAT">>) ->
    [{<<"Code">>, <<"484">>}
     ,{<<"Message">>, <<"Invalid number format">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-can_not_be_completed_as_dialed">>, Call)}
    ];
default_response(_Call, <<"FACILITY_REJECTED">>) ->
    [{<<"Code">>, <<"510">>}
     ,{<<"Message">>, <<"Facility rejected">>}
    ];
default_response(_Call, <<"NORMAL_UNSPECIFIED">>) ->
    [{<<"Code">>, <<"480">>}
     ,{<<"Message">>, <<"Normal unspecified">>}
    ];
default_response(_Call, <<"NORMAL_CIRCUIT_CONGESTION">>) ->
    [{<<"Code">>, <<"503">>}
     ,{<<"Message">>, <<"Normal circuit congestion">>}
    ];
default_response(_Call, <<"NETWORK_OUT_OF_ORDER">>) ->
    [{<<"Code">>, <<"503">>}
     ,{<<"Message">>, <<"Network out of order">>}
    ];
default_response(Call, <<"NORMAL_TEMPORARY_FAILURE">>) ->
    [{<<"Code">>, <<"503">>}
     ,{<<"Message">>, <<"Normal temporary failure">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-can_not_be_completed_at_this_time">>, Call)}
    ];
default_response(_Call, <<"SWITCH_CONGESTION">>) ->
    [{<<"Code">>, <<"503">>}
     ,{<<"Message">>, <<"Switch congestion">>}
    ];
default_response(_Call, <<"REQUESTED_CHAN_UNAVAIL">>) ->
    [{<<"Code">>, <<"503">>}
     ,{<<"Message">>, <<"Requested channel unavailable">>}
    ];
default_response(_Call, <<"OUTGOING_CALL_BARRED">>) ->
    [{<<"Code">>, <<"403">>}
     ,{<<"Message">>, <<"Outgoing call barred">>}
    ];
default_response(_Call, <<"INCOMING_CALL_BARRED">>) ->
    [{<<"Code">>, <<"403">>}
     ,{<<"Message">>, <<"Incoming call barred">>}
    ];
default_response(_Call, <<"BEARERCAPABILITY_NOTAUTH">>) ->
    [{<<"Code">>, <<"403">>}
     ,{<<"Message">>, <<"Bearer capability not authorized">>}
    ];
default_response(_Call, <<"BEARERCAPABILITY_NOTAVAIL">>) ->
    [{<<"Code">>, <<"403">>}
     ,{<<"Message">>, <<"Bearer capability not presently available">>}
    ];
default_response(_Call, <<"BEARERCAPABILITY_NOTIMPL">>) ->
    [{<<"Code">>, <<"488">>}
     ,{<<"Message">>, <<"Bearer capability not implemented">>}
    ];
default_response(_Call, <<"FACILITY_NOT_IMPLEMENTED">>) ->
    [{<<"Code">>, <<"501">>}
     ,{<<"Message">>, <<"Facility not implemented">>}
    ];
default_response(Call, <<"SERVICE_NOT_IMPLEMENTED">>) ->
    [{<<"Code">>, <<"501">>}
     ,{<<"Message">>, <<"Service not implemented">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-facility_trouble">>, Call)}
    ];
default_response(Call, <<"INCOMPATIBLE_DESTINATION">>) ->
    [{<<"Code">>, <<"488">>}
     ,{<<"Message">>, <<"Incompatible destination">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-facility_trouble">>, Call)}
    ];
default_response(Call, <<"MANDATORY_IE_MISSING">>) ->
    [{<<"Code">>, <<"400">>}
     ,{<<"Message">>, <<"Mandatory informatin missing">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-facility_trouble">>, Call)}
    ];
default_response(_Call, <<"RECOVERY_ON_TIMER_EXPIRE">>) ->
    [{<<"Code">>, <<"504">>}
     ,{<<"Message">>, <<"Recovery on timer expire">>}
     ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(Call, <<"MANDATORY_IE_LENGTH_ERROR">>) ->
    [{<<"Code">>, <<"400">>}
     ,{<<"Message">>, <<"Mandatory informatin missing">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-facility_trouble">>, Call)}
    ];
default_response(_Call, <<"ORIGINATOR_CANCEL">>) ->
    [{<<"Code">>, <<"487">>}
     ,{<<"Message">>, <<"Originator cancel">>}
    ];
default_response(_Call, <<"MEDIA_TIMEOUT">>) ->
    [{<<"Code">>, <<"504">>}
     ,{<<"Message">>, <<"Media timeout">>}
     ,{<<"Media">>, <<"tone_stream://%(250,250,480,620);loops=25">>}
    ];
default_response(Call, <<"PROGRESS_TIMEOUT">>) ->
    [{<<"Code">>, <<"486">>}
     ,{<<"Message">>, <<"Progress timeout">>}
     ,{<<"Media">>, wh_media_util:get_prompt(<<"fault-can_not_be_completed_at_this_time">>, Call)}
    ].
