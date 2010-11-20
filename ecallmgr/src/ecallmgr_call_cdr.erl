%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive the CHANNEL_HANGUP_COMPLETE event and generate a CDR, putting
%%% it on the Messaging Bus
%%% queue
%%% @end
%%% Created : 9 Nov 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_cdr).

-export([new_cdr/3]).

-import(logger, [log/2, format_log/3]).
-import(props, [get_value/2, get_value/3]).

-type proplist() :: list(tuple(binary(), (binary() | list() | fun()) )).

-include("whistle_amqp.hrl").

-define(APPNAME, <<"ecallmgr.call.cdr">>).
-define(APPVER, <<"0.2.0">>).
-define(EVENT_CAT, <<"call-detail">>).
-define(EVENT_NAME, <<"cdr">>).

-define(FS_TO_WHISTLE_MAP, [{<<"FreeSWITCH-Hostname">>, <<"Handling-Server-Name">>}
			    ,{<<"Hangup-Cause">>, <<"Hangup-Cause">>}
			    ,{<<"Unique-ID">>, <<"Call-ID">>}
			    ,{<<"Event-Date-Timestamp">>, <<"Timestamp">>}
			    ,{<<"Call-Direction">>, <<"Call-Direction">>}
			    ,{<<"variable_switch_r_sdp">>, <<"Remote-SDP">>}
			    ,{<<"variable_sip_local_sdp_str">>, <<"Local-SDP">>}
			    ,{<<"variable_sip_to_uri">>, <<"To-Uri">>}
			    ,{<<"variable_sip_from_uri">>, <<"From-Uri">>}
			    ,{<<"Caller-Caller-ID-Name">>, <<"Caller-ID-Name">>}
			    ,{<<"Caller-Caller-ID-Number">>, <<"Caller-ID-Number">>}
			    ,{<<"Caller-Callee-ID-Name">>, <<"Callee-ID-Name">>}
			    ,{<<"Caller-Callee-ID-Number">>, <<"Callee-ID-Number">>}
			    ,{<<"Other-Leg-Unique-ID">>, <<"Other-Leg-Call-ID">>}
			    ,{<<"variable_sip_user_agent">>, <<"User-Agent">>}
			    ,{<<"variable_duration">>, <<"Duration-Seconds">>}
			    ,{<<"variable_billsec">>, <<"Billing-Seconds">>}
			    ,{<<"variable_progresssec">>, <<"Ringing-Seconds">>}
			    ,{<<"variable_digits_dialed">>, <<"Digits-Dialed">>}
			   ]).
-define(FS_TO_WHISTLE_OUTBOUND_MAP, [{<<"variable_sip_cid_type">>, <<"Caller-ID-Type">>}]).

-spec(new_cdr/3 :: (UUID :: binary(), AmqpHost :: binary(), EvtProp :: proplist()) -> no_return()).
new_cdr(UUID, AmqpHost, EvtProp) ->
    CDRJson = create_cdr(EvtProp),
    amqp_util:callmgr_publish(AmqpHost, CDRJson, <<"application/json">>, <<?KEY_CALL_CDR/binary, UUID/binary>>).

-spec(create_cdr/1 :: (EvtProp :: proplist()) -> iolist()).
create_cdr(EvtProp) ->
    DefProp = whistle_api:default_headers(<<>>, ?EVENT_CAT, ?EVENT_NAME, ?APPNAME, ?APPVER),
    ApiProp = lists:foldl(fun({FSKey, WK}, WApi) ->
				  case get_value(FSKey, EvtProp) of
				      undefined -> WApi;
				      V -> [{WK, V} | WApi]
				  end
			  end, DefProp, ?FS_TO_WHISTLE_MAP),
    {ok, JSON} = whistle_api:call_cdr(ApiProp),
    JSON.
