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

-include("ecallmgr.hrl").
-include("whistle_amqp.hrl").
-include("whistle_api.hrl").

-define(APPNAME, <<"ecallmgr.call.cdr">>).
-define(APPVER, <<"0.3.1">>).
-define(EVENT_CAT, <<"call_detail">>).
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
			    ,{<<"ecallmgr">>, <<"Custom-Channel-Vars">>}
			   ]).
-define(FS_TO_WHISTLE_OUTBOUND_MAP, [{<<"variable_sip_cid_type">>, <<"Caller-ID-Type">>}]).

-spec(new_cdr/3 :: (UUID :: binary(), AmqpHost :: binary(), EvtProp :: proplist()) -> no_return()).
new_cdr(UUID, AmqpHost, EvtProp) ->
    CDRJson = create_cdr(EvtProp),
    format_log(info, "CALL_CDR(~p): ~s~n", [UUID, CDRJson]),
    amqp_util:callevt_publish(AmqpHost, UUID, CDRJson, cdr).

-spec(create_cdr/1 :: (EvtProp :: proplist()) -> iolist()).
create_cdr(EvtProp) ->
    DefProp = whistle_api:default_headers(<<>>, ?EVENT_CAT, ?EVENT_NAME, ?APPNAME, ?APPVER),
    ApiProp0 = add_values(?FS_TO_WHISTLE_MAP, DefProp, EvtProp),
    ApiProp1 = case get_value(<<"direction">>, ApiProp0) of
		   <<"outbound">> -> add_values(?FS_TO_WHISTLE_OUTBOUND_MAP, ApiProp0, EvtProp);
		   _ -> ApiProp0
	       end,
    {ok, JSON} = whistle_api:call_cdr(ApiProp1),
    JSON.

-spec(add_values/3 :: (Mappings :: proplist(), BaseProp :: proplist(), ChannelProp :: proplist()) -> proplist()).
add_values(Mappings, BaseProp, ChannelProp) ->
    lists:foldl(fun({<<"ecallmgr">>, <<"Custom-Channel-Vars">>=WK}, WApi) ->
			[{WK, {struct, ecallmgr_util:custom_channel_vars(ChannelProp)}} | WApi];
		    ({FSKey, WK}, WApi) ->
			case get_value(FSKey, ChannelProp) of
			    undefined -> WApi;
			    V -> [{WK, whistle_util:to_binary(V)} | WApi]
			end
		end, BaseProp, Mappings).
