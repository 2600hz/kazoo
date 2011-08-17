%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive the CHANNEL_HANGUP_COMPLETE event and generate a CDR, putting
%%% it on the Messaging Bus
%%% queue
%%% @end
%%% Created : 9 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_cdr).

-export([new_cdr/2]).

-include("ecallmgr.hrl").

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

-spec new_cdr/2 :: (UUID, EvtProp) -> ok when
      UUID :: binary(),
      EvtProp :: proplist().
new_cdr(UUID, EvtProp) ->
    CDRJson = create_cdr(EvtProp),
    ?LOG_SYS(UUID, "CDR to send: ~s", [CDRJson]),
    amqp_util:callevt_publish(UUID, CDRJson, cdr).

-spec create_cdr/1 :: (EvtProp) -> iolist() when
      EvtProp :: proplist().
create_cdr(EvtProp) ->
    DefProp = wh_api:default_headers(<<>>, ?EVENT_CAT, ?EVENT_NAME, ?APP_NAME, ?APP_VERSION),
    ApiProp0 = add_values(?FS_TO_WHISTLE_MAP, DefProp, EvtProp),
    ApiProp1 = case props:get_value(<<"direction">>, ApiProp0) of
		   <<"outbound">> -> add_values(?FS_TO_WHISTLE_OUTBOUND_MAP, ApiProp0, EvtProp);
		   _ -> ApiProp0
	       end,
    {ok, JSON} = wh_api:call_cdr(ApiProp1),
    JSON.

-spec add_values/3 :: (Mappings, BaseProp, ChannelProp) -> proplist() when
      Mappings :: proplist(),
      BaseProp :: proplist(),
      ChannelProp :: proplist().
add_values(Mappings, BaseProp, ChannelProp) ->
    lists:foldl(fun({<<"ecallmgr">>, <<"Custom-Channel-Vars">>=WK}, WApi) ->
                        [{WK, {struct, ecallmgr_util:custom_channel_vars(ChannelProp)}} | WApi];
                   ({<<"Event-Date-Timestamp">>=FSKey, WK}, WApi) ->
                        case props:get_value(FSKey, ChannelProp) of
                            undefined -> WApi;
                            V -> VUnix =  wh_util:unix_seconds_to_gregorian_seconds(wh_util:microseconds_to_seconds(V)),
                                 [{WK, wh_util:to_binary(VUnix)} | WApi]
                        end;
                   ({FSKey, WK}, WApi) ->
                        case props:get_value(FSKey, ChannelProp) of
                            undefined -> WApi;
                            V -> [{WK, wh_util:to_binary(V)} | WApi]
                        end
                end, BaseProp, Mappings).
