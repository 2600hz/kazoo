%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Receive the CHANNEL_HANGUP_COMPLETE event and generate a CDR, putting
%%% it on the Messaging Bus
%%% queue
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_call_cdr).

-export([new_cdr/2]).

-include("ecallmgr.hrl").

-define(EVENT_CAT, <<"call_detail">>).
-define(EVENT_NAME, <<"cdr">>).

-type fs_to_whistle_map() :: [{ne_binary() | [ne_binary(),...], ne_binary()},...].
-define(FS_TO_WHISTLE_MAP, [{<<"FreeSWITCH-Hostname">>, <<"Handling-Server-Name">>}
                            ,{<<"Hangup-Cause">>, <<"Hangup-Cause">>}
                            ,{<<"Unique-ID">>, <<"Call-ID">>}
                            ,{<<"Event-Date-Timestamp">>, <<"Timestamp">>}
                            ,{<<"Call-Direction">>, <<"Call-Direction">>}
                            ,{<<"variable_switch_r_sdp">>, <<"Remote-SDP">>}
                            ,{<<"variable_sip_local_sdp_str">>, <<"Local-SDP">>}
                            ,{<<"variable_sip_to_uri">>, <<"To-Uri">>}
                            ,{<<"variable_sip_from_uri">>, <<"From-Uri">>}
                            ,{[<<"variable_effective_caller_id_number">>, <<"Caller-Caller-ID-Number">>], <<"Caller-ID-Number">>}
                            ,{[<<"variable_effective_caller_id_name">>, <<"Caller-Caller-ID-Name">>], <<"Caller-ID-Name">>}
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

-spec new_cdr/2 :: (ne_binary(), proplist()) -> 'ok'.
new_cdr(UUID, EvtProp) ->
    put(callid, UUID),
    new_cdr(UUID, EvtProp, ecallmgr_call_events:has_channel_is_moving_flag(EvtProp)).
new_cdr(_UUID, _EvtProp, true) ->
    lager:debug("CDR has the channel_is_moving flag, supressing");
new_cdr(UUID, EvtProp, false) ->
    CDR = create_cdr(EvtProp),

    lager:debug("publising cdr: ~p", [CDR]),
    wapi_call:publish_cdr(UUID, CDR).

-spec create_cdr/1 :: (proplist()) -> proplist().
create_cdr(EvtProp) ->
    DefProp = wh_api:default_headers(<<>>, ?EVENT_CAT, ?EVENT_NAME, ?APP_NAME, ?APP_VERSION),
    ApiProp0 = add_values(?FS_TO_WHISTLE_MAP, DefProp, EvtProp),
    case props:get_value(<<"direction">>, ApiProp0) of
        <<"outbound">> -> add_values(?FS_TO_WHISTLE_OUTBOUND_MAP, ApiProp0, EvtProp);
        _ -> ApiProp0
    end.

-spec add_values/3 :: (fs_to_whistle_map(), proplist(), proplist()) -> proplist().
add_values(Mappings, BaseProp, ChannelProp) ->
    lists:foldl(fun({<<"ecallmgr">>, <<"Custom-Channel-Vars">>=WK}, WApi) ->
                        [{WK, wh_json:from_list(ecallmgr_util:custom_channel_vars(ChannelProp))} | WApi];

                   ({<<"Event-Date-Timestamp">>=FSKey, WK}, WApi) ->
                        case props:get_value(FSKey, ChannelProp) of
                            undefined -> WApi;
                            V -> VUnix =  wh_util:unix_seconds_to_gregorian_seconds(wh_util:microseconds_to_seconds(V)),
                                 [{WK, wh_util:to_binary(VUnix)} | WApi]
                        end;

                   ({FSKeys, WK}, WApi) when is_list(FSKeys) ->
                        case get_first_value(FSKeys, ChannelProp) of
                            undefined -> WApi;
                            V -> [{WK, V} | WApi]
                        end;

                   ({FSKey, WK}, WApi) ->
                        case props:get_value(FSKey, ChannelProp) of
                            undefined -> WApi;
                            V -> [{WK, wh_util:to_binary(V)} | WApi]
                        end
                end, BaseProp, Mappings).

-spec get_first_value/2 :: ([ne_binary(),...] | [], proplist()) -> ne_binary() | 'undefined'.
get_first_value([FSKey|T], ChannelProp) ->
    case props:get_value(FSKey, ChannelProp) of
        undefined -> get_first_value(T, ChannelProp);
        V -> wh_util:to_binary(V)
    end;
get_first_value([], _) -> undefined.
