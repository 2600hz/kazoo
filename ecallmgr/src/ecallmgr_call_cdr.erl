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

-define(APPNAME, <<"ecallmgr.call.cdr">>).
-define(APPVER, <<"0.2.0">>).
-define(EVENT_CAT, <<"call-detail">>).
-define(EVENT_NAME, <<"cdr">>).

-define(FS_TO_WHISTLE_MAP, [{<<"FreeSWITCH-Hostname">>, <<"Handling-Server-Name">>}
			    ,{<<"Hangup-Cause">>, <<"Hangup-Cause">>}
			    ,{<<"Unique-ID">>, <<"Call-ID">>}
			    ,{<<"Event-Date-Timestamp">>, <<"Timestamp">>}
			    ,{<<"Call-Direction">>, <<"Call-Direction">>}
			    ,{<<"variable_switch_r_sdp">>, <<"SDP">>}
			    ,{<<"variable_sip_to_uri">>, <<"To-Uri">>}
			    ,{<<"variable_sip_from_uri">>, <<"From-Uri">>}
			    ,{<<"Caller-Caller-ID-Name">>, <<"Caller-ID-Name">>}
			    ,{<<"Caller-Caller-ID-Number">>, <<"Caller-ID-Number">>}
			    ,{<<"Caller-Callee-ID-Name">>, <<"Callee-ID-Name">>}
			    ,{<<"Caller-Callee-ID-Number">>, <<"Callee-ID-Number">>}
			    ,{<<"Other-Leg-Unique-ID">>, <<"Other-Leg-Call-ID">>}
			    
			   ]).

-spec(new_cdr/3 :: (UUID :: binary(), AmqpHost :: binary(), EvtProp :: proplist()) -> no_return()).
new_cdr(UUID, AmqpHost, EvtProp) ->
    CDRJson = create_cdr(EvtProp),
    amqp_util:callevt_publish(AmqpHost, UUID, CDRJson, <<"application/json">>).

create_cdr(Prop) ->
    DefProp = whistle_api:default_headers(<<>>, ?EVENT_CAT, ?EVENT_NAME, ?APPNAME, ?APPVER),
    to_be_continued.
