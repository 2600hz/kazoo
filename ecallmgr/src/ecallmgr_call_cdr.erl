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

-define(APPNAME, <<"ecallmgr.call.event">>).
-define(APPVER, <<"0.2.0">>).
-define(EVENT_CAT, <<"Call-Detail">>).
-define(EVENT_NAME, <<"CDR">>).

-spec(new_cdr/3 :: (UUID :: binary(), AmqpHost :: binary(), EvtProp :: proplist()) -> no_return()).
new_cdr(UUID, AmqpHost, EvtProp) ->
    CDRJson = create_cdr(EvtProp),
    amqp_util:callevt_publish(AmqpHost, UUID, CDRJson, <<"application/json">>).

create_cdr(Prop) ->
    DefProp = whistle_api:default_headers(<<>>, ?EVENT_CAT, ?EVENT_NAME, ?APPNAME, ?APPVER),
    to_be_continued.
