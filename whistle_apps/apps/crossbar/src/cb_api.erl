%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Builder and validator, much like wh_api.erl, of the AMQP APIs
%%% exposed by this WhApp
%%% @end
%%% Created :  31 August 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_api).

-export([new_document_event/1]).

-export([new_document_event_v/1]).

-include("../include/crossbar.hrl").

%%--------------------------------------------------------------------
%% @doc New Document event - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec new_document_event/1 :: (Prop) -> tuple(ok, iolist()) | tuple(error, string()) when
      Prop :: proplist() | json_object().
new_document_event({struct, Prop}) ->
    new_document_event(Prop);
new_document_event(Prop) ->
    case new_document_event_v(Prop) of
	true -> wh_api:build_message(Prop, ?DOCUMENT_EVENT_HEADERS, ?OPTIONAL_DOCUMENT_EVENT_HEADERS);
	false -> {error, "Proplist failed validation for new_voicemail"}
    end.

-spec new_document_event_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
new_document_event_v({struct, Prop}) ->
    new_document_event_v(Prop);
new_document_event_v(Prop) ->
    wh_api:validate(Prop, ?DOCUMENT_EVENT_HEADERS, ?DOCUMENT_EVENT_VALUES, ?DOCUMENT_EVENT_TYPES).
