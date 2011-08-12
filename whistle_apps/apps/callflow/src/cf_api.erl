%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Builder and validator, much like wh_api.erl, of the AMQP APIs
%%% exposed by this WhApp
%%% @end
%%% Created :  7 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_api).

-export([new_voicemail/1]).

-export([new_voicemail_v/1]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @doc New Voicemail Saved - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec new_voicemail/1 :: (Prop) -> tuple(ok, iolist()) | tuple(error, string()) when
      Prop :: proplist() | json_object().
new_voicemail({struct, Prop}) ->
    new_voicemail(Prop);
new_voicemail(Prop) ->
    case new_voicemail_v(Prop) of
	true -> wh_api:build_message(Prop, ?NEW_VOICEMAIL_HEADERS, ?OPTIONAL_NEW_VOICEMAIL_HEADERS);
	false -> {error, "Proplist failed validation for new_voicemail"}
    end.

-spec new_voicemail_v/1 :: (Prop) -> boolean() when
      Prop :: proplist() | json_object().
new_voicemail_v({struct, Prop}) ->
    new_voicemail_v(Prop);
new_voicemail_v(Prop) ->
    wh_api:validate(Prop, ?NEW_VOICEMAIL_HEADERS, ?NEW_VOICEMAIL_VALUES, ?NEW_VOICEMAIL_TYPES).
