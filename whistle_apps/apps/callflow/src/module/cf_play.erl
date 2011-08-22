%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_play).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [b_play/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId, account_id=AccountId}=Call) ->
    put(callid, CallId),
    Media = case wh_json:get_value(<<"id">>, Data) of
                <<"/system_media", _/binary>> = Path -> Path;
                <<"system_media", _/binary>> = Path -> Path;
                Path ->
                    ?LOG("prepending media ID with /~s/", [AccountId]),
                    <<$/, (wh_util:to_binary(AccountId))/binary, $/, Path/binary>>
            end,
    ?LOG("playing media ~s", [Media]),
    b_play(Media, wh_json:get_value(<<"terminators">>, Data), Call),
    CFPid ! {continue}.
