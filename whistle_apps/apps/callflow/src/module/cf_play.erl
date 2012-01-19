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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (json_object(), #cf_call{}) -> ok.
handle(Data, #cf_call{account_id=AccountId}=Call) ->
    Media = case wh_json:get_value(<<"id">>, Data) of
                <<"/system_media", _/binary>> = Path -> Path;
                <<"system_media", _/binary>> = Path -> Path;
                <<"local://",_/binary>> = Path -> Path;
                Path ->
                    ?LOG("prepending media ID with /~s/", [AccountId]),
                    <<$/, (wh_util:to_binary(AccountId))/binary, $/, Path/binary>>
            end,
    case wh_json:is_false(<<"answer">>, Data) of
        true -> ok;
        false -> cf_call_command:answer(Call)
    end,
    ?LOG("playing media ~s", [Media]),
    cf_call_command:b_play(Media, wh_json:get_value(<<"terminators">>, Data), Call),
    cf_exe:continue(Call).
