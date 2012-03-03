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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> ok.
handle(Data, Call) ->
    AccountId = whapps_call:account_id(Call),
    Media = case wh_json:get_value(<<"id">>, Data) of
                <<"/system_media", _/binary>> = Path -> Path;
                <<"system_media", _/binary>> = Path -> Path;
                <<"local_stream://",_/binary>> = Path -> Path;
                Path ->
                    ?LOG("prepending media ID with /~s/", [AccountId]),
                    <<$/, (wh_util:to_binary(AccountId))/binary, $/, Path/binary>>
            end,
    case wh_json:is_false(<<"answer">>, Data) of
        true -> ok;
        false -> whapps_call_command:answer(Call)
    end,
    ?LOG("playing media ~s", [Media]),
    whapps_call_command:b_play(Media, Call),
    cf_exe:continue(Call).
