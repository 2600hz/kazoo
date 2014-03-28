%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Say something
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_play).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) -> {'continue', whapps_call:call()}.
handle(Data, Call) ->
    AccountId = whapps_call:account_id(Call),
    Path = wh_json:get_value(<<"id">>, Data),
    case wh_media_util:media_path(Path, AccountId) of
        'undefined' -> lager:info("invalid data in the play metaflow");
        Media -> play(Data, Call, Media)
    end,
    {'continue', Call}.

-spec play(wh_json:object(), whapps_call:call(), ne_binary()) -> any().
play(Data, Call, Media) ->
    case wh_json:is_false(<<"answer">>, Data) of
        'true' -> 'ok';
        'false' ->
            whapps_call_command:answer(Call),
            timer:sleep(100)
    end,
    lager:info("playing media ~s", [Media]),
    _ = whapps_call_command:b_play(Media, Call).
