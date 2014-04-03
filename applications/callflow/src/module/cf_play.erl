%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
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
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = whapps_call:account_id(Call),
    Path = wh_json:get_value(<<"id">>, Data),
    case wh_media_util:media_path(Path, AccountId) of
        'undefined' -> lager:info("invalid data in the play callflow");
        Media -> play(Data, Call, Media)
    end,
    cf_exe:continue(Call).

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
