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

-define(POST_ANSWER_DELAY, whapps_config:get_integer(?CF_CONFIG_CAT, <<"post_answer_delay">>, 100)).

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
    Call2 =
        case wh_media_util:media_path(Path, AccountId) of
            'undefined' ->
                lager:info("invalid data in the play callflow"),
                Call;
            Media ->
                NoopId = play(Data, Call, Media),
                {'ok', Call1} = cf_util:wait_for_noop(Call, NoopId),
                cf_exe:set_call(Call1),
                Call1
    end,
    cf_exe:continue(Call2).

-spec play(wh_json:object(), whapps_call:call(), ne_binary()) -> any().
play(Data, Call, Media) ->
    case wh_json:is_false(<<"answer">>, Data) of
        'true' -> 'ok';
        'false' ->
            whapps_call_command:answer(Call),
            timer:sleep(?POST_ANSWER_DELAY)
    end,
    lager:info("playing media ~s", [Media]),
    whapps_call_command:play(Media, Call).
