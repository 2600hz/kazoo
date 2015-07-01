%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%% "data":{"id":"doc_id"}
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
    Path = wh_doc:id(Data),
    case wh_media_util:media_path(Path, AccountId) of
        'undefined' ->
            lager:info("invalid data in the play callflow"),
            cf_exe:continue(Call);
        Media ->
            NoopId = play(Data, Call, Media),
            handle_noop_recv(Call, cf_util:wait_for_noop(Call, NoopId))
    end.

-spec handle_noop_recv(whapps_call:call(), {'ok', whapps_call:call()} | {'error', _}) -> 'ok'.
handle_noop_recv(_OldCall, {'ok', Call}) ->
    cf_exe:set_call(Call),
    cf_exe:continue(Call);
handle_noop_recv(Call, {'error', 'channel_hungup'}) ->
    cf_exe:hard_stop(Call);
handle_noop_recv(Call, {'error', _E}) ->
    lager:debug("failure playing: ~p", [_E]),
    cf_exe:continue(Call).

-spec play(wh_json:object(), whapps_call:call(), ne_binary()) -> ne_binary().
play(Data, Call, Media) ->
    case wh_json:is_false(<<"answer">>, Data) of
        'true' -> 'ok';
        'false' ->
            whapps_call_command:answer(Call),
            timer:sleep(?POST_ANSWER_DELAY)
    end,
    lager:info("playing media ~s", [Media]),
    whapps_call_command:play(Media, Call).
