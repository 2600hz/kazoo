%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Plays a media file to the caller.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`id'</dt>
%%%   <dd>Media ID.</dd>
%%% </dl>
%%%
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_play).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(POST_ANSWER_DELAY, kapps_config:get_integer(?CF_CONFIG_CAT, <<"post_answer_delay">>, 100)).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    Path = kz_json:get_ne_binary_value(<<"id">>, Data),
    case kz_media_util:media_path(Path, AccountId) of
        'undefined' ->
            lager:info("invalid data in the play callflow"),
            cf_exe:continue(Call);
        Media ->
            NoopResult = play(Data, Call, Media),
            handle_noop_recv(Call, NoopResult)
    end.

-spec handle_noop_recv(kapps_call:call(), {'ok', kapps_call:call()} | {'error', any()}) -> 'ok'.
handle_noop_recv(_OldCall, {'ok', Call}) ->
    cf_exe:set_call(Call),
    cf_exe:continue(Call);
handle_noop_recv(Call, {'error', 'channel_hungup'}) ->
    cf_exe:hard_stop(Call);
handle_noop_recv(Call, {'error', _E}) ->
    lager:debug("failure playing: ~p", [_E]),
    cf_exe:continue(Call).

-spec play(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) ->
                  {'ok', kapps_call:call()} |
                  {'error', 'channel_hungup' | kz_json:object()}.
play(Data, Call, Media) ->
    case kz_json:is_false(<<"answer">>, Data) of
        'true' -> 'ok';
        'false' ->
            kapps_call_command:answer(Call),
            timer:sleep(?POST_ANSWER_DELAY)
    end,
    lager:info("playing media ~s", [Media]),

    NoopId = kapps_call_command:noop_id(),
    Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ])
               ,play_command(Data, kapps_call:call_id_direct(Call), Media)
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Commands">>, Commands}
              ],
    kapps_call_command:send_command(Command, Call),
    cf_util:wait_for_noop(Call, NoopId).

-spec play_command(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
play_command(Data, CallId, Media) ->
    kz_json:from_list(
      [{<<"Application-Name">>, <<"play">>}
      ,{<<"Media-Name">>, Media}
      ,{<<"Terminators">>, kapps_call_command:play_terminators(kz_json:get_list_value(<<"terminators">>, Data))}
      ,{<<"Call-ID">>, CallId}
      ,{<<"Endless-Playback">>, kz_json:is_true(<<"endless_playback">>, Data)}
      ,{<<"Loop-Count">>, kz_json:get_integer_value(<<"loop_count">>, Data)}
      ]).
