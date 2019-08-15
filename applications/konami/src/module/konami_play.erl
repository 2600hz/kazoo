%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Play a media file
%%% Data = {
%%%   "id":"media_id"
%%%   ,"leg":["self", "peer", "both"] // which leg to play the media to
%%% }
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_play).

-export([handle/2
        ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()}.
handle(Data, Call) ->
    AccountId = kapps_call:account_id(Call),
    Path = kz_doc:id(Data),
    case kz_media_util:media_path(Path, AccountId) of
        'undefined' -> lager:info("invalid data in the play metaflow");
        Media -> play(Data, Call, Media)
    end,
    {'continue', Call}.

-spec play(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> any().
play(Data, Call, Media) ->
    case kz_json:is_false(<<"answer">>, Data) of
        'true' -> 'ok';
        'false' ->
            kapps_call_command:answer_now(Call),
            timer:sleep(100)
    end,
    lager:info("playing media ~s", [Media]),

    PlayCommand = kapps_call_command:play_command(Media, ?ANY_DIGIT, leg(Data, Call), Call),
    kapps_call_command:send_command(kz_json:set_value(<<"Insert-At">>, <<"now">>, PlayCommand)
                                   ,Call
                                   ).

-spec leg(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
leg(Data, Call) ->
    case kz_json:get_value(<<"dtmf_leg">>, Data) =:= kapps_call:call_id(Call) of
        'true' ->
            play_on(kz_json:get_value(<<"leg">>, Data, <<"both">>), 'a');
        'false' ->
            play_on(kz_json:get_value(<<"leg">>, Data, <<"both">>), 'b')
    end.

-spec play_on(kz_term:ne_binary(), 'a' | 'b') -> kz_term:ne_binary().
play_on(<<"both">>, _) -> <<"Both">>;
play_on(<<"self">>, 'a') -> <<"A">>;
play_on(<<"self">>, 'b') -> <<"B">>;
play_on(<<"peer">>, 'a') -> <<"B">>;
play_on(<<"peer">>, 'b') -> <<"A">>.

-spec number_builder(kz_json:object()) -> kz_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'play' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'play'? ", "~d"),

    K = [<<"numbers">>, kz_term:to_binary(Number)],

    case number_builder_check(kz_json:get_value(K, DefaultJObj)) of
        'undefined' -> kz_json:delete_key(K, DefaultJObj);
        NumberJObj -> kz_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(kz_term:api_object()) -> kz_term:api_object().
number_builder_check('undefined') ->
    number_builder_media(kz_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [kz_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(kz_json:object(), string()) -> kz_term:api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_media(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_media(kz_json:object()) -> kz_json:object().
number_builder_media(NumberJObj) ->
    {'ok', [Media]} = io:fread("What media file would you like to play? ", "~s"),
    number_builder_leg(NumberJObj, kz_term:to_binary(Media)).

-spec number_builder_leg(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
number_builder_leg(NumberJObj, Media) ->
    {'ok', [Leg]} = io:fread("On what leg of the call ('self', 'peer', or 'both')? ", "~s"),
    metaflow_jobj(NumberJObj, Media, kz_term:to_binary(Leg)).

-spec metaflow_jobj(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
metaflow_jobj(NumberJObj, Media, Leg) ->
    kz_json:set_values([{<<"module">>, <<"play">>}
                       ,{<<"data">>, play_data(Media, Leg)}
                       ]
                      ,NumberJObj
                      ).

-spec play_data(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
play_data(Media, Leg) ->
    kz_json:from_list([{<<"id">>, Media}
                      ,{<<"leg">>, Leg}
                      ]).
