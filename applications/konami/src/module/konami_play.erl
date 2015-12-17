%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Play a media file
%%% Data = {
%%%   "id":"media_id"
%%%   ,"leg":["self", "peer", "both"] // which leg to play the media to
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_play).

-export([handle/2
         ,number_builder/1
        ]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    AccountId = whapps_call:account_id(Call),
    Path = wh_doc:id(Data),
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
            whapps_call_command:answer_now(Call),
            timer:sleep(100)
    end,
    lager:info("playing media ~s", [Media]),

    PlayCommand = whapps_call_command:play_command(Media, ?ANY_DIGIT, leg(Data, Call), Call),
    whapps_call_command:send_command(
      wh_json:set_value(<<"Insert-At">>, <<"now">>, PlayCommand)
      ,Call
     ).

-spec leg(wh_json:object(), whapps_call:call()) -> ne_binary().
leg(Data, Call) ->
    case wh_json:get_value(<<"dtmf_leg">>, Data) =:= whapps_call:call_id(Call) of
        'true' ->
            play_on(wh_json:get_value(<<"leg">>, Data, <<"both">>), 'a');
        'false' ->
            play_on(wh_json:get_value(<<"leg">>, Data, <<"both">>), 'b')
    end.

-spec play_on(ne_binary(), 'a' | 'b') -> ne_binary().
play_on(<<"both">>, _) -> <<"Both">>;
play_on(<<"self">>, 'a') -> <<"A">>;
play_on(<<"self">>, 'b') -> <<"B">>;
play_on(<<"peer">>, 'a') -> <<"B">>;
play_on(<<"peer">>, 'b') -> <<"A">>.

-spec number_builder(wh_json:object()) -> wh_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'play' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'play'? ", "~d"),

    K = [<<"numbers">>, wh_util:to_binary(Number)],

    case number_builder_check(wh_json:get_value(K, DefaultJObj)) of
        'undefined' -> wh_json:delete_key(K, DefaultJObj);
        NumberJObj -> wh_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    number_builder_media(wh_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [wh_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(wh_json:object(), string()) -> api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_media(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_media(wh_json:object()) -> wh_json:object().
number_builder_media(NumberJObj) ->
    {'ok', [Media]} = io:fread("What media file would you like to play? ", "~s"),
    number_builder_leg(NumberJObj, wh_util:to_binary(Media)).

-spec number_builder_leg(wh_json:object(), ne_binary()) -> wh_json:object().
number_builder_leg(NumberJObj, Media) ->
    {'ok', [Leg]} = io:fread("On what leg of the call ('self', 'peer', or 'both')? ", "~s"),
    metaflow_jobj(NumberJObj, Media, wh_util:to_binary(Leg)).

-spec metaflow_jobj(wh_json:object(), ne_binary(), ne_binary()) -> wh_json:object().
metaflow_jobj(NumberJObj, Media, Leg) ->
    wh_json:set_values([{<<"module">>, <<"play">>}
                        ,{<<"data">>, play_data(Media, Leg)}
                       ], NumberJObj).

-spec play_data(ne_binary(), ne_binary()) -> wh_json:object().
play_data(Media, Leg) ->
    wh_json:from_list([{<<"id">>, Media}
                       ,{<<"leg">>, Leg}
                      ]).
