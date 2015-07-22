%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Put the call on hold
%%% Data = {
%%%   "moh":"media_id"
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(konami_hold).

-export([handle/2
         ,number_builder/1
        ]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    AMOH = wh_json:get_value(<<"moh_aleg">>, Data),
    AMOHToPlay = wh_media_util:media_path(AMOH, Call),
    BMOH = wh_json:get_value(<<"moh_bleg">>, Data, AMOH),
    BMOHToPlay = wh_media_util:media_path(BMOH, Call),
    Unholdkey = wh_json:get_value(<<"unhold_key">>, Data, <<"1">>),
    RequestingLeg = wh_json:get_value(<<"dtmf_leg">>, Data),

    HoldCommand = whapps_call_command:soft_hold_command(RequestingLeg, Unholdkey, AMOHToPlay, BMOHToPlay),

    lager:debug("leg ~s is putting ~s on hold", [RequestingLeg, hold_leg(Call, RequestingLeg)]),

    whapps_call_command:send_command(props:set_value(<<"Insert-At">>, <<"now">>, HoldCommand)
                                     ,Call
                                    ),
    {'continue', Call}.

-spec hold_leg(whapps_call:call(), ne_binary()) -> ne_binary().
hold_leg(Call, RequestingLeg) ->
    case whapps_call:call_id(Call) of
        RequestingLeg -> whapps_call:other_leg_call_id(Call);
        HoldLeg -> HoldLeg
    end.

-spec number_builder(wh_json:object()) -> wh_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'hold' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'hold'? ", "~d"),

    K = [<<"numbers">>, wh_util:to_binary(Number)],

    case number_builder_check(wh_json:get_value(K, DefaultJObj)) of
        'undefined' -> wh_json:delete_key(K, DefaultJObj);
        NumberJObj -> wh_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    number_builder_moh(wh_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [wh_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(wh_json:object(), string()) -> api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_moh(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_moh(wh_json:object()) -> wh_json:object().
number_builder_moh(NumberJObj) ->
    {'ok', [MOH]} = io:fread("Any custom music on hold to play ('n' to leave as default MOH, 'h' for help)? ", "~s"),
    metaflow_jobj(NumberJObj, MOH).

-spec metaflow_jobj(wh_json:object(), string()) -> wh_json:object().
metaflow_jobj(NumberJObj, "h") ->
    io:format("To set a system_media file as MOH, enter: /system_media/{MEDIA_ID}~n", []),
    io:format("To set an account's media file as MOH, enter: /{ACCOUNT_ID}/{MEDIA_ID}~n", []),
    io:format("To set an third-party HTTP url, enter: http://other.server.com/moh.mp3~n~n", []),
    number_builder_moh(NumberJObj);
metaflow_jobj(NumberJObj, MOH) ->
    wh_json:set_values([{<<"module">>, <<"hold">>}
                        ,{<<"data">>, moh_data(MOH)}
                       ], NumberJObj).

-spec moh_data(string()) -> wh_json:object().
moh_data("n") ->
    wh_json:new();
moh_data(MOH) ->
    wh_json:from_list([{<<"moh">>, wh_util:to_binary(MOH)}]).
