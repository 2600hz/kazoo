%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc Put the call on hold
%%% Data = {
%%%   "moh_aleg":"media_id"
%%%   ,"moh_bleg":"media_id"
%%%   ,"unhold_key":"DTMF"
%%% }
%%%
%%% @author James Aimonetti
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_hold).

-export([handle/2
        ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
          {'continue', kapps_call:call()}.
handle(Data, Call) ->
    AMOH = kz_json:get_value(<<"moh_aleg">>, Data),
    AMOHToPlay = kz_media_util:media_path(AMOH, kapps_call:account_id(Call)),

    BMOH = kz_json:get_value(<<"moh_bleg">>, Data, AMOH),
    BMOHToPlay = kz_media_util:media_path(BMOH, kapps_call:account_id(Call)),

    Unholdkey = kz_json:get_value(<<"unhold_key">>, Data, <<"1">>),

    RequestingLeg = kz_json:get_ne_binary_value(<<"dtmf_leg">>, Data),

    HoldCommand = kapps_call_command:soft_hold_command(RequestingLeg, Unholdkey, AMOHToPlay, BMOHToPlay),

    lager:debug("leg ~s is putting ~s on hold", [RequestingLeg, hold_leg(Call, RequestingLeg)]),

    kapps_call_command:send_command(props:set_value(<<"Insert-At">>, <<"now">>, HoldCommand)
                                   ,Call
                                   ),
    {'continue', Call}.

-spec hold_leg(kapps_call:call(), kz_term:ne_binary()) -> kz_term:ne_binary().
hold_leg(Call, RequestingLeg) when is_binary(RequestingLeg) ->
    case kapps_call:call_id(Call) of
        RequestingLeg -> kapps_call:other_leg_call_id(Call);
        HoldLeg -> HoldLeg
    end.

-spec number_builder(kz_json:object()) -> kz_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'hold' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'hold'? ", "~d"),

    K = [<<"numbers">>, kz_term:to_binary(Number)],

    case number_builder_check(kz_json:get_value(K, DefaultJObj)) of
        'undefined' -> kz_json:delete_key(K, DefaultJObj);
        NumberJObj -> kz_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(kz_term:api_object()) -> kz_term:api_object().
number_builder_check('undefined') ->
    number_builder_moh(kz_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [kz_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(kz_json:object(), string()) -> kz_term:api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_moh(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_moh(kz_json:object()) -> kz_json:object().
number_builder_moh(NumberJObj) ->
    {'ok', [MOH]} = io:fread("Any custom music on hold to play ('n' to leave as default MOH, 'h' for help)? ", "~s"),
    metaflow_jobj(NumberJObj, MOH).

-spec metaflow_jobj(kz_json:object(), string()) -> kz_json:object().
metaflow_jobj(NumberJObj, "h") ->
    io:format("To set a system_media file as MOH, enter: /system_media/{MEDIA_ID}~n", []),
    io:format("To set an account's media file as MOH, enter: /{ACCOUNT_ID}/{MEDIA_ID}~n", []),
    io:format("To set an third-party HTTP url, enter: http://other.server.com/moh.mp3~n~n", []),
    number_builder_moh(NumberJObj);
metaflow_jobj(NumberJObj, MOH) ->
    kz_json:set_values([{<<"module">>, <<"hold">>}
                       ,{<<"data">>, moh_data(MOH)}
                       ], NumberJObj).

-spec moh_data(string()) -> kz_json:object().
moh_data("n") ->
    kz_json:new();
moh_data(MOH) ->
    kz_json:from_list([{<<"moh">>, kz_term:to_binary(MOH)}]).
