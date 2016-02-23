%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Reconnect the two legs of the call, if possible
%%% Data = {
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_resume).

-export([handle/2
         ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(_Data, Call) ->
    lager:debug("reconnecting ~s and ~s", [whapps_call:call_id(Call)
                                           ,whapps_call:other_leg_call_id(Call)
                                          ]),
    whapps_call_command:pickup(whapps_call:other_leg_call_id(Call), <<"now">>, Call),
    {'continue', Call}.

-spec number_builder(wh_json:object()) -> wh_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'resume' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'resume'? ", "~d"),

    K = [<<"numbers">>, wh_util:to_binary(Number)],

    case number_builder_check(wh_json:get_value(K, DefaultJObj)) of
        'undefined' -> wh_json:delete_key(K, DefaultJObj);
        NumberJObj -> wh_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    metaflow_jobj(wh_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [wh_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(wh_json:object(), string()) -> api_object().
number_builder_check_option(NumberJObj, "e") ->
    metaflow_jobj(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec metaflow_jobj(wh_json:object()) -> wh_json:object().
metaflow_jobj(NumberJObj) ->
    wh_json:set_values([{<<"module">>, <<"resume">>}
                        ,{<<"data">>, wh_json:new()}
                       ], NumberJObj).
