%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Hang up the call
%%% Data = {}
%%% @end
%%% @contributors
%%%   Michal Tesar
%%%-------------------------------------------------------------------
-module(konami_break).

-export([handle/2
         ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    RequestingLeg = wh_json:get_value(<<"dtmf_leg">>, Data),

    lager:debug("attempting to break ~s", [RequestingLeg]),

    Command = [{<<"Application-Name">>, <<"break">>}
               ,{<<"Call-ID">>, RequestingLeg}
               ,{<<"Insert-At">>, <<"now">>}
              ],
    whapps_call_command:send_command(Command, Call),
    {'continue', Call}.

-spec number_builder(wh_json:object()) -> wh_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'break' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'break'? ", "~d"),

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
    wh_json:set_values([{<<"module">>, <<"break">>}
                        ,{<<"data">>, wh_json:new()}
                       ], NumberJObj).
