%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Record something
%%% "data":{
%%%   "action":["start","stop"] // one of these
%%%   ,"time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%%   ,"url":"http://server.com/path/to/dump/file" // what URL to PUT the file to
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
-module(konami_record_call).

-export([handle/2
        ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()}.
handle(Data, Call) ->
    Call1 = handle(Data, Call, get_action(kz_json:get_ne_binary_value(<<"action">>, Data))),
    {'continue', Call1}.

-spec handle(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) ->
                    kapps_call:call().
handle(Data, Call, <<"mask">>) ->
    LegId = kz_json:get_ne_binary_value(<<"dtmf_leg">>, Data),
    lager:debug("masking recording on leg ~s, see you on the other side", [LegId]),
    kapps_call:mask_recording(LegId, Call);
handle(Data, Call, <<"unmask">>) ->
    LegId = kz_json:get_ne_binary_value(<<"dtmf_leg">>, Data),
    lager:debug("unmasking recording on leg ~s, see you on the other side", [LegId]),
    kapps_call:unmask_recording(LegId, Call);
handle(Data, Call, <<"start">>) ->
    lager:debug("starting recording, see you on the other side"),
    kapps_call:start_recording(Data, Call);
handle(Data, Call, <<"stop">>) ->
    LegId = kz_json:get_ne_binary_value(<<"dtmf_leg">>, Data),
    lager:debug("for leg ~s is sent command to stop recording call", [LegId]),
    kapps_call:stop_recording(LegId, Call).

-spec get_action(kz_term:api_ne_binary()) -> kz_term:ne_binary().
get_action('undefined') -> <<"start">>;
get_action(<<"mask">>) -> <<"mask">>;
get_action(<<"unmask">>) -> <<"unmask">>;
get_action(<<"stop">>) -> <<"stop">>;
get_action(_) -> <<"start">>.

-spec number_builder(kz_json:object()) -> kz_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'record_call' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'record_call'? ", "~d"),

    K = [<<"numbers">>, kz_term:to_binary(Number)],

    case number_builder_check(kz_json:get_value(K, DefaultJObj)) of
        'undefined' -> kz_json:delete_key(K, DefaultJObj);
        NumberJObj -> kz_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(kz_term:api_object()) -> kz_term:api_object().
number_builder_check('undefined') ->
    number_builder_action(kz_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [kz_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(kz_json:object(), string()) -> kz_term:api_object().
number_builder_check_option(NumberJObj, "e") ->
    number_builder_action(NumberJObj);
number_builder_check_option(_NumberJObj, "d") ->
    'undefined';
number_builder_check_option(NumberJObj, _Option) ->
    io:format("invalid selection~n", []),
    number_builder_check(NumberJObj).

-spec number_builder_action(kz_json:object()) -> kz_json:object().
number_builder_action(NumberJObj) ->
    {'ok', [Action]} = io:fread("What action: 'start' or 'stop': ", "~s"),
    number_builder_time_limit(NumberJObj, kz_term:to_binary(Action)).

-spec number_builder_time_limit(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
number_builder_time_limit(NumberJObj, Action) ->
    {'ok', [TimeLimit]} = io:fread("How many seconds to limit the recording to: ", "~d"),
    number_builder_format(NumberJObj, Action, TimeLimit).

-spec number_builder_format(kz_json:object(), kz_term:ne_binary(), pos_integer()) -> kz_json:object().
number_builder_format(NumberJObj, Action, TimeLimit) ->
    {'ok', [Format]} = io:fread("What format would you like the recording? ('wav' or 'mp3'): ", "~3s"),
    number_builder_url(NumberJObj, Action, TimeLimit, kz_term:to_binary(Format)).

-spec number_builder_url(kz_json:object(), kz_term:ne_binary(), pos_integer(), kz_term:ne_binary()) -> kz_json:object().
number_builder_url(NumberJObj, Action, TimeLimit, Format) ->
    {'ok', [URL]} = io:fread("What URL to send the recording to at the end: ", "~s"),
    metaflow_jobj(NumberJObj, Action, TimeLimit, Format, kz_term:to_binary(URL)).

-spec metaflow_jobj(kz_json:object(), kz_term:ne_binary(), pos_integer(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
metaflow_jobj(NumberJObj, Action, TimeLimit, Format, URL) ->
    kz_json:set_values([{<<"module">>, <<"record_call">>}
                       ,{<<"data">>, data(Action, TimeLimit, Format, URL)}
                       ], NumberJObj).

-spec data(kz_term:ne_binary(), pos_integer(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
data(Action, TimeLimit, Format, URL) ->
    kz_json:from_list([{<<"action">>, Action}
                      ,{<<"time_limit">>, TimeLimit}
                      ,{<<"format">>, Format}
                      ,{<<"url">>, URL}
                      ]).
