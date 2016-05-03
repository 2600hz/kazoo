%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Record something
%%% "data":{
%%%   "action":["start","stop"] // one of these
%%%   ,"time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%%   ,"url":"http://server.com/path/to/dump/file" // what URL to PUT the file to
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_record_call).

-export([handle/2
         ,number_builder/1
        ]).

-include("konami.hrl").

-spec handle(kz_json:object(), kapps_call:call()) ->
                    {'continue', kapps_call:call()} |
                    no_return().
handle(Data, Call) ->
    handle(Data, Call, get_action(kz_json:get_value(<<"action">>, Data))),
    {'continue', Call}.

handle(Data, Call, <<"start">>) ->
    lager:debug("starting recording, see you on the other side"),
    case kz_json:is_true(<<"record_on_answer">>, Data, 'false') of
        'true' -> kz_media_recording:start_recording(Call, Data);
        'false' ->
            Format = kz_media_recording:get_format(kz_json:get_value(<<"format">>, Data)),
            MediaName = kz_media_recording:get_media_name(kapps_call:call_id(Call), Format),
            Props = [{<<"Media-Name">>, MediaName}
                     ,{<<"Media-Transfer-Method">>, kz_json:get_value(<<"method">>, Data, <<"put">>)}
                     ,{<<"Media-Transfer-Destination">>, kz_json:get_value(<<"url">>, Data)}
                     ,{<<"Additional-Headers">>, kz_json:get_value(<<"additional_headers">>, Data)}
                     ,{<<"Time-Limit">>, kz_json:get_value(<<"time_limit">>, Data)}
                    ],
            _ = kapps_call_command:record_call(Props, <<"start">>, Call)
    end;
handle(Data, Call, <<"stop">> = Action) ->
    Format = kz_media_recording:get_format(kz_json:get_value(<<"format">>, Data)),
    MediaName = kz_media_recording:get_media_name(kapps_call:call_id(Call), Format),

    _ = kapps_call_command:record_call([{<<"Media-Name">>, MediaName}], Action, Call),
    lager:debug("sent command to stop recording").

-spec get_action(api(binary())) -> ne_binary().
get_action('undefined') -> <<"start">>;
get_action(<<"stop">>) -> <<"stop">>;
get_action(_) -> <<"start">>.

-spec number_builder(kz_json:object()) -> kz_json:object().
number_builder(DefaultJObj) ->
    io:format("Let's configure a 'record_call' metaflow~n", []),

    {'ok', [Number]} = io:fread("What number should invoke 'record_call'? ", "~d"),

    K = [<<"numbers">>, kz_util:to_binary(Number)],

    case number_builder_check(kz_json:get_value(K, DefaultJObj)) of
        'undefined' -> kz_json:delete_key(K, DefaultJObj);
        NumberJObj -> kz_json:set_value(K, NumberJObj, DefaultJObj)
    end.

-spec number_builder_check(api_object()) -> api_object().
number_builder_check('undefined') ->
    number_builder_action(kz_json:new());
number_builder_check(NumberJObj) ->
    io:format("  Existing config for this number: ~s~n", [kz_json:encode(NumberJObj)]),
    io:format("  e. Edit Number~n", []),
    io:format("  d. Delete Number~n", []),
    {'ok', [Option]} = io:fread("What would you like to do: ", "~s"),
    number_builder_check_option(NumberJObj, Option).

-spec number_builder_check_option(kz_json:object(), string()) -> api_object().
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
    number_builder_time_limit(NumberJObj, kz_util:to_binary(Action)).

-spec number_builder_time_limit(kz_json:object(), ne_binary()) -> kz_json:object().
number_builder_time_limit(NumberJObj, Action) ->
    {'ok', [TimeLimit]} = io:fread("How many seconds to limit the recording to: ", "~d"),
    number_builder_format(NumberJObj, Action, TimeLimit).

-spec number_builder_format(kz_json:object(), ne_binary(), pos_integer()) -> kz_json:object().
number_builder_format(NumberJObj, Action, TimeLimit) ->
    {'ok', [Format]} = io:fread("What format would you like the recording? ('wav' or 'mp3'): ", "~3s"),
    number_builder_url(NumberJObj, Action, TimeLimit, kz_util:to_binary(Format)).

-spec number_builder_url(kz_json:object(), ne_binary(), pos_integer(), ne_binary()) -> kz_json:object().
number_builder_url(NumberJObj, Action, TimeLimit, Format) ->
    {'ok', [URL]} = io:fread("What URL to send the recording to at the end: ", "~s"),
    metaflow_jobj(NumberJObj, Action, TimeLimit, Format, kz_util:to_binary(URL)).

-spec metaflow_jobj(kz_json:object(), ne_binary(), pos_integer(), ne_binary(), ne_binary()) -> kz_json:object().
metaflow_jobj(NumberJObj, Action, TimeLimit, Format, URL) ->
    kz_json:set_values([{<<"module">>, <<"record_call">>}
                        ,{<<"data">>, data(Action, TimeLimit, Format, URL)}
                       ], NumberJObj).

-spec data(ne_binary(), pos_integer(), ne_binary(), ne_binary()) -> kz_json:object().
data(Action, TimeLimit, Format, URL) ->
    kz_json:from_list([{<<"action">>, Action}
                       ,{<<"time_limit">>, TimeLimit}
                       ,{<<"format">>, Format}
                       ,{<<"url">>, URL}
                      ]).
