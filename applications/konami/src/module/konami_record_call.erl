%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Record something
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_record_call).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    handle(Data, Call, wh_json:get_value(<<"action">>, Data, <<"start">>)),
    {'continue', Call}.

handle(Data, Call, <<"start">> = Action) ->
    Timelimit = timelimit(Data),
    MediaName = media_name(whapps_call:call_id_direct(Call), format(Data)),
    lager:debug("starting recording for ~s", [MediaName]),
    whapps_call_command:record_call(MediaName, Action, Timelimit, Call).

-spec recording_doc_id(ne_binary()) -> ne_binary().
recording_doc_id(CallId) -> <<"call_recording_", CallId/binary>>.

-spec media_name(ne_binary(), ne_binary()) -> ne_binary().
media_name(CallId, Ext) ->
    <<(recording_doc_id(CallId))/binary, ".", Ext/binary>>.

-spec timelimit(wh_json:object()) -> non_neg_integer().
timelimit(JObj) ->
    Max = whapps_config:get(?CF_CONFIG_CAT, <<"max_recording_time_limit">>, 600),
    case wh_json:get_integer_value(<<"time_limit">>, JObj) of
        'undefined' -> Max;
        N when N > Max -> Max;
        N when N =< 0 -> Max;
        N -> N
    end.

-spec url(wh_json:object()) -> ne_binary().
url(Data) ->
    wh_json:get_value(<<"url">>, Data).

-spec format(wh_json:object()) -> ne_binary().
format(Data) ->
    case wh_json:get_value(<<"format">>, Data) of
        <<"mp3">> -> <<"mp3">>;
        _ -> <<"wav">>
    end.
