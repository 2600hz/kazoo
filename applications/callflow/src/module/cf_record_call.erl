%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%
%%% Fix KAZOO-3406: Sponsored by Velvetech LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(cf_record_call).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
-spec handle(kz_json:object(), kapps_call:call(), ne_binary()) -> kapps_call:call().
handle(Data, Call) ->
    cf_exe:continue(
      handle(Data, Call, get_action(Data))
     ).

handle(Data, Call, <<"start">>) ->
    cf_exe:update_call(kapps_call:start_recording(Data, Call));

handle(_Data, Call, <<"stop">>) ->
    cf_exe:update_call(kapps_call:stop_recording(Call)).


-spec get_action(api_object()) -> ne_binary().
get_action('undefined') -> <<"start">>;
get_action(Data) ->
    case kz_json:get_value(<<"action">>, Data) of
        <<"stop">> -> <<"stop">>;
        _ -> <<"start">>
    end.
