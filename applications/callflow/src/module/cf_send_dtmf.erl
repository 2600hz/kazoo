%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "digits":"#123" // what sequence to send
%%%   ,"duration_ms":2000 // optional duration, in milliseconds, to send dtmf
%%% }
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_send_dtmf).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DTMFs = wh_json:get_value(<<"digits">>, Data),
    Duration = wh_json:get_binary_value(<<"duration_ms">>, Data),

    whapps_call_command:send_dtmf(DTMFs, Duration, Call),
    lager:debug("sent '~s' @ '~s' duration", [DTMFs, Duration]),

    cf_exe:continue(Call).
