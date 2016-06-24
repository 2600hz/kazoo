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

-include_lib("callflow/src/callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DTMFs = kz_json:get_value(<<"digits">>, Data),
    Duration = kz_json:get_binary_value(<<"duration_ms">>, Data),

    kapps_call_command:send_dtmf(DTMFs, Duration, Call),
    lager:debug("sent '~s' @ '~s' duration", [DTMFs, Duration]),

    cf_exe:continue(Call).
