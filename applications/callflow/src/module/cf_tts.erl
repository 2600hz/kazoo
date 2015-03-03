%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "text":"This is what should be said"
%%%   // optional
%%%   ,"voice":"male" // or "female"
%%%   ,"language":"en"
%%%   ,"engine":"flite" // or "ispeech if configured
%%% }
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_tts).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    NoopId = whapps_call_command:tts(
               wh_json:get_value(<<"text">>, Data)
               ,wh_json:get_value(<<"voice">>, Data)
               ,wh_json:get_value(<<"language">>, Data)
               ,?ANY_DIGIT
               ,wh_json:get_value(<<"engine">>, Data)
               ,Call
              ),

    {'ok', Call1} = cf_util:wait_for_noop(Call, NoopId),

    %% Give control back to cf_exe process
    cf_exe:set_call(Call1),
    cf_exe:continue(Call1).
