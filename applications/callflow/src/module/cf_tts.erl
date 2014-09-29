%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "text":"This is what should be said"
%%%   // optional
%%%   ,"voice":"male" // or "female"
%%%   ,"langauge":"en"
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
    %% Data is the "data" object from the JSON payload
    %% Call is the current whapps_call record

    _ = whapps_call_command:b_tts(
          wh_json:get_value(<<"text">>, Data)
          ,wh_json:get_value(<<"voice">>, Data)
          ,wh_json:get_value(<<"language">>, Data)
          ,?ANY_DIGIT
          ,wh_json:get_value(<<"engine">>, Data)
          ,Call
         ),

    %% Give control back to cf_exe process
    cf_exe:continue(Call).
