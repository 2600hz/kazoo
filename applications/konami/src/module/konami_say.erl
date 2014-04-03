%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Say something
%%% Data = {
%%%   "text":"text to say"
%%%   ,"type":"name_spelled"
%%%   ,"method":"pronounced"
%%%   ,"language":"en"
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_say).

-export([handle/2]).

-include("../konami.hrl").

-spec handle(wh_json:object(), whapps_call:call()) ->
                    {'continue', whapps_call:call()}.
handle(Data, Call) ->
    Say = wh_json:get_value(<<"text">>, Data),
    lager:debug("saying '~s'", [Say]),

    SayCommand = whapps_call_command:say_command(Say
                                                 ,wh_json:get_value(<<"type">>, Data)
                                                 ,wh_json:get_value(<<"method">>, Data)
                                                 ,wh_json:get_value(<<"language">>, Data)
                                                 ,Call
                                                ),
    whapps_call_command:send_command(
      wh_json:set_value(<<"Insert-At">>, <<"now">>, SayCommand)
      ,Call
     ),
    {'continue', Call}.
