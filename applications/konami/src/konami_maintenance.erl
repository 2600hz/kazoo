%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(konami_maintenance).

-export([is_running/0
         ,add_default_metaflow/0
         ,add_default_account_metaflow/1
        ]).

is_running() ->
    case lists:keyfind('konami', 1, application:which_applications()) of
        'false' -> io:format("Konami is not currently running on this node~n", []);
        {_App, _Desc, _Vsn} ->
            io:format("Konami (~s) is running~n", [_Vsn])
    end.

add_default_metaflow() ->
    Default = whapps_config:get(<<"metaflows">>, <<"default_metaflow">>, wh_json:new()),
    io:format("Welcome to the Default System Metaflow builder~n"),
    intro_builder(Default, fun(JObj) ->
                                   whapps_config:set_default(<<"metaflows">>, <<"default_metaflow">>, JObj)
                           end).

add_default_account_metaflow(AccountId) ->
    Default = whapps_account_config:get(AccountId, <<"metaflows">>, <<"default_metaflow">>, wh_json:new()),
    io:format("Welcome to the Default Account Metaflow builder for ~s~n", [AccountId]),
    intro_builder(Default, fun(JObj) ->
                                   whapps_account_config:set(AccountId, <<"metaflows">>, <<"default_metaflow">>, JObj)
                           end).

intro_builder(Default, SaveFun) ->
    io:format("The current default metaflow:~n"),
    io:format("  Binding Digit: ~s~n"
              ,[wh_json:get_value(<<"binding_digit">>, Default, konami_config:binding_digit())]
             ),
    io:format("  Digit Timeout(ms): ~b~n"
              ,[wh_json:get_integer_value(<<"digit_timeout_ms">>, Default, konami_config:timeout())]
             ),

    io:format("  Numbers: ~s~n", [wh_json:encode(wh_json:get_value(<<"numbers">>, Default, wh_json:new()))]),
    io:format("  Patterns: ~s~n~n", [wh_json:encode(wh_json:get_value(<<"patterns">>, Default, wh_json:new()))]),

    menu_builder(Default, SaveFun).

menu_builder(Default, SaveFun) ->
    io:format("1. Change Binding Digit~n"
              "2. Change Digit Timeout~n"
              "3. Change Numbers~n"
              "4. Change Patterns~n"
              "5. Show Current Defaults~n"
              "6. Save~n"
              "7. Exit~n~n"
              ,[]),
    {'ok', [Option]} = io:fread("Which action: ", "~d"),
    menu_builder_action(Default, SaveFun, Option).

menu_builder_action(Default, SaveFun, 1) ->
    {'ok', [BindingDigit]} = io:fread("New binding digit: ", "~s"),
    menu_builder(wh_json:set_value(<<"binding_digit">>, BindingDigit, Default), SaveFun);
menu_builder_action(Default, SaveFun, 2) ->
    {'ok', [DigitTimeout]} = io:fread("New digit timeout (ms): ", "~d"),
    menu_builder(wh_json:set_value(<<"digit_timeout_ms">>, DigitTimeout, Default), SaveFun);
menu_builder_action(Default, SaveFun, 3) ->
    number_builder(Default, SaveFun);
menu_builder_action(Default, SaveFun, 4) ->
    pattern_builder(Default, SaveFun);
menu_builder_action(Default, SaveFun, 5) ->
    intro_builder(Default, SaveFun);
menu_builder_action(Default, SaveFun, 6) ->
    case SaveFun(Default) of
        {'ok', _} ->
            io:format("Defaults successfully saved!~n~n"),
            intro_builder(Default, SaveFun);
        {'error', E} ->
            io:format("failed to save defaults: ~p~n", [E]),
            menu_builder(Default, SaveFun)
    end;
menu_builder_action(_Default, _SaveFun, 7) ->
    'ok';
menu_builder_action(Default, SaveFun, _) ->
    io:format("Action not recognized!~n~n"),
    menu_builder(Default, SaveFun).

number_builder(_Default, _SaveFun) ->
    'ok'.

pattern_builder(_Default, _SaveFun) ->
    'ok'.
