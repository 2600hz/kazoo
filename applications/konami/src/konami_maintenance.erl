%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
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

-include("konami.hrl").

-record(builder_action, {module_fun_name :: atom()
                         ,metaflow_key :: ne_binary()
                         ,builders = [] :: [{pos_integer(), atom()},...] | []
                        }).
-type builder_action() :: #builder_action{}.

-spec is_running() -> 'ok'.
is_running() ->
    case lists:keyfind('konami', 1, application:which_applications()) of
        'false' -> io:format("Konami is not currently running on this node~n", []);
        {_App, _Desc, _Vsn} ->
            io:format("Konami (~s) is running~n", [_Vsn])
    end.

-spec add_default_metaflow() -> 'ok'.
add_default_metaflow() ->
    Default = whapps_config:get(<<"metaflows">>, <<"default_metaflow">>, wh_json:new()),
    io:format("Welcome to the Default System Metaflow builder~n"),
    intro_builder(Default, fun(JObj) ->
                                   whapps_config:set_default(<<"metaflows">>, <<"default_metaflow">>, JObj)
                           end).

-spec add_default_account_metaflow(ne_binary()) -> 'ok'.
add_default_account_metaflow(AccountId) ->
    Default = whapps_account_config:get(AccountId, <<"metaflows">>, <<"default_metaflow">>, wh_json:new()),
    io:format("Welcome to the Default Account Metaflow builder for ~s~n", [AccountId]),
    intro_builder(Default, fun(JObj) ->
                                   whapps_account_config:set(AccountId, <<"metaflows">>, <<"default_metaflow">>, JObj)
                           end).

-type save_fun() :: fun((wh_json:object()) -> any()).

-spec intro_builder(wh_json:object(), save_fun()) -> 'ok'.
intro_builder(Default, SaveFun) ->
    wh_util:ensure_started('konami'),
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

-spec menu_builder(wh_json:object(), save_fun()) -> 'ok'.
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

-spec menu_builder_action(wh_json:object(), save_fun(), pos_integer()) -> 'ok'.
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

-spec number_builder(wh_json:object(), save_fun()) -> 'ok'.
number_builder(Default, SaveFun) ->
    Ms = builder_modules('number_builder'),
    builder_menu(Default
                 ,SaveFun
                 ,#builder_action{builders=lists:zip(lists:seq(1, length(Ms)), Ms)
                                  ,metaflow_key = <<"numbers">>
                                  ,module_fun_name='number_builder'
                                 }
                ).

-spec pattern_builder(wh_json:object(), save_fun()) -> 'ok'.
pattern_builder(Default, SaveFun) ->
    Ms = builder_modules('pattern_builder'),
    builder_menu(Default
                 ,SaveFun
                 ,#builder_action{builders=lists:zip(lists:seq(1, length(Ms)), Ms)
                                  ,metaflow_key = <<"patterns">>
                                  ,module_fun_name='pattern_builder'
                                 }
                ).

-spec print_builders(wh_proplist()) -> ['ok',...] | [].
print_builders(Builders) ->
    [io:format("  ~b. ~s~n", [N, builder_name(M)]) || {N, M} <- Builders].

-spec builder_name(ne_binary() | atom()) -> ne_binary().
builder_name(<<"konami_", Name/binary>>) -> wh_util:ucfirst_binary(Name);
builder_name(<<_/binary>> = Name) -> wh_util:ucfirst_binary(Name);
builder_name(M) -> builder_name(wh_util:to_binary(M)).

-spec builder_menu(wh_json:object(), save_fun(), builder_action()) -> 'ok'.
builder_menu(Default, SaveFun, #builder_action{builders=Builders
                                               ,metaflow_key=Key
                                              }=BA) ->
    io:format("~s Builders:~n", [wh_util:ucfirst_binary(Key)]),

    print_builders(Builders),
    io:format("  0. Return to Menu~n~n", []),

    {'ok', [Option]} = io:fread("Which builder to add: ", "~d"),
    builder_action(Default
                   ,SaveFun
                   ,BA
                   ,Option
                  ).

-spec builder_action(wh_json:object(), save_fun(), builder_action(), non_neg_integer()) -> 'ok'.
builder_action(Default, SaveFun, _BA, 0) ->
    menu_builder(Default, SaveFun);
builder_action(Default, SaveFun, #builder_action{builders=Builders}=BA, N) ->
    case lists:keyfind(N, 1, Builders) of
        'false' ->
            invalid_action(Default, SaveFun, BA);
        {_, Module} ->
            execute_action(Default, SaveFun, BA, Module)
    end.

-spec execute_action(wh_json:object(), save_fun(), builder_action(), atom()) -> 'ok'.
execute_action(Default, SaveFun, #builder_action{module_fun_name=ModuleFun
                                                 ,metaflow_key=Key
                                                }=BA, Module) ->
    try Module:ModuleFun(Default) of
        NewDefault ->
            io:format("  ~s: ~s~n~n", [wh_util:ucfirst_binary(Key)
                                       ,wh_json:encode(wh_json:get_value(Key, NewDefault, wh_json:new()))
                                      ]),
            builder_menu(NewDefault, SaveFun, BA)
    catch
        _E:_R ->
            io:format("failed to build ~s metaflow for ~s~n", [Key, Module]),
            io:format("~s: ~p~n~n", [_E, _R]),
            builder_menu(Default, SaveFun, BA)
    end.

-spec invalid_action(wh_json:object(), save_fun(), builder_action()) -> 'ok'.
invalid_action(Default, SaveFun, BA) ->
    io:format("invalid option selected~n", []),
    builder_menu(Default, SaveFun, BA).

-spec builder_modules(atom()) -> atoms().
builder_modules(F) ->
    {'ok', Modules} = application:get_key('konami', 'modules'),
    [M || M <- Modules, is_builder_module(M, F)].

-spec is_builder_module(atom(), atom()) -> boolean().
is_builder_module(M, F) ->
    try M:module_info('exports') of
        Exports -> props:get_value(F, Exports) =:= 1
    catch
        _E:_R -> 'false'
    end.
