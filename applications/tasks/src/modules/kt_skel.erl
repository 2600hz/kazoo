%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_skel).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ]).

%% Verifiers
-export([col2/1
        ]).

%% Appliers
-export([id1/3
        ,id2/3
        ]).

%% Triggerables
-export([my_minute_job/0
        ]).

-include("tasks.hrl").

-define(CATEGORY, "skel").
-define(ACTIONS, [<<"id1">>
                 ,<<"id2">>
                 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".col2">>, ?MODULE, 'col2'),
    _ = tasks_bindings:bind(?TRIGGER_MINUTELY, ?MODULE, 'my_minute_job'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(<<"id2">>) ->
    [<<"Col1">>, <<"Col2">>].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> kz_term:proplist().
action(<<"id1">>) ->
    [{<<"description">>, <<"The identity task">>}
    ,{<<"doc">>, <<"Takes 1 column as input and return it as is.">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"optional">>, [<<"col1">>]}
    ];
action(<<"id2">>) ->
    [{<<"description">>, <<"The identity task">>}
    ,{<<"doc">>, <<"Takes 2 columns as input and return them as is.">>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"col1">>, <<"col2">>]}
    ].

%%% Verifiers
-spec col2(kz_term:ne_binary()) -> boolean().
col2(?NE_BINARY) -> 'true'.

%%% Appliers

-spec id1(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
id1(_ExtraArgs, _IterValue, #{<<"col1">> := undefined}) ->
    [<<"col1 was not defined">>];
id1(_ExtraArgs, _IterValue, #{<<"col1">> := Col1}) ->
    [<<"col1 was set to", Col1/binary, "!">>].

-spec id2(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
id2(_ExtraArgs, _IterValue, #{<<"col1">> := Col1
                             ,<<"col2">> := Col2
                             }) ->
    [Col1, Col2].

%%% Triggerables

-spec my_minute_job() -> ok.
my_minute_job() ->
    {_, {_, _Data, _}} = calendar:local_time(),
    lager:debug("executed every minute! (~p)", [_Data]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%% End of Module.
