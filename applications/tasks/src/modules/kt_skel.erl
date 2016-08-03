%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_skel).
%% behaviour: tasks_provider

-export([init/0]).
-export([help/0, help/1
        ]).

%% Verifiers
-export([
        ]).

%% Appliers
-export([id1/3
        ,id2/4
        ]).

-include_lib("kazoo/include/kz_types.hrl").

-define(CATEGORY, "skel").
-define(ACTIONS, [<<"id1">>
                 ,<<"id2">>
                 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    lists:foreach(fun init/1, ?ACTIONS),
    _ = tasks_bindings:bind(<<"tasks.help."?CATEGORY>>, ?MODULE, 'help').

-spec init(ne_binary()) -> any().
init(Action) ->
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".", Action/binary>>, ?MODULE, kz_util:to_atom(Action)),
    _ = tasks_bindings:bind(<<"tasks.help."?CATEGORY".", Action/binary>>, ?MODULE, 'help').

-spec help() -> kz_json:object().
-spec help(ne_binaries()) -> kz_json:object().
help() -> help(?ACTIONS).
help(Actions) ->
    kz_json:from_list(
      [{<<?CATEGORY>>
       ,kz_json:from_list([{Action, action(Action)} || Action <- Actions])
       }
      ]).

-spec action(ne_binary()) -> kz_json:object().
action(<<"id1">>) ->
    kz_json:from_list(
      [{<<"description">>, <<"The identity task">>}
      ,{<<"doc">>, <<"Takes 1 column as input and return it as is.">>}
      ,{<<"expected_content">>, <<"text/csv">>}
      ,{<<"optional">>, [<<"col1">>]}
      ]
     );
action(<<"id2">>) ->
    kz_json:from_list(
      [{<<"description">>, <<"The identity task">>}
      ,{<<"doc">>, <<"Takes 2 columns as input and return them as is.">>}
      ,{<<"expected_content">>, <<"text/csv">>}
      ,{<<"mandatory">>, [<<"col1">>, <<"col2">>]}
      ]
     ).

%%% Verifiers


%%% Appliers

-spec id1(kz_proplist(), task_iterator(), api_binary()) -> task_return().
id1(_Props, _IterValue, Col1) ->
    [Col1].

-spec id2(kz_proplist(), task_iterator(), ne_binary(), ne_binary()) -> task_return().
id2(_Props, _IterValue, Col1, Col2) ->
    [Col1, Col2].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% End of Module.
