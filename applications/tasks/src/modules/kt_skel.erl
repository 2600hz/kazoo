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
-export([help/0
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
    _ = tasks_provider:bind(<<"tasks."?CATEGORY".id1">>, ?MODULE, 'id1'),
    _ = tasks_provider:bind(<<"tasks."?CATEGORY".id2">>, ?MODULE, 'id2'),
    _ = tasks_bindings:bind(<<"tasks.help."?CATEGORY".id1">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks.help."?CATEGORY".id2">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks.help."?CATEGORY>>, ?MODULE, 'help').


-spec help() -> kz_json:object().
help() ->
    kz_json:from_list(
      [{<<?CATEGORY>>
       ,kz_json:from_list([{Action, help(Action)} || Action <- ?ACTIONS])
       }
      ]).

-spec help(ne_binary()) -> kz_json:object().
help(<<"id1">>) ->
    kz_json:from_list(
      [{<<"description">>, <<"The identity task">>}
      ,{<<"doc">>, <<"Takes 1 column as input and return it as is.">>}
      ,{<<"expected_content">>, <<"text/csv">>}
      ]
     );
help(<<"id2">>) ->
    kz_json:from_list(
      [{<<"description">>, <<"The identity task">>}
      ,{<<"doc">>, <<"Takes 2 columns as input and return them as is.">>}
      ,{<<"expected_content">>, <<"text/csv">>}
      ]
     ).

%%% Verifiers


%%% Appliers

-spec id1(kz_proplist(), task_iterator(), api_binary()) -> task_return().
id1(_Props, _IterValue, Col1) ->
    [Col1].

-spec id2(kz_proplist(), task_iterator(), api_binary(), api_binary()) -> task_return().
id2(_Props, _IterValue, Col1, Col2) ->
    [Col1, Col2].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% End of Module.
