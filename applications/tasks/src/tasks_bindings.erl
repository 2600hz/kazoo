%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Store routing keys/pid bindings. When a binding is fired,
%%% pass the payload to the pid for evaluation, accumulating
%%% the results for the response to the running process.
%%%
%%% ```
%%% foo.erl -> bind("module.init").
%%% *** Later ***
%%% module.erl
%%%   init() -> run("module.init", {some, "payload", 4, <<"You">>}).
%%%                foo ! Payload,
%%%                receive -> Resp
%%%   init() <- [Resp]
%%%   init() -> Decides what to do with responses
%%% '''
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Ben Wann
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tasks_bindings).

%% API
-export([bind/3, bind/4
        ,bind_actions/3
        ,unbind/3, unbind/4
        ,apply/2, apply/3
        ,map/2
        ,pmap/2, pmap/3
        ,fold/2
        ,flush/0, flush/1
        ,filter/1
        ,modules_loaded/0
        ,init/0, init_mod/1
        ]).

%% Helper Functions for Results of a map/2
-export([any/1
        ,all/1
        ,succeeded/1
        ,failed/1
        ]).

-include("tasks.hrl").

-type payload() :: list() | kz_json:object() | kz_term:ne_binary().

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec apply(kz_json:object(), list()) -> list().
apply(API, Args) ->
    Action = kz_json:get_value(<<"action">>, API),
    ?MODULE:apply(API, Action, Args).

-spec apply(kz_json:object(), kz_term:ne_binary(), list()) -> list().
apply(API, <<Action/binary>>, Args) when is_list(Args) ->
    Category = kz_json:get_ne_binary_value(<<"category">>, API),
    Route = <<"tasks.", Category/binary, ".", Action/binary>>,
    lager:debug("using route ~s", [Route]),
    map(Route, Args).

%%------------------------------------------------------------------------------
%% @doc Map over bound handlers.
%% Return `[{Result, Payload1}]', a list of tuples, the first element
%% of which is the result of the bound handler, and the second element
%% is the payload, possibly modified
%% @end
%%------------------------------------------------------------------------------
-type map_results() :: list().
-spec map(kz_term:ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    kazoo_bindings:map(Routing, Payload).

-spec pmap(kz_term:ne_binary(), payload()) -> map_results().
pmap(Routing, Payload) ->
    kazoo_bindings:pmap(Routing, Payload).

-spec pmap(kz_term:ne_binary(), payload(), kazoo_bindings:kz_rt_options()) -> map_results().
pmap(Routing, Payload, Options) ->
    kazoo_bindings:pmap(Routing, Payload, Options).

%%------------------------------------------------------------------------------
%% @doc Fold over bound handlers.
%% Return the modified Payload after it has been threaded through
%% all matching bindings
%% @end
%%------------------------------------------------------------------------------
-type fold_results() :: payload().
-spec fold(kz_term:ne_binary(), payload()) -> fold_results().
fold(Routing, Payload) ->
    kazoo_bindings:fold(Routing, Payload).

%%------------------------------------------------------------------------------
%% @doc Helper functions for working on a result set of bindings.
%% @end
%%------------------------------------------------------------------------------
-spec any(map_results()) -> boolean().
any(Res) when is_list(Res) ->
    kazoo_bindings:any(Res, fun check_bool/1).

-spec all(map_results()) -> boolean().
all(Res) when is_list(Res) ->
    kazoo_bindings:all(Res, fun check_bool/1).

-spec failed(map_results()) -> map_results().
failed(Res) when is_list(Res) ->
    kazoo_bindings:failed(Res, fun filter_out_succeeded/1).

-spec succeeded(map_results()) -> map_results().
succeeded(Res) when is_list(Res) ->
    kazoo_bindings:succeeded(Res, fun filter_out_failed/1).

%%------------------------------------------------------------------------------
%% @doc Helpers for the result set helpers.
%% @end
%%------------------------------------------------------------------------------
-spec check_bool({boolean(), any()} | boolean()) -> boolean().
check_bool({'true', _}) -> 'true';
check_bool('true') -> 'true';
check_bool(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_out_failed({boolean() | 'halt', any()} | boolean() | any()) -> boolean().
filter_out_failed({'true', _}) -> 'true';
filter_out_failed('true') -> 'true';
filter_out_failed({'halt', _}) -> 'true';
filter_out_failed({'false', _}) -> 'false';
filter_out_failed('false') -> 'false';
filter_out_failed({'EXIT', _}) -> 'false';
filter_out_failed(Term) -> not kz_term:is_empty(Term).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_out_succeeded({boolean() | 'halt', any()} | boolean() | any()) -> boolean().
filter_out_succeeded({'true', _}) -> 'false';
filter_out_succeeded('true') -> 'false';
filter_out_succeeded({'halt', _}) -> 'true';
filter_out_succeeded({'false', _}) -> 'true';
filter_out_succeeded('false') -> 'true';
filter_out_succeeded({'EXIT', _}) -> 'true';
filter_out_succeeded(Term) -> kz_term:is_empty(Term).

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result()].
-spec bind(kz_term:ne_binary() | kz_term:ne_binaries(), atom(), atom()) ->
                  bind_result() | bind_results().
bind(Bindings, Module, Fun) ->
    bind(Bindings, Module, Fun, 'undefined').

-spec bind(kz_term:ne_binary() | kz_term:ne_binaries(), atom(), atom(), any()) ->
                  bind_result() | bind_results().
bind([_|_]=Bindings, Module, Fun, Payload) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    kazoo_bindings:bind(Binding, Module, Fun, Payload).

-spec bind_actions(kz_term:ne_binary(), module(), kz_term:ne_binaries()) -> 'ok'.
bind_actions(RoutePrefix, Module, Actions) ->
    lists:foreach(fun (Action) ->
                          bind(<<RoutePrefix/binary, ".", Action/binary>>
                              ,Module
                              ,kz_term:to_atom(Action)
                              )
                  end
                 ,Actions
                 ).

-spec unbind(kz_term:ne_binary() | kz_term:ne_binaries(), atom(), atom()) -> 'ok'.
unbind(Bindings, Module, Fun) ->
    unbind(Bindings, Module, Fun, 'undefined').

-spec unbind(kz_term:ne_binary() | kz_term:ne_binaries(), atom(), atom(), any()) -> 'ok'.
unbind([_|_]=Bindings, Module, Fun, Payload) ->
    _ = [unbind(Binding, Module, Fun, Payload) || Binding <- Bindings],
    'ok';
unbind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    _ = kazoo_bindings:unbind(Binding, Module, Fun, Payload),
    'ok'.

-spec flush() -> 'ok'.
flush() ->
    lists:foreach(fun kazoo_bindings:flush_mod/1, modules_loaded()).

-spec flush(kz_term:ne_binary()) -> 'ok'.
flush(Binding) -> kazoo_bindings:flush(Binding).

-spec filter(kazoo_bindings:filter_fun()) -> 'ok'.
filter(Predicate) ->
    kazoo_bindings:filter(Predicate).

-spec modules_loaded() -> kz_term:atoms().
modules_loaded() ->
    lists:usort(
      [Mod || Mod <- kazoo_bindings:modules_loaded(),
              is_task_module(Mod)
      ]).

-spec is_task_module(kz_term:ne_binary() | atom()) -> boolean().
is_task_module(<<"kt_", _/binary>>) -> 'true';
is_task_module(Mod)
  when is_atom(Mod) ->
    is_task_module(kz_term:to_binary(Mod));
is_task_module(_) -> 'false'.


-spec init() -> 'ok'.
init() ->
    lager:debug("initializing tasks bindings"),
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    lists:foreach(fun init_mod/1, ?TASKS).

-spec init_mod(module()) -> any().
init_mod(ModuleName) ->
    lager:debug("initializing module: ~p", [ModuleName]),
    maybe_init_mod(ModuleName).

maybe_init_mod(ModuleName) ->
    maybe_init_mod(ModuleName, kz_module:is_exported(ModuleName, 'init', 0)).

maybe_init_mod(_M, 'false') -> 'ok';
maybe_init_mod(ModuleName, 'true') ->
    lager:debug("trying to init module: ~p", [ModuleName]),
    (kz_term:to_atom(ModuleName, 'true')):init().
