%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz INC
%%% @doc
%%% Store routing keys/pid bindings. When a binding is fired,
%%% pass the payload to the pid for evaluation, accumulating
%%% the results for the response to the running process.
%%%
%%% foo.erl -> bind("module.init").
%%% *** Later ***
%%% module.erl
%%%   init() -> run("module.init", {some, "payload", 4, <<"You">>}).
%%%                foo ! Payload,
%%%                receive -> Resp
%%%   init() <- [Resp]
%%%   init() -> Decides what to do with responses
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole_bindings).

%% API
-export([bind/3
         ,map/2
         ,fold/2
         ,flush/0, flush/1, flush_mod/1
         ,modules_loaded/0
         ,init/0
        ]).

%% Helper Functions for Results of a map/2
-export([any/1
         ,all/1
         ,succeeded/1
         ,failed/1
        ]).

-include("blackhole.hrl").

-type payload() :: 'ok'.

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% return [ {Result, Payload1} ], a list of tuples, the first element
%% of which is the result of the bound handler, and the second element
%% is the payload, possibly modified
%% @end
%%--------------------------------------------------------------------
-type map_results() :: list().
-spec map(ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    kazoo_bindings:map(Routing, Payload).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% return the modified Payload after it has been threaded through
%% all matching bindings
%% @end
%%--------------------------------------------------------------------
-type fold_results() :: payload().
-spec fold(ne_binary(), payload()) -> fold_results().
fold(Routing, Payload) ->
    kazoo_bindings:fold(Routing, Payload).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec any(wh_proplist()) -> boolean().
any(Res) when is_list(Res) ->
    kazoo_bindings:any(Res, fun check_bool/1).

-spec all(wh_proplist()) -> boolean().
all(Res) when is_list(Res) ->
    kazoo_bindings:all(Res, fun check_bool/1).

-spec failed(map_results()) -> map_results().
failed(Res) when is_list(Res) ->
    kazoo_bindings:failed(Res, fun filter_out_succeeded/1).

-spec succeeded(map_results()) -> map_results().
succeeded(Res) when is_list(Res) ->
    kazoo_bindings:succeeded(Res, fun filter_out_failed/1).

%%-------------------------------------------------------------------------
%% @doc
%% Helpers for the result set helpers
%% @end
%%-------------------------------------------------------------------------
-spec check_bool({boolean(), term()} | boolean()) -> boolean().
check_bool({'true', _}) -> 'true';
check_bool('true') -> 'true';
check_bool(_) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_out_failed({boolean() | 'halt', _} | boolean() | term()) -> boolean().
filter_out_failed({'true', _}) -> 'true';
filter_out_failed('true') -> 'true';
filter_out_failed({'halt', _}) -> 'true';
filter_out_failed({'false', _}) -> 'false';
filter_out_failed('false') -> 'false';
filter_out_failed({'EXIT', _}) -> 'false';
filter_out_failed(Term) -> not wh_util:is_empty(Term).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_out_succeeded({boolean() | 'halt', _} | boolean() | term()) -> boolean().
filter_out_succeeded({'true', _}) -> 'false';
filter_out_succeeded('true') -> 'false';
filter_out_succeeded({'halt', _}) -> 'true';
filter_out_succeeded({'false', _}) -> 'true';
filter_out_succeeded('false') -> 'true';
filter_out_succeeded({'EXIT', _}) -> 'true';
filter_out_succeeded(Term) -> wh_util:is_empty(Term).

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result(),...] | [].
-spec bind(ne_binary() | ne_binaries(), atom(), atom()) ->
                  bind_result() | bind_results().
bind([_|_]=Bindings, Module, Fun) ->
    [bind(Binding, Module, Fun) || Binding <- Bindings];
bind(Binding, Module, Fun) when is_binary(Binding) ->
    kazoo_bindings:bind(Binding, Module, Fun).

-spec flush() -> 'ok'.
flush() -> kazoo_bindings:flush().

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> kazoo_bindings:flush(Binding).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(BHMod) -> kazoo_binding:flush(BHMod).

-spec modules_loaded() -> atoms().
modules_loaded() -> kazoo_bindings:modules_loaded().

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing bindings"),
    put('callid', ?LOG_SYSTEM_ID),
    _ = [init_mod(Mod)
         || Mod <- whapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"autoload_modules">>, ?DEFAULT_MODULES)
        ],
    'ok'.

init_mod(ModuleName) ->
    maybe_init_mod(ModuleName),
    maybe_init_mod_supervisor(ModuleName).

maybe_init_mod(ModuleName) ->
    try (wh_util:to_atom(ModuleName, 'true')):init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p.", [ModuleName, _E, _R])
    end.

maybe_init_mod_supervisor(ModuleName) ->
    SupervisorName = wh_util:to_atom(<<ModuleName/binary, "_sup">>, 'true'),
    try blackhole_module_sup:start_child(SupervisorName, 'supervisor') of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p.", [SupervisorName, _E, _R])
    end.
