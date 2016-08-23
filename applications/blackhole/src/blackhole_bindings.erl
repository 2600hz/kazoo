%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
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
-include("blackhole.hrl").

%% API
-export([bind/3,bind/4
        ,unbind/3,unbind/4
        ,map/2
        ,fold/2
        ,flush/0, flush/1, flush_mod/1
        ,filter/1
        ,modules_loaded/0
        ,init/0
        ]).

%% Helper Functions for Results of a map/2
-export([any/1
        ,all/1
        ,succeeded/1
        ,failed/1
        ]).

-export([list/0]).

-type payload() :: bh_context:context() | ne_binary().

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
    kazoo_bindings:map(<<"v1.blackhole.",Routing/binary>>, Payload).

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
    kazoo_bindings:fold(<<"v1.blackhole.", Routing/binary>>, Payload).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec any(kz_proplist()) -> boolean().
any(Res) when is_list(Res) ->
    kazoo_bindings:any(Res, fun check_bool/1).

-spec all(kz_proplist()) -> boolean().
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
-spec check_bool({boolean(), any()} | boolean()) -> boolean().
check_bool({'true', _}) -> 'true';
check_bool('true') -> 'true';
check_bool(_) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_out_failed({boolean() | 'halt', any()} | boolean() | any()) -> boolean().
filter_out_failed({'true', _}) -> 'true';
filter_out_failed('true') -> 'true';
filter_out_failed({'halt', _}) -> 'true';
filter_out_failed({'false', _}) -> 'false';
filter_out_failed('false') -> 'false';
filter_out_failed({'EXIT', _}) -> 'false';
filter_out_failed(Term) -> not kz_util:is_empty(Term).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_out_succeeded({boolean() | 'halt', any()} | boolean() | any()) -> boolean().
filter_out_succeeded({'true', _}) -> 'false';
filter_out_succeeded('true') -> 'false';
filter_out_succeeded({'halt', _}) -> 'true';
filter_out_succeeded({'false', _}) -> 'true';
filter_out_succeeded('false') -> 'true';
filter_out_succeeded({'EXIT', _}) -> 'true';
filter_out_succeeded(Term) -> kz_util:is_empty(Term).

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result()].
-spec bind(ne_binary() | ne_binaries(), atom(), atom()) ->
                  bind_result() | bind_results().
bind(Bindings, Module, Fun) ->
    bind(Bindings, Module, Fun, 'undefined').

-spec bind(ne_binary() | ne_binaries(), atom(), atom(), any()) ->
                  bind_result() | bind_results().
bind([_|_]=Bindings, Module, Fun, Payload) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    kazoo_bindings:bind(<<"v1.blackhole.", Binding/binary>>, Module, Fun, Payload).

-spec unbind(ne_binary() | ne_binaries(), atom(), atom()) -> 'ok'.
unbind(Bindings, Module, Fun) ->
    unbind(Bindings, Module, Fun, 'undefined').

-spec unbind(ne_binary() | ne_binaries(), atom(), atom(), any()) -> 'ok'.
unbind([_|_]=Bindings, Module, Fun, Payload) ->
    _ = [unbind(Binding, Module, Fun, Payload) || Binding <- Bindings],
    'ok';
unbind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    kazoo_bindings:unbind(<<"v1.blackhole.", Binding/binary>>, Module, Fun, Payload).

-spec flush() -> 'ok'.
flush() ->
    _ = [ kazoo_bindings:flush(Binding) || Binding <- list() ],
    'ok'.

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> kazoo_bindings:flush(<<"v1.blackhole.", Binding/binary>>).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(BHMod) -> kazoo_bindings:flush(BHMod).

filter(Predicate) ->
    kazoo_bindings:filter(Predicate).

-spec modules_loaded() -> atoms().
modules_loaded() -> kazoo_bindings:modules_loaded().

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing blackhole bindings"),
    kz_util:put_callid(?LOG_SYSTEM_ID),
    _ = [init_mod(Mod)
         || Mod <- kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"autoload_modules">>, ?DEFAULT_MODULES)
        ],
    'ok'.

init_mod(ModuleName) ->
    lager:debug("initializing module: ~p", [ModuleName]),
    maybe_init_mod(ModuleName).

maybe_init_mod(ModuleName) ->
    lager:debug("trying to init module: ~p", [ModuleName]),
    try (kz_util:to_atom(ModuleName, 'true')):init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p.", [ModuleName, _E, _R])
    end.

list() ->
    lists:filter(fun(<<"v1.blackhole", _/binary>>) -> 'true'; (_) -> 'false' end, kazoo_bindings:list()).
