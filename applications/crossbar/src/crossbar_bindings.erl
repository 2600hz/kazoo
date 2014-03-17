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
%%%-------------------------------------------------------------------
-module(crossbar_bindings).

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

-include("crossbar.hrl").

-type payload() :: path_tokens() | % mapping over path tokens in URI
                   [cb_context:context() | path_token() | 'undefined',...] |
                   cb_context:context() |
                   {cb_context:context(), wh_proplist()} | % v1_resource:rest_init/2
                   {'error', _} | % v1_util:execute_request/2
                   {wh_json:json_strings(), cb_context:context(), path_tokens()} |
                   {wh_datetime(), cowboy_req:req(), cb_context:context()} | % v1_resource:expires/2
                   {cowboy_req:req(), cb_context:context()}. % mapping over the request/context records

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
-type map_results() :: [boolean() |
                        http_methods() |
                        {boolean() | 'halt', cb_context:context()}
                        ,...] | [].
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
flush_mod(CBMod) -> kazoo_binding:flush(CBMod).

-spec modules_loaded() -> atoms().
modules_loaded() -> kazoo_bindings:modules_loaded().

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing bindings"),

    put('callid', ?LOG_SYSTEM_ID),
    _ = [maybe_init_mod(Mod)
         || Mod <- whapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, ?DEFAULT_MODULES)
    ],
    'ok'.

maybe_init_mod(ModBin) ->
    try (wh_util:to_atom(ModBin, 'true')):init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p. Trying other versions...", [ModBin, _E, _R]),
            maybe_init_mod_versions(?VERSION_SUPPORTED, ModBin)
    end.

maybe_init_mod_versions([], _) -> 'ok';
maybe_init_mod_versions([Version|Versions], ModBin) ->
    Module = <<ModBin/binary, "_", Version/binary>>,
    try (wh_util:to_atom(Module, 'true')):init() of
        _ -> maybe_init_mod_versions(Versions, ModBin)
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p", [Module, _E, _R]),
            maybe_init_mod_versions(Versions, ModBin)
    end.
