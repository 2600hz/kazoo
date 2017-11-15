%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
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
        ,map/2, pmap/2
        ,fold/2
        ,flush/0, flush/1, flush_mod/1
        ,modules_loaded/0
        ]).

-export([start_link/0
        ,init/0
        ]).

%% Helper Functions for Results of a map/2
-export([any/1
        ,all/1
        ,succeeded/1
        ,failed/1
        ,matches/2
        ]).

-include("crossbar.hrl").

-type payload() :: path_tokens() | % mapping over path tokens in URI
                   [cb_context:context() | path_token() | 'undefined',...] |
                   cb_context:context() |
                   {cb_context:context(), kz_proplist()} | % v1_resource:rest_init/2
                   {'error', _} | % v1_util:execute_request/2
                   {kz_json:path(), cb_context:context(), path_tokens()} |
                   {kz_datetime(), cowboy_req:req(), cb_context:context()} | % v1_resource:expires/2
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
                       ].
-spec map(ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    lager:debug("mapping ~s", [Routing]),
    kazoo_bindings:map(Routing, Payload).

-spec pmap(ne_binary(), payload()) -> map_results().
pmap(Routing, Payload) ->
    lager:debug("pmapping ~s", [Routing]),
    kazoo_bindings:pmap(Routing, Payload).

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
    lager:debug("folding ~s", [Routing]),
    kazoo_bindings:fold(Routing, Payload).

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

-spec succeeded(map_results()) -> map_results().
succeeded(Res) when is_list(Res) ->
    Successes = kazoo_bindings:succeeded(Res, fun filter_out_failed/1),
    case props:get_value('halt', Successes) of
        'undefined' -> Successes;
        HaltContext -> [{'halt', HaltContext}]
    end.

-spec failed(map_results()) -> map_results().
failed(Res) when is_list(Res) ->
    kazoo_bindings:failed(Res, fun filter_out_succeeded/1).

-spec matches(ne_binaries(), ne_binaries()) -> boolean().
matches([], _) -> 'false';
matches([R|Restrictions], Tokens) ->
    Restriction = [cow_qs:urldecode(T) || T <- binary:split(R, <<"/">>, ['global', 'trim'])],
    kazoo_bindings:matches(Restriction, Tokens)
        orelse matches(Restrictions, Tokens).

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
filter_out_failed(Term) -> not kz_term:is_empty(Term).

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
filter_out_succeeded(Term) -> kz_term:is_empty(Term).

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result()].
-spec bind(ne_binary(), atom(), atom()) ->
                  bind_result() | bind_results().
bind(Binding=?NE_BINARY, Module, Fun) ->
    kazoo_bindings:bind(Binding, Module, Fun).

-spec flush() -> 'ok'.
flush() ->
    lists:foreach(fun kazoo_bindings:flush_mod/1, modules_loaded()).

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> kazoo_bindings:flush(Binding).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(CBMod) -> kazoo_bindings:flush_mod(CBMod).

-spec modules_loaded() -> atoms().
modules_loaded() ->
    lists:usort(
      [Mod || Mod <- kazoo_bindings:modules_loaded(),
              is_cb_module(Mod)
      ]).

-spec is_cb_module(ne_binary() | atom()) -> boolean().
is_cb_module(<<"cb_", _/binary>>) -> 'true';
is_cb_module(<<"crossbar_", _/binary>>) -> 'true';
is_cb_module(<<_/binary>>) -> 'false';
is_cb_module(Mod) ->
    is_cb_module(kz_term:to_binary(Mod)).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = init(),
    garbage_collect(self()),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing bindings"),
    kz_util:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    AutoloadModules = crossbar_config:autoload_modules(?DEFAULT_MODULES),
    lists:foreach(fun maybe_init_mod/1, AutoloadModules).

-spec maybe_init_mod(ne_binary() | atom()) -> 'ok'.
maybe_init_mod(Mod) ->
    case crossbar_init:start_mod(Mod) of
        'ok' -> 'ok';
        {'error', Error} ->
            lager:notice("failed to initialize ~s: ~p", [Mod, Error])
    end.
