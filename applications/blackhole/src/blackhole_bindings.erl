%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
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
-module(blackhole_bindings).

%% API
-export([bind/3, bind/4
        ,unbind/3, unbind/4
        ,map/2, map/3
        ,pmap/2, pmap/3
        ,fold/2
        ,flush/0, flush/1, flush_mod/1
        ,filter/1
        ,modules_loaded/0
        ,init/0, init_mod/1
        ,bindings/0, bindings/1
        ]).

-export([start_link/0]).

%% Helper Functions for Results of a map/2
-export([any/1
        ,all/1
        ,succeeded/1
        ,failed/1
        ]).

-include("blackhole.hrl").

-type payload_el() :: bh_context:context() | kz_term:ne_binary() | map() | kz_json:object().
-type payload() :: [payload_el()] | payload_el().

%%%=============================================================================
%%% API
%%%=============================================================================

-type map_results() :: list().
%% return `[{Result, Payload1}]', a list of tuples, the first element
%% of which is the result of the bound handler, and the second element
%% is the payload, possibly modified

-type kz_bindings() :: list().
%% return `[{Result, Payload1}]', a list of tuples, the first element
%% of which is the result of the bound handler, and the second element
%% is the payload, possibly modified

%%------------------------------------------------------------------------------
%% @doc return the modified Payload after it has been threaded through
%% all matching bindings
%% @end
%%------------------------------------------------------------------------------
-type fold_results() :: payload().
-spec fold(kz_term:ne_binary(), payload()) -> fold_results().
fold(Routing, Payload) ->
    kazoo_bindings:fold(Routing, Payload).

%%------------------------------------------------------------------------------
%% @doc Helper functions for working on a result set of bindings
%% @end
%%------------------------------------------------------------------------------
-spec any(kz_term:proplist()) -> boolean().
any(Res) when is_list(Res) ->
    kazoo_bindings:any(Res, fun check_bool/1).

-spec all(kz_term:proplist()) -> boolean().
all(Res) when is_list(Res) ->
    kazoo_bindings:all(Res, fun check_bool/1).

-spec failed(map_results()) -> map_results().
failed(Res) when is_list(Res) ->
    kazoo_bindings:failed(Res, fun filter_out_succeeded/1);
failed(Res)-> failed([Res]).

-spec succeeded(map_results()) -> map_results().
succeeded(Res) when is_list(Res) ->
    kazoo_bindings:succeeded(Res, fun filter_out_failed/1);
succeeded(Res)-> succeeded([Res]).

%%------------------------------------------------------------------------------
%% @doc Helpers for the result set helpers
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
filter_out_failed([Result]) ->
    case bh_context:is_context(Result) of
        'true' -> not bh_context:success(Result);
        'false' -> not kz_term:is_empty(Result)
    end;
filter_out_failed(Result) ->
    case bh_context:is_context(Result) of
        'true' -> not bh_context:success(Result);
        'false' -> not kz_term:is_empty(Result)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec filter_out_succeeded({boolean() | 'halt', any()} | boolean() | bh_context:context() | [bh_context:context()]) ->
          boolean().
filter_out_succeeded({'true', _}) -> 'false';
filter_out_succeeded('true') -> 'false';
filter_out_succeeded({'halt', _}) -> 'true';
filter_out_succeeded({'false', _}) -> 'true';
filter_out_succeeded('false') -> 'true';
filter_out_succeeded({'EXIT', _}) -> 'true';
filter_out_succeeded([Result]) ->
    case bh_context:is_context(Result) of
        'true' -> bh_context:success(Result);
        'false' -> kz_term:is_empty(Result)
    end;
filter_out_succeeded(Result) ->
    case bh_context:is_context(Result) of
        'true' -> bh_context:success(Result);
        'false' -> kz_term:is_empty(Result)
    end.

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

-spec filter(kazoo_bindings:filter_fun()) -> 'ok'.
filter(Predicate) ->
    kazoo_bindings:filter(Predicate).

-spec flush() -> 'ok'.
flush() ->
    lists:foreach(fun kazoo_bindings:flush_mod/1, modules_loaded()).

-spec flush(kz_term:ne_binary()) -> 'ok'.
flush(Binding) -> kazoo_bindings:flush(Binding).

-spec flush_mod(kz_term:ne_binary() | atom()) -> 'ok'.
flush_mod(BHMod)
  when is_binary(BHMod) ->
    flush_mod(kz_term:to_atom(BHMod, 'true'));
flush_mod(BHMod) -> kazoo_bindings:flush_mod(BHMod).

-spec modules_loaded() -> kz_term:atoms().
modules_loaded() ->
    lists:usort(
      [Mod || Mod <- kazoo_bindings:modules_loaded(),
              is_bh_module(Mod)
      ]).

-spec is_bh_module(kz_term:ne_binary() | atom()) -> boolean().
is_bh_module(<<"bh_", _/binary>>) -> 'true';
is_bh_module(<<"blackhole_", _/binary>>) -> 'true';
is_bh_module(<<_/binary>>) -> 'false';
is_bh_module(Mod) -> is_bh_module(kz_term:to_binary(Mod)).

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing blackhole bindings"),
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),
    Mods = lists:usort(blackhole_config:autoload_modules() ++ ?COMMAND_MODULES),
    lists:foreach(fun init_mod/1, Mods).

-spec init_mod(kz_term:ne_binary() | atom()) ->
          'ok' |
          {'error', 'undefined' | 'unknown'}.
init_mod(ModuleName) ->
    lager:debug("initializing module: ~p", [ModuleName]),
    maybe_init_mod(ModuleName).

-spec maybe_init_mod(kz_term:ne_binary() | atom()) ->
          'ok' |
          {'error', 'undefined' | 'unknown'}.
maybe_init_mod(ModuleName) ->
    Module = kz_term:to_atom(ModuleName, 'true'),
    maybe_init_mod(Module, kz_module:is_exported(Module, 'init', 0)).

maybe_init_mod(Module, 'false') ->
    lager:warning("failed to initalize module ~s", [Module]),
    {'error', 'undefined'};
maybe_init_mod(Module, 'true') ->
    try Module:init() of
        _ -> lager:debug("initialized ~s", [Module])
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p.", [Module, _E, _R]),
            {'error', 'unknown'}
    end.

-spec bindings() -> kazoo_bindings:kz_bindings().
bindings() ->
    bindings(<<"blackhole.#">>).

-spec bindings(kz_term:ne_binary()) -> kazoo_bindings:kz_bindings().
bindings(Routing) ->
    RTOptions = [{'matches', fun bh_match/2}],
    kazoo_bindings:bindings(Routing, RTOptions).


%%------------------------------------------------------------------------------
%% @doc Match routing patterns. `*' matches one slot, `#' matches zero or more.
%% For example pattern `<<"#.6.*.1.4.*">>'  can match `<<"6.a.a.6.a.1.4.a">>'.
%%
%% <div class="notice">Matching only accepts wildcards on first argument (asymetric).</div>
%%
%% This is a copy from {@link kazoo_bindings} with extra
%% checks for `bh_matches([_ | Bs], [<<"*">>|Rs]) ->'
%% @end
%%------------------------------------------------------------------------------
-spec bh_matches(kz_term:ne_binaries(), kz_term:ne_binaries()) -> boolean().

%% if both are empty, we made it!
bh_matches([], []) -> 'true';
bh_matches([<<"#">>], []) -> 'true';

bh_matches([<<"#">>, <<"*">>], []) -> 'false';
bh_matches([<<"#">>, <<"*">>], [<<>>]) -> 'false';
bh_matches([<<"#">>, <<"*">>], [_]) -> 'true'; % match one item:  #.* matches foo

bh_matches([<<"#">> | Bs], []) -> % sadly, #.# would match foo, foo.bar, foo.bar.baz, etc
    bh_matches(Bs, []);           % so keep checking by stripping of the first #

%% if one runs out without a wildcard, no matchy
bh_matches([], [_|_]) -> 'false'; % foo.*   foo
bh_matches([_|_], []) -> 'false';
bh_matches([_|_], [<<>>]) -> 'false';

%% * matches one segment only
bh_matches([<<"*">> | Bs], [_|Rs]) ->
    bh_matches(Bs, Rs); % so ignore what the routing segment is and continue

bh_matches([_ | Bs], [<<"*">>|Rs]) ->
    bh_matches(Bs, Rs); % so ignore what the routing segment is and continue

%% # can match 0 or more segments
bh_matches([<<"#">>, B | Bs], [B | Rs]) ->
    %% Since the segment in B could be repeated later in the Routing Key, we need to bifurcate here
    %% but we'll short circuit if this was indeed the end of the # matching
    %% see binding_bh_matches(<<"#.A.*">>,<<"A.a.A.a">>)

    case lists:member(B, Rs) of
        'true' ->
            bh_matches(Bs, Rs)
                orelse bh_matches([<<"#">> | Bs], Rs);
        'false' ->
            bh_matches(Bs, Rs)
    end;

bh_matches([<<"#">>, <<"*">> | _]=Bs, [_ | Rs]) ->
    bh_matches(Bs, Rs);

bh_matches([<<"#">> | _]=Bs, [_ | Rs]) ->
    bh_matches(Bs, Rs); % otherwise leave the # in to continue matching

%% if the segments match, continue
bh_matches([B | Bs], [B | Rs]) ->
    bh_matches(Bs, Rs);
%% otherwise no match
bh_matches(_, _) -> 'false'.


-spec map(kz_term:ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    RTOptions = [{'matches', fun bh_match/2}],
    kazoo_bindings:map(Routing, Payload, RTOptions).

-spec map(kz_term:ne_binary(), payload(), kz_bindings()) -> map_results().
map(Routing, Payload, Bindings) ->
    RTOptions = [{'matches', fun bh_match/2}
                ,{'candidates', fun(_) -> Bindings end}
                ],
    kazoo_bindings:map(Routing, Payload, RTOptions).

-spec pmap(kz_term:ne_binary(), payload()) -> map_results().
pmap(Routing, Payload) ->
    RTOptions = [{'matches', fun bh_match/2}],
    kazoo_bindings:pmap(Routing, Payload, RTOptions).

-spec pmap(kz_term:ne_binary(), payload(), kz_bindings()) -> map_results().
pmap(Routing, Payload, Bindings) ->
    RTOptions = [{'matches', fun bh_match/2}
                ,{'candidates', fun(_) -> Bindings end}
                ],
    kazoo_bindings:pmap(Routing, Payload, RTOptions).

bh_match(AParts, BParts) ->
    bh_matches(AParts, BParts)
        orelse bh_matches(BParts, AParts).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = init(),
    'ignore'.
