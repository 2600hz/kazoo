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

%% API
-export([bind/3,bind/4
        ,unbind/3,unbind/4
        ,map/2, map/3
        ,fold/2
        ,flush/0, flush/1, flush_mod/1
        ,filter/1
        ,modules_loaded/0
        ,init/0
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

-define(BH_MODULES,
        kapps_config:get(?BLACKHOLE_CONFIG_CAT, <<"autoload_modules">>, ?DEFAULT_MODULES ++ ?COMMAND_MODULES)).

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
-type kz_bindings() :: list().

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
    kazoo_bindings:succeeded(Res, fun filter_out_failed/1);
succeeded(#bh_context{}=Ctx) ->
    case bh_context:success(Ctx) of
        'true' -> [Ctx];
        'false' -> []
    end.

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
filter_out_failed(#bh_context{}=Ctx) ->
    not bh_context:success(Ctx);
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
filter_out_succeeded(#bh_context{}=Ctx) ->
    bh_context:success(Ctx);
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
    kazoo_bindings:bind(Binding, Module, Fun, Payload).

-spec unbind(ne_binary() | ne_binaries(), atom(), atom()) -> 'ok'.
unbind(Bindings, Module, Fun) ->
    unbind(Bindings, Module, Fun, 'undefined').

-spec unbind(ne_binary() | ne_binaries(), atom(), atom(), any()) -> 'ok'.
unbind([_|_]=Bindings, Module, Fun, Payload) ->
    _ = [unbind(Binding, Module, Fun, Payload) || Binding <- Bindings],
    'ok';
unbind(Binding, Module, Fun, Payload) when is_binary(Binding) ->
    kazoo_bindings:unbind(Binding, Module, Fun, Payload).

filter(Predicate) ->
    kazoo_bindings:filter(Predicate).

-spec flush() -> 'ok'.
flush() ->
    lists:foreach(fun kazoo_bindings:flush_mod/1, modules_loaded()).

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> kazoo_bindings:flush(Binding).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(BHMod) -> kazoo_bindings:flush(BHMod).

-spec modules_loaded() -> atoms().
modules_loaded() ->
    lists:usort(
      [Mod || Mod <- kazoo_bindings:modules_loaded(),
              is_bh_module(Mod)
      ]).

-spec is_bh_module(ne_binary() | atom()) -> boolean().
is_bh_module(<<"bh_", _/binary>>) -> 'true';
is_bh_module(<<"blackhole_", _binary>>) -> 'true';
is_bh_module(<<_/binary>>) -> 'false';
is_bh_module(Mod) -> is_bh_module(kz_util:to_binary(Mod)).

-spec init() -> 'ok'.
init() ->
    lager:debug("initializing blackhole bindings"),
    kz_util:put_callid(?LOG_SYSTEM_ID),
    lists:foreach(fun init_mod/1, ?BH_MODULES ++ ?COMMAND_MODULES).

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

-spec bindings() -> kazoo_bindings:kz_bindings().
bindings() ->
    kazoo_bindings:bindings(<<"blackhole.#">>).

-spec bindings(ne_binary()) -> kazoo_bindings:kz_bindings().
bindings(Routing) ->
    kazoo_bindings:bindings(Routing).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Match routing patterns. * matches 1 slot, # 0 or more.
%% Note: matching only accepts wilcards on first argument (asymetric).
%% @end
%%
%% <<"#.6.*.1.4.*">>,<<"6.a.a.6.a.1.4.a">>
%%
%% this is a copy from kazoo_bindings with extra
%% check for bh_matches([_ | Bs], [<<"*">>|Rs]) ->
%%--------------------------------------------------------------------
-spec bh_matches(ne_binaries(), ne_binaries()) -> boolean().

%% if both are empty, we made it!
bh_matches([], []) -> 'true';
bh_matches([<<"#">>], []) -> 'true';

bh_matches([<<"#">>, <<"*">>], []) -> 'false';
bh_matches([<<"#">>, <<"*">>], [<<>>]) -> 'false';
bh_matches([<<"#">>, <<"*">>], [_]) -> 'true'; % match one item:  #.* matches foo

bh_matches([<<"#">> | Bs], []) -> % sadly, #.# would match foo, foo.bar, foo.bar.baz, etc
    bh_matches(Bs, []);           % so keep checking by stipping of the first #

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


-spec map(ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    RTOptions = [{'matches', fun bh_match/2}],
    kazoo_bindings:map(Routing, Payload, RTOptions).

-spec map(ne_binary(), payload(), kz_bindings()) -> map_results().
map(Routing, Payload, Bindings) ->
    RTOptions = [{'matches', fun bh_match/2}
                ,{'candidates', fun(_) -> Bindings end}
                ],
    kazoo_bindings:map(Routing, Payload, RTOptions).

bh_match(AParts, BParts) ->
    bh_matches(AParts, BParts)
        orelse bh_matches(BParts, AParts).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = init(),
    'ignore'.
