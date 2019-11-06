%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc monitors usurp_control
%%%
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_usurp_monitor).
-behaviour(gen_listener).

-compile({no_auto_import,[register/2]}).

%% API
-export([start_link/0]).

-export([register/3, register/4]).

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-type state() :: map().

-type usurp_type() :: 'usurp_control' | 'usurp_publisher'.
-type cache_key() :: {usurp_type(), kz_term:ne_binary()}.

-record(cache, {key :: cache_key()
               ,ref_id :: kz_term:ne_binary()
               ,pid :: pid()
               }).
-type cache() :: #cache{}.

-define(BINDINGS, [{'call', [{'restrict_to', ['usurp_control'
                                             ,'usurp_publisher'
                                             ]}
                            ,'federate'
                            ]}
                  ]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE,
                            [{'responders', ?RESPONDERS}
                            ,{'bindings', ?BINDINGS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ], []).

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?SERVER),
    lager:debug("starting usurp monitor"),
    {'ok', #{calls => ets:new('calls', ['set', {'keypos', #cache.key}])
            ,pids => ets:new('pids', ['set', {'keypos', #cache.pid}])
            }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({register, Type, CallId, RefId, Pid}, State) ->
    kz_log:put_callid(CallId),
    {'noreply', handle_register(#cache{key={Type, CallId}, ref_id=RefId, pid=Pid}, State)};
handle_cast(_, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', _Ref, 'process', Pid, _Reason}, State) ->
    {'noreply', handle_unregister(Pid, State)};
handle_info(_Msg, State) ->
    lager:debug("unhandled message: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, #{calls := Calls}) ->
    kz_log:put_callid(JObj),
    CallId = kz_call_event:call_id(JObj),
    Type = usurp_type(JObj),
    RefId = usurp_reference(Type, JObj),
    _ = handle_usurp(Type, CallId, RefId, JObj, Calls),
    'ignore'.

-spec handle_usurp(usurp_type(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), ets:tid()) -> 'ok'.
handle_usurp(Type, CallId, RefId, JObj, Calls) ->
    gproc:send({'p', 'l', {'call_control', CallId}}, {Type, CallId, RefId, JObj}),
    _ = [Pid ! {Type, CallId, RefId, JObj} || #cache{pid=Pid} <- ets:lookup(Calls, {Type, CallId})],
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec register(usurp_type(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
register(Type, CallId, RefId) ->
    register(Type, CallId, RefId, self()).

-spec register(usurp_type(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> 'ok'.
register(Type, CallId, RefId, Pid) ->
    gen_listener:cast(?SERVER, {register, Type, CallId, RefId, Pid}).

-spec handle_register(cache(), state()) -> state().
handle_register(#cache{key={Type, CallId}, ref_id=RefId, pid=Pid}=Cache, #{calls := Calls, pids := Pids} = State) ->
    _ = handle_usurp(Type, CallId, RefId, kz_json:new(), Calls),
    _ = ets:insert(Calls, Cache),
    _ = ets:insert(Pids, Cache),
    _ = erlang:monitor(process, Pid),
    State.

-spec handle_unregister(pid(), state()) -> state().
handle_unregister(Pid, #{pids := Pids} = State) ->
    case ets:lookup(Pids, Pid) of
        [#cache{}=Cache] -> unregister(Cache, State);
        _ -> State
    end.

-spec unregister(cache(), state()) -> state().
unregister(#cache{}=Cache, #{calls := Calls, pids := Pids} = State) ->
    _ = ets:delete_object(Calls, Cache),
    _ = ets:delete_object(Pids, Cache),
    State.

-spec usurp_type(kz_json:object()) -> usurp_type().
usurp_type(JObj) ->
    kz_term:to_atom(kz_api:event_name(JObj), 'true').

-spec usurp_reference(usurp_type(), kz_json:object()) -> kz_term:ne_binary().
usurp_reference('usurp_control', JObj) ->
    kz_call_event:fetch_id(JObj);
usurp_reference('usurp_publisher', JObj) ->
    kz_json:get_ne_binary_value(<<"Reference">>, JObj).
