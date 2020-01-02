%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_nodes).
-behaviour(gen_server).

-export([start_link/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_caches.hrl").

-record(state, {name :: atom()
               ,new_node_flush :: boolean()
               ,expire_node_flush :: boolean()
               }).
-type state() :: #state{}.

-spec start_link(atom(), kz_cache:start_options()) -> kz_types:startlink_ret().
start_link(Name, Props) ->
    gen_server:start_link(?MODULE
                         ,#state{name=Name
                                ,new_node_flush=props:is_true('new_node_flush', Props, 'false')
                                ,expire_node_flush=props:is_true('expire_node_flush', Props, 'false')
                                }
                         ,[]
                         ).

-spec init(state()) -> {'ok', state()}.
init(State) ->
    maybe_notify_new(State),
    maybe_notify_expired(State),
    {'ok', State}.

maybe_notify_new(#state{new_node_flush='true'}) ->
    kz_nodes:notify_new();
maybe_notify_new(#state{new_node_flush='false'}) ->
    'ok'.

maybe_notify_expired(#state{expire_node_flush='true'}) ->
    kz_nodes:notify_expire();
maybe_notify_expired(#state{expire_node_flush='false'}) ->
    'ok'.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> {'noreply', state()}.
handle_call(_Req, _From, Name) ->
    {'noreply', Name}.

-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'kz_nodes', {'expire', #kz_node{node=_Node}}}
           ,#state{name=Name
                  ,expire_node_flush='true'
                  }=State
           ) ->
    lager:debug("node ~s has expired, flush everything from ~s", [_Node, Name]),
    kz_cache_ets:flush(Name),
    {'noreply', State};
handle_cast({'kz_nodes', {'new', #kz_node{node=_Node}}}
           ,#state{name=Name
                  ,new_node_flush='true'
                  }=State) ->
    lager:debug("new node ~s, flush everything from ~s", [_Node, Name]),
    kz_cache_ets:flush(Name),
    {'noreply', State};
handle_cast(_Req, Name) ->
    {'noreply', Name}.

-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Msg, Name) ->
    {'noreply', Name}.

-spec terminate(atom(), any()) -> 'ok'.
terminate(_Name, _Reason) ->
    lager:info("terminating: ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
