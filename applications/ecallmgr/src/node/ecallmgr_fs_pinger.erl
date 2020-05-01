%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc When connecting to a FreeSWITCH node, we create three processes: one to
%%% handle authentication (directory) requests; one to handle route (dialplan)
%%% requests, and one to monitor the node and various stats about the node.
%%%
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_pinger).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-record(state, {node = 'undefined' :: atom()
               ,options = [] :: kz_term:proplist()
               ,timeout = 2 * ?MILLISECONDS_IN_SECOND
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    gen_server:start_link(?SERVER, [Node, Options], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom() | kz_term:proplist()]) -> {'ok', state()}.
init([Node, Props]) ->
    kz_log:put_callid(Node),
    self() ! 'initialize_pinger',
    lager:info("node ~s not responding, periodically retrying connection", [Node]),
    {'ok', #state{node=Node, options=Props}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%% #state{nodes=[{FSNode, HandlerPid}]}
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, #state{timeout=Timeout}=State) ->
    {'reply', {'error', 'not_implemented'}, State, Timeout}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, #state{timeout=Timeout}=State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State, Timeout}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info('initialize_pinger', #state{node=Node, options=Props}=State) ->
    kz_notify:system_alert("node ~s disconnected from ~s", [Node, node()]),
    _ = case props:get_value('cookie', Props) of
            'undefined' -> 'ok';
            Cookie when is_atom(Cookie) ->
                lager:debug("setting cookie to ~s for ~s", [Cookie, Node]),
                erlang:set_cookie(Node, Cookie)
        end,
    GracePeriod = kapps_config:get_integer(?APP_NAME, <<"node_down_grace_period">>, 10 * ?MILLISECONDS_IN_SECOND),
    erlang:send_after(GracePeriod, self(), {'flush_channels', Node}),
    erlang:send_after(3 * ?MILLISECONDS_IN_SECOND, self(), 'check_node_status'),
    {'noreply', State};
handle_info({'flush_channels', Node}, State) ->
    lager:info("node ~s has been down past the grace period, flushing channels", [Node]),
    ecallmgr_fs_channels:flush_node(Node),
    ecallmgr_fs_conferences:flush_node(Node),
    {'noreply', State};
handle_info('check_node_status', #state{timeout=Timeout}=State)
  when Timeout > ?MILLISECONDS_IN_MINUTE * 15 ->
    handle_info('check_node_status', State#state{timeout=?MILLISECONDS_IN_MINUTE});
handle_info('check_node_status', #state{node=Node, timeout=Timeout}=State) ->
    case net_adm:ping(Node) of
        'pong' ->
            %% give the node a moment to init
            timer:sleep(?MILLISECONDS_IN_SECOND),
            kz_notify:system_alert("node ~s connected to ~s", [Node, node()]),
            'ok' = ecallmgr_fs_nodes:nodeup(Node),
            {'stop', 'normal', State};
        _Else ->
            lager:debug("node ~s not responding, waiting ~b seconds to ping again", [Node, Timeout div ?MILLISECONDS_IN_SECOND]),
            erlang:send_after(Timeout, self(), 'check_node_status'),
            {'noreply', State#state{timeout=Timeout+?MILLISECONDS_IN_SECOND}, 'hibernate'}
    end;
handle_info('exit', State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs pinger ~p to '~s' termination", [_Reason, Node]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
