%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_port_request_crawler).
-behaviour(gen_server).

%% API
-export([start_link/0
        ,stop/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").
-include_lib("kazoo_numbers/include/knm_phone_number.hrl").

-define(CLEANUP_ROUNDTRIP_TIME
       ,kapps_config:get_integer(?CONFIG_CAT, <<"crawler_delay_time_ms">>, ?MILLISECONDS_IN_MINUTE)
       ).

-record(state, {cleanup_ref :: reference()}).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?MODULE, 'stop').

cleanup_timer() ->
    erlang:start_timer(?CLEANUP_ROUNDTRIP_TIME, self(), 'ok').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    {'ok', #state{cleanup_ref=cleanup_timer()}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    lager:debug("crawler has been stopped"),
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', Ref, _Msg}, #state{cleanup_ref=Ref}=State) ->
    _ = kz_process:spawn(fun crawl_port_requests/0),
    {'noreply', State#state{cleanup_ref=cleanup_timer()}, 'hibernate'};
handle_info(_Msg, State) ->
    lager:debug("unhandled msg: ~p", [_Msg]),
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
terminate(_Reason, _State) ->
    lager:debug("~s terminating: ~p", [?MODULE, _Reason]).

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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec crawl_port_requests() -> 'ok'.
crawl_port_requests() ->
    Start = kz_time:start_time(),
    knm_port_request:send_submitted_requests(),
    lager:info("port_request crawler completed a full crawl in ~pms", [kz_time:elapsed_ms(Start)]).
