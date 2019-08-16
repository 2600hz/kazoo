%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Sean Wysor
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_openoffice_server).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,add/2
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo_convert/include/kz_convert.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, {'local', ?MODULE}).

-define(TIMEOUT_LIFETIME, ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT_CANCEL_JOB, 120 * ?MILLISECONDS_IN_SECOND).
-define(TIMEOUT_DEQUEUE, 'dequeue').
-define(TIMEOUT_CANCEL, 'cancel_job').
-define(SERVER_TIMEOUT, 10 * ?MILLISECONDS_IN_SECOND).

-record(state, {queue = queue:new() :: queue:queue()
               ,timer_ref ::  reference()
               }).
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
    gen_server:start_link(?SERVER, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @doc convert an openoffice document, respecting the one document at a time
%% constraint.
%%
%% Openoffice does not permit simultaneous conversions, so this gen_server is a
%% serialization mechanism for conversions from openoffice formats.
%% @end
%%------------------------------------------------------------------------------
-spec add(kz_term:ne_binary(), map()) -> {'ok', kz_term:ne_binary()}|{'error', kz_term:ne_binary()}.
add(Source, Options) ->
    gen_server:call(?MODULE, {'add', Source, Options}, ?SERVER_TIMEOUT).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()} |
                      {'stop', any()}.
init([]) ->
    {'ok', #state{
              'timer_ref' = start_timer(?TIMEOUT_DEQUEUE)
             }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call({atom(), {'file', kz_term:ne_binary()}, map()}, kz_term:pid_ref(), state()) ->
                         kz_types:handle_call_ret_state(state()).
handle_call('stop', _From, #state{} = State) ->
    {'stop', 'normal', 'ok', State};
handle_call({'add', Content, Options}, From, #state{queue=Queue}=State) ->
    DropTimerRef = start_timer(?TIMEOUT_CANCEL),
    {'noreply', State#state{queue=queue:in({DropTimerRef, From, Content, Options}, Queue)}}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', TRef, ?TIMEOUT_DEQUEUE}, #state{queue=Queue, timer_ref=TRef}=State) ->
    case queue:out(Queue) of
        {'empty', _} ->
            {'noreply', State#state{timer_ref=start_timer(?TIMEOUT_DEQUEUE)}};
        {{'value', {DropTimerRef, From, Content, Options}}, NewQueue} ->
            _ = erlang:cancel_timer(DropTimerRef),
            gen_server:reply(From, kz_fax_converter:do_openoffice_to_pdf(Content, Options)),
            {'noreply', State#state{timer_ref=start_timer(?TIMEOUT_DEQUEUE), queue=NewQueue}}
    end;
handle_info({'timeout', DropTimerRef, ?TIMEOUT_CANCEL}, #state{queue=Queue}=State) ->
    Fun = fun({TimerRef, _, _, _}) when DropTimerRef =:= TimerRef -> false;
             (_) -> true
          end,
    {'noreply', State#state{queue=queue:filter(Fun, Queue)}};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State, 'hibernate'}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{timer_ref=TimerRef}) ->
    _ = stop_timer(TimerRef),
    lager:debug("openoffice_server going down: ~p", [_Reason]).

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
-spec start_timer(atom()) -> reference().
start_timer(?TIMEOUT_DEQUEUE) ->
    erlang:start_timer(?TIMEOUT_LIFETIME, self(), ?TIMEOUT_DEQUEUE);
start_timer(?TIMEOUT_CANCEL) ->
    erlang:start_timer(?TIMEOUT_CANCEL_JOB, self(), ?TIMEOUT_CANCEL).


-spec stop_timer(reference()) -> integer() | boolean() | 'ok'.
stop_timer(Ref) ->
    erlang:cancel_timer(Ref).
