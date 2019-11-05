%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_leader_listener).

-behaviour(gen_listener).

-export([is_ready/0]).
-export([start_link/1]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("amqp_leader.hrl").

-define(SERVER, ?MODULE).

-record(state, {self = self()          :: pid()
               ,name                   :: atom()
               ,pending = []           :: [{pid(), any()}]
               ,has_queue = 'false'    :: boolean()
               ,is_consuming = 'false' :: boolean()
               }).

-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(Name), [{'leader', [{'name', Name}]}]).
-define(RESPONDERS, []).
-define(QUEUE_NAME(Name), kapi_leader:queue(Name)).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_ready() -> 'true'.
is_ready() ->
    {'registered_name', Name} = erlang:process_info(self(), 'registered_name'),
    Sup = element(2, hd([X || X <- supervisor:which_children('amqp_leader_sup'), element(1, X) =:= Name])),
    Pid = element(2, hd([X || X <- supervisor:which_children(Sup), element(1, X) =:= ?MODULE])),
    Ref = make_ref(),
    gen_listener:cast(Pid, {'is_ready', self(), Ref}),
    recv_ready(Ref).

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(atom() | pid()) -> kz_types:startlink_ret().
start_link(Name) ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS(Name)}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME(Name)}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                      %%,{basic_qos, 1}                % only needed if prefetch controls
                                     ], [Name]).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([atom()]) -> {'ok', state()}.
init([Name]) ->
    kz_log:put_callid(kapi_leader:queue(Name, node())),
    kz_nodes:notify_expire(),
    {'ok', #state{self = self(), name = Name}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), {pid(), any()}, state()) -> kz_types:handle_call_ret_state(state()).
handle_call('is_ready', From, #state{pending = Pids} = State) ->
    NewState = maybe_ready(State#state{pending = [From | Pids]}),
    {'noreply', NewState};
handle_call(_Request, _From, State) ->
    lager:warning("unhandled call ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'is_ready', Pid, Ref}, #state{pending = Pending} = State) ->
    NewState = maybe_ready(State#state{pending = [{Pid, Ref} | Pending]}),
    {'noreply', NewState};
handle_cast({'kz_nodes', {'expire', #kz_node{node = Node}}}, #state{name = Name} = State) ->
    Name ! {'DOWN', Node},
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    lager:debug("connected"),
    NewState = maybe_ready(State#state{has_queue = 'true'}),
    {'noreply', NewState};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    lager:debug("consuming"),
    NewState = maybe_ready(State#state{is_consuming = 'true'}),
    {'noreply', NewState};
handle_cast(_Msg, State) ->
    lager:warning("unhandled cast ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:warning("~s unhandled info ~p", [node(), _Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> {'reply', []}.
handle_event(JObj, #state{name = Name}) ->
    kz_log:put_callid(kapi_leader:queue(Name, node())),
    NodeBin = kz_term:to_binary(node()),
    case kz_json:get_value(<<"Node">>, JObj) of
        NodeBin -> 'ok';
        _ ->
            Msg = erlang:binary_to_term(kz_binary:from_hex(kz_json:get_value(<<"Message">>, JObj))),
            Name ! Msg
    end,
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed/
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
-spec recv_ready(reference()) -> 'true'.
recv_ready(Ref) ->
    receive
        {Ref, 'ready'} -> ready_when_node_is_up()
    after
        1 * ?MILLISECONDS_IN_SECOND ->
            lager:debug("listener not ready yet"),
            recv_ready(Ref)
    end.

-spec ready_when_node_is_up() -> 'true'.
ready_when_node_is_up() ->
    case kz_nodes:is_up(node()) of
        'false' ->
            lager:debug("node is not up yet"),
            timer:sleep(1 * ?MILLISECONDS_IN_SECOND),
            ready_when_node_is_up();
        'true' ->
            'true'
    end.

-spec maybe_ready(state()) -> state().
maybe_ready(#state{pending = Pids
                  ,has_queue = 'true'
                  ,is_consuming = 'true'
                  } = State) ->
    _ = [gen_server:reply(Pid, 'ready') || Pid <- Pids],
    State#state{pending = []};
maybe_ready(#state{has_queue='true'}=State) ->
    lager:debug("not consuming"),
    State;
maybe_ready(#state{has_queue='false'}=State) ->
    lager:debug("no queue"),
    State.
