%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_task).

-behaviour(gen_listener).

-export([start_link/3
        ,relay_amqp/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-record(state, {call :: kapps_call:call()
               ,callback :: fun()
               ,args :: list()
               ,pid :: kz_term:api_pid()
               ,ref :: kz_term:api_reference()
               ,queue :: kz_term:api_binary()
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallId), [{'call', [{'callid', CallId}]}
                          ,{'self', []}
                          ]).
-define(RESPONDERS, [{{?MODULE, 'relay_amqp'}
                     ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%------------------------------------------------------------------------------
%% @doc Starts the listener and binds to the call channel destroy events.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(kapps_call:call(), fun(), list()) -> kz_types:startlink_ret().
start_link(Call, Fun, Args) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(kapps_call:call_id(Call))}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ]
                           ,[Call, Fun, Args]
                           ).

%%------------------------------------------------------------------------------
%% @doc Handles call events (typically triggered by a FreeSWITCH event).
%% For the purposes of the singular hook listener, we are only interested in
%% `CHANNEL_DESTROY'.
%% @end
%%------------------------------------------------------------------------------
-spec relay_amqp(kz_json:object(), kz_term:proplist()) -> any().
relay_amqp(JObj, Props) ->
    Pid = props:get_value('cf_task_pid', Props),
    kapps_call_command:relay_event(Pid, JObj).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the listener, and sends the init hook.
%% @end
%%------------------------------------------------------------------------------
-spec init([fun()]) -> {'ok', state()}.
init([Call, Callback, Args]) ->
    _ = kapps_call:put_callid(Call),
    lager:debug("started event listener for cf_task"),
    {'ok', #state{call=Call
                 ,callback=Callback
                 ,args=Args
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handle call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), any(), state()) ->
                         {'reply', {'error', 'not_implemented'}, state()}.
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) ->
                         {'noreply', state()} |
                         {'stop', 'normal', state()}.
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    {'noreply', launch_task(State)};
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info({'DOWN', Ref, 'process', Pid, Reason}
           ,#state{ref=Ref
                  ,pid=Pid
                  }=State
           ) ->
    lager:debug("task in ~p (~p) exited with reason: ~p", [Pid, Ref, Reason]),
    {'stop', 'normal', State};
handle_info(Info, State) ->
    lager:debug("unhandled message: ~p", [Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> {'reply', kz_term:proplist()}.
handle_event(_JObj, #state{pid='undefined'}) -> 'ignore';
handle_event(_JObj, #state{pid=Pid}) ->
    {'reply', [{'cf_task_pid', Pid}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> any().
terminate(_Reason, _State) ->
    lager:debug("callflow task terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec launch_task(state()) -> state().
launch_task(#state{queue=Q
                  ,call=Call
                  ,callback=Callback
                  ,args=Args
                  }=State
           ) ->
    {Pid, Ref} = kz_process:spawn_monitor(fun task_launched/5, [Q, Call, Callback, Args, self()]),
    lager:debug("watching task execute in ~p (~p)", [Pid, Ref]),
    State#state{pid=Pid, ref=Ref}.

-spec task_launched(kz_term:api_binary(), kapps_call:call(), fun(), list(), pid()) -> any().
task_launched(Q, Call, Callback, Args, Parent) ->
    kapps_call:put_callid(Call),
    _ = kz_amqp_channel:consumer_pid(Parent),
    Funs = [{fun kapps_call:kvs_store/3, 'consumer_pid', Parent}
           ,{fun kapps_call:set_controller_queue/2, Q}
           ],
    apply(Callback, Args ++ [kapps_call:exec(Funs, Call)]).
