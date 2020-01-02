%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_shared).
-behaviour(gen_listener).

%% API
-export([start_link/4
        ,ack/2
        ,nack/2
        ,deliveries/1
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

-record(state, {fsm_pid :: kz_term:api_pid()
               ,deliveries = [] :: deliveries()
               }).
-type state() :: #state{}.

-define(SHARED_BINDING_OPTIONS(Priority)
       ,[{'consume_options', [{'no_ack', 'false'}
                             ,{'exclusive', 'false'}
                             ]}
        ,{'basic_qos', 1}
        ,{'queue_options', [{'exclusive', 'false'}
                           ,{'arguments', [{<<"x-message-ttl">>, ?MILLISECONDS_IN_DAY}
                                          ,{<<"x-max-length">>, 1000}
                                          ,{<<"x-max-priority">>, Priority}
                                          ]
                            }
                           ]
         }
        ]).

-define(SHARED_QUEUE_BINDINGS(AcctId, QueueId), [{'self', []}]).

-define(RESPONDERS, [{{'acdc_queue_handler', 'handle_member_call'}
                     ,[{<<"member">>, <<"call">>}]
                     }
                    ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:startlink_ret().
start_link(WorkerSup, _, AccountId, QueueId) ->
    {'ok', QueueJObj} = kz_datamgr:open_cache_doc(kzs_util:format_account_db(AccountId), QueueId),
    Priority = kz_json:get_integer_value(<<"max_priority">>, QueueJObj),
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?SHARED_QUEUE_BINDINGS(AccountId, QueueId)}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', kapi_acdc_queue:shared_queue_name(AccountId, QueueId)}
                             | ?SHARED_BINDING_OPTIONS(Priority)
                            ]
                           ,[WorkerSup]
                           ).

-spec ack(kz_types:server_ref(), gen_listener:basic_deliver()) -> 'ok'.
ack(Srv, Delivery) ->
    gen_listener:ack(Srv, Delivery),
    gen_listener:cast(Srv, {'ack', Delivery}).

-spec nack(kz_types:server_ref(), gen_listener:basic_deliver()) -> 'ok'.
nack(Srv, Delivery) ->
    gen_listener:nack(Srv, Delivery),
    gen_listener:cast(Srv, {'noack', Delivery}).

-spec deliveries(kz_types:server_ref()) -> deliveries().
deliveries(Srv) ->
    gen_listener:call(Srv, 'deliveries').

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([pid()]) -> {'ok', state()}.
init([WorkerSup]) ->
    kz_log:put_callid(?DEFAULT_LOG_SYSTEM_ID),

    lager:debug("shared queue proc started"),
    gen_listener:cast(self(), {'get_fsm_proc', WorkerSup}),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('deliveries', _From, #state{deliveries=Ds}=State) ->
    {'reply', Ds, State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'get_fsm_proc', WorkerSup}, State) ->
    FSMPid = acdc_queue_worker_sup:fsm(WorkerSup),
    lager:debug("sending messages to FSM ~p", [FSMPid]),
    {'noreply', State#state{fsm_pid=FSMPid}};
handle_cast({'delivery', Delivery}, #state{deliveries=Ds}=State) ->
    {'noreply', State#state{deliveries=[Delivery|Ds]}};
handle_cast({'ack', Delivery}, #state{deliveries=Ds}=State) ->
    {'noreply', State#state{deliveries=lists:delete(Delivery, Ds)}};
handle_cast({'noack', Delivery}, #state{deliveries=Ds}=State) ->
    {'noreply', State#state{deliveries=lists:delete(Delivery, Ds)}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_Q}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'basic.cancel',_,'true'}, State) ->
    lager:debug("recv basic.cancel...no!!!"),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all messages from the message bus
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{fsm_pid=FSM}) ->
    {'reply', [{'fsm_pid', FSM}]}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{deliveries=Ds}) ->
    _ = [catch kz_amqp_util:basic_nack(Delivery) || Delivery <- Ds],
    lager:debug("acdc_queue_shared terminating: ~p", [_Reason]).

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
