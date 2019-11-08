%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(listener_federator).

-behaviour(gen_listener).

-export([start_link/3
        ,stop/1
        ,broker/1
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/4
        ,terminate/2
        ,code_change/3
        ]).

-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(SERVER, ?MODULE).

-record(state, {parent :: pid()
               ,broker :: kz_term:ne_binary()
               ,self_binary = kz_term:to_binary(pid_to_list(self())) :: kz_term:ne_binary()
               ,zone :: kz_term:ne_binary()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(pid(), kz_term:ne_binary(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Parent, Broker, Params) ->
    ParentCallId = kz_util:get_callid(),
    gen_listener:start_link(?SERVER, Params, [Parent, ParentCallId, Broker]).

-spec broker(kz_types:server_ref()) -> kz_term:ne_binary().
broker(Pid) ->
    gen_listener:call(Pid, 'get_broker').

-spec stop(kz_types:server_ref()) -> 'ok'.
stop(Pid) ->
    gen_listener:call(Pid, {'stop', self()}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([pid() | kz_term:ne_binary()]) -> {'ok', state()}.
init([Parent, ParentCallId, Broker]=L) ->
    lager:debug("federating listener ~p(~s) on broker ~s", L),
    _ = kz_amqp_channel:consumer_broker(Broker),
    Zone = kz_term:to_binary(kz_amqp_connections:broker_zone(Broker)),

    CallId = kz_binary:join([ParentCallId, Zone], <<"-">>),
    kz_util:put_callid(CallId),

    {'ok', #state{parent=Parent
                 ,broker=Broker
                 ,zone=Zone
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), any(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'stop', Parent}, _From, #state{parent=Parent}=State) ->
    {'stop', 'normal', 'ok', State};
handle_call('get_broker', _From, #state{broker=Broker}=State) ->
    {'reply', Broker, State};
handle_call(_Request, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'gen_listener', {'created_queue', _}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, #state{parent=Parent, broker=Broker}=State) ->
    gen_server:cast(Parent, {'federator_is_consuming', Broker, 'true'}),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Info, State) ->
    lager:info("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), gen_listener:basic_deliver(), amqp_basic(), state()) -> gen_listener:handle_event_return().
handle_event(JObj, BasicDeliver, BasicData, #state{parent=Parent
                                                  ,broker=Broker
                                                  ,self_binary=Self
                                                  ,zone=Zone
                                                  }) ->
    lager:debug("relaying federated ~s event (~p) ~s from ~s to ~p with consumer pid ~p",
                [kz_api:event_category(JObj), kz_api:msg_id(JObj), kz_api:event_name(JObj), Zone, Parent, Self]
               ),
    RemoteServerId = list_to_binary(["consumer://"
                                    ,Self, "/"
                                    ,kz_json:get_ne_binary_value(<<"Server-ID">>, JObj, <<>>)
                                    ]),
    gen_listener:federated_event(Parent
                                ,kz_json:set_values([{<<"Server-ID">>, RemoteServerId}
                                                    ,{<<"AMQP-Broker">>, Broker}
                                                    ,{<<"AMQP-Broker-Zone">>, Zone}
                                                    ]
                                                   ,JObj
                                                   )
                                ,BasicDeliver
                                ,BasicData
                                ),
    'ignore'.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate('shutdown', _State) -> 'ok';
terminate(_Reason, _State) ->
    lager:debug("listener federator terminating: ~p", [_Reason]).

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
