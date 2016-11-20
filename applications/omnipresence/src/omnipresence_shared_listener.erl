%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnipresence_shared_listener).
-behaviour(gen_listener).

-export([start_link/0]).
-export([start_listener/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("omnipresence.hrl").
-include_lib("kazoo_apps/include/kz_hooks.hrl").

-define(SERVER, ?MODULE).

-record(state, {subs_pid :: pid()
               ,subs_ref :: reference()
               }).
-type state() :: #state{}.

-define(BINDINGS, [{'self', []}
                   %% channel events that toggle presence lights
                  ,{'call', [{'restrict_to', ['CHANNEL_CREATE'
                                             ,'CHANNEL_ANSWER'
                                             ,'CHANNEL_DESTROY'
                                             ,'CHANNEL_CONNECTED'
                                             ,'CHANNEL_DISCONNECTED'
                                             ]
                             }
                            ,'federate'
                            ]}
                  ,{'presence', [{'restrict_to', ['update'
                                                 ,'mwi_update'
                                                 ,'reset'
                                                 ,'flush'
                                                 ,'search_req'
                                                 ]
                                 }
                                ,'federate'
                                ]}
                  ,{'omnipresence', [{'restrict_to', ['subscribe']}]}
                  ]).
-define(RESPONDERS, [{{'omnip_subscriptions', 'handle_channel_event'}
                     ,[{<<"call_event">>, <<"*">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_presence_update'}
                     ,[{<<"presence">>, <<"update">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_mwi_update'}
                     ,[{<<"presence">>, <<"mwi_update">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_reset'}
                     ,[{<<"presence">>, <<"reset">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_flush'}
                     ,[{<<"presence">>, <<"flush">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_kamailio_subscribe'}
                     ,[{<<"presence">>, <<"subscription">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_search_req'}
                     ,[{<<"presence">>, <<"search_req">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<"omnip_shared_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(LISTENER_PARAMS, [{'bindings', ?BINDINGS}
                         ,{'responders', ?RESPONDERS}
                         ,{'queue_name', ?QUEUE_NAME}
                         ,{'queue_options', ?QUEUE_OPTIONS}
                         ,{'consume_options', ?CONSUME_OPTIONS}
                         ]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_listener() -> 'ok'.
start_listener() ->
    gen_listener:cast(?SERVER, {'ready'}).

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    gen_listener:cast(self(), 'find_subscriptions_srv'),
    lager:debug("omnipresence_listener started"),
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('find_subscriptions_srv', #state{subs_pid=_Pid}=State) ->
    case omnipresence_sup:subscriptions_srv() of
        'undefined' ->
            lager:debug("no subs_pid"),
            gen_listener:cast(self(), 'find_subscriptions_srv'),
            timer:sleep(500),
            {'noreply', State#state{subs_pid='undefined'}};
        P when is_pid(P) ->
            lager:debug("new subs pid: ~p", [P]),
            {'noreply', State#state{subs_pid=P
                                   ,subs_ref=erlang:monitor('process', P)
                                   }}
    end;
handle_cast({'gen_listener',{'created_queue',_Queue}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'ready'}, State) ->
    gen_listener:start_listener(self(), ?LISTENER_PARAMS),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'DOWN', Ref, 'process', Pid, _R}, #state{subs_pid=Pid
                                                     ,subs_ref=Ref
                                                     }=State) ->
    gen_listener:cast(self(), 'find_subscriptions_srv'),
    {'noreply', State#state{subs_pid='undefined'
                           ,subs_ref='undefined'
                           }};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{subs_pid=S}) ->
    {'reply', [{'omnip_subscriptions', S}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
