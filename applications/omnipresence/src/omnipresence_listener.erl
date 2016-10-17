%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnipresence_listener).
-behaviour(gen_listener).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("omnipresence.hrl").

-define(SERVER, ?MODULE).

-record(state, {subs_pid = 'undefined' :: api_pid()
               ,subs_ref :: reference()
               ,queue = 'undefined' :: api_binary()
               ,consuming = 'false' :: boolean()
               ,sync = 'false' :: boolean()
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'self', []}
                  ,{'presence', [{'restrict_to', ['subscribe']}]}
                  ,{'omnipresence', [{'restrict_to', ['notify']}]}
                  ]).
-define(RESPONDERS, [{{'omnip_subscriptions', 'handle_subscribe'}
                     ,[{<<"presence">>, <<"subscription">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_sync'}
                     ,[{<<"presence">>, <<"sync">>}]
                     }
                    ,{{'omnip_subscriptions', 'handle_kamailio_notify'}
                     ,[{<<"presence">>, <<"notify">>}]
                     }
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(SUBSCRIPTIONS_SYNC_ENABLED, kapps_config:get_is_true(?CONFIG_CAT, <<"subscriptions_sync_enabled">>, 'false')).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

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
            {'noreply', State#state{subs_pid='undefined'}};
        P when is_pid(P) ->
            lager:debug("new subs pid: ~p", [P]),
            gen_listener:cast(self(), 'send_sync'),
            {'noreply', State#state{subs_pid=P
                                   ,subs_ref=erlang:monitor('process', P)
                                   }}
    end;
handle_cast({'gen_listener',{'created_queue',Queue}}, State) ->
    gen_listener:cast(self(), 'send_sync'),
    {'noreply', State#state{queue=Queue}};
handle_cast({'gen_listener',{'is_consuming',IsConsuming}}, State) ->
    gen_listener:cast(self(), 'send_sync'),
    {'noreply', State#state{consuming=IsConsuming}};
handle_cast('send_sync', #state{subs_pid=Pid, queue=Queue, consuming=IsConsuming} = State)
  when Pid =:= 'undefined'
       orelse Queue =:= 'undefined'
       orelse IsConsuming =:= 'false'  ->
    {'noreply', State};
handle_cast('send_sync', #state{subs_pid='undefined'}=State) ->
    {'noreply', State};
handle_cast('send_sync', #state{queue='undefined'}=State) ->
    {'noreply', State};
handle_cast('send_sync', #state{consuming='false'}=State) ->
    {'noreply', State};
handle_cast('send_sync', #state{subs_pid=Pid, queue=Queue, consuming='true', sync='false'} = State) ->
    maybe_sync_subscriptions(?SUBSCRIPTIONS_SYNC_ENABLED, Queue),
    erlang:send_after(2 * ?MILLISECONDS_IN_SECOND, Pid, 'check_sync'),
    {'noreply', State#state{sync='true'}};
handle_cast(_Msg, State) ->
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

-spec maybe_sync_subscriptions(boolean(), binary()) -> 'ok'.
maybe_sync_subscriptions('false', _) -> 'ok';
maybe_sync_subscriptions('true', Queue) ->
    Payload = kz_json:from_list(
                [{<<"Action">>, <<"Request">>}
                 | kz_api:default_headers(Queue, ?APP_NAME, ?APP_VERSION)
                ]),
    kapi_presence:publish_sync(Payload).
