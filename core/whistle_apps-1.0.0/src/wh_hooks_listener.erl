%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Listens for a list of events and gproc-sends them out to folks who
%%% want them
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_hooks_listener).

-behaviour(gen_listener).

-export([start_link/0
         ,handle_call_event/2
         ,register/0, register/1, register/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("whistle_apps.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(CACHE_NAME, 'wh_hooks_cache').

%% Three main call events
-define(BINDINGS, [{'call', [{'restrict_to', ['new_channel'
                                              ,'answered_channel'
                                              ,'destroy_channel'
                                             ]}
                            ]}
                   ,{'route', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                       ,[{<<"channel">>, <<"*">>}
                         ,{<<"dialplan">>, <<"route_req">>}
                        ]
                      }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

-define(HOOK_REG
        ,{'p', 'l', 'wh_hook'}).
-define(HOOK_REG(AccountId)
        ,{'p', 'l', {'wh_hook', AccountId}}).
-define(HOOK_REG(AccountId, EventName)
        ,{'p', 'l', {'wh_hook', AccountId, EventName}}).

-spec register() -> 'true'.
-spec register(ne_binary()) -> 'true'.
-spec register(ne_binary(), ne_binary()) -> 'true'.
register() ->
    'true' = gproc:reg(?HOOK_REG).
register(AccountId) ->
    'true' = gproc:reg(?HOOK_REG(AccountId)).
register(AccountId, EventName) ->
    'true' = gproc:reg(?HOOK_REG(AccountId, EventName)).

-spec handle_call_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, _Props) ->
    HookEvent = wh_json:get_value(<<"Event-Name">>, JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                                  ,JObj
                                 ),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),

    wh_util:put_callid(CallId),

    lager:debug("handle ~s(~s)", [HookEvent, AccountId]),

    handle_call_event(JObj, AccountId, HookEvent, CallId).

-spec handle_call_event(wh_json:object(), api_binary(), ne_binary(), ne_binary()) ->
                               'ok'.
handle_call_event(JObj, 'undefined', <<"new">>, CallId) ->
    lager:debug("event 'new' had no account id, caching"),
    maybe_cache_call_event(JObj, CallId);
handle_call_event(_JObj, 'undefined', _HookEvent, _CallId) ->
    lager:debug("event '~s' had no account id, ignoring", [_HookEvent]);
handle_call_event(_JObj, AccountId, <<"route_req">>, CallId) ->
    lager:debug("recv route_req with account id ~s, looking for events to relay"
                ,[AccountId]
               ),
    maybe_relay_new_event(AccountId, CallId);
handle_call_event(JObj, AccountId, HookEvent, _CallId) ->
    Evt = ?HOOK_EVT(AccountId, HookEvent, JObj),
    gproc:send(?HOOK_REG, Evt),
    gproc:send(?HOOK_REG(AccountId), Evt),
    gproc:send(?HOOK_REG(AccountId, HookEvent), Evt).

-spec maybe_cache_call_event(wh_json:object(), ne_binary()) -> 'ok'.
maybe_cache_call_event(JObj, CallId) ->
    case wh_cache:peek_local(?CACHE_NAME, CallId) of
        {'error', 'not_found'} ->
            wh_cache:store_local(?CACHE_NAME, CallId, {'new', JObj}, [{'expires', 5000}]);
        {'ok', {'account_id', AccountId}} ->
            handle_call_event(JObj
                              ,AccountId
                              ,wh_json:get_value(<<"Event-Name">>, JObj)
                              ,CallId
                             )
    end.

-spec maybe_relay_new_event(ne_binary(), ne_binary()) -> 'ok'.
maybe_relay_new_event(AccountId, CallId) ->
    case wh_cache:peek_local(?CACHE_NAME, CallId) of
        {'error', 'not_found'} ->
            wh_cache:store_local(?CACHE_NAME, CallId, {'account_id', AccountId}, [{'expires', 5000}]);
        {'ok', {'new', JObj}} ->
            handle_call_event(JObj
                              ,AccountId
                              ,wh_json:get_value(<<"Event-Name">>, JObj)
                              ,CallId
                             )
    end.

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
-spec init([]) -> {'ok', 'ok'}.
init([]) ->
    lager:debug("started ~s", [?MODULE]),
    _ = spawn('whistle_apps_sup', 'start_child', [?CACHE(?CACHE_NAME)]),
    {'ok', 'ok'}.

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
handle_cast({'wh_amqp_channel', {'new_channel', _IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _Q}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
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
handle_event(_JObj, _State) ->
    {'reply', []}.

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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
