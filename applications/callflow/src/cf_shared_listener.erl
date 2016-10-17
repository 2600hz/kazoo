%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% Listener for route requests that can be fulfilled by callflows
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_shared_listener).
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

-include("callflow.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'notifications'
                   ,[{'restrict_to', ['register']}]
                   },
                   {'presence'
                   ,[{'restrict_to', ['probe', 'mwi_query']}
                    ,{'probe_type', <<"dialog">>}
                    ]
                   }
                  ,{'self', []}
                  ,{'callflow', []}
                  ]).
-define(RESPONDERS, [{{'cf_util', 'presence_probe'}
                     ,[{<<"presence">>, <<"probe">>}]
                     }
                    ,{{'cf_util', 'presence_mwi_query'}
                     ,[{<<"presence">>, <<"mwi_query">>}]
                     }
                    ,{{'cf_util', 'notification_register'}
                     ,[{<<"notification">>, <<"register">>}]
                     }
                    ,{'cf_route_resume', [{<<"callflow">>, <<"resume">>}]}
                    ]).
-define(QUEUE_NAME, <<"callflow_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER, [{'responders', ?RESPONDERS}
                                     ,{'bindings', ?BINDINGS}
                                     ,{'queue_name', ?QUEUE_NAME}
                                     ,{'queue_options', ?QUEUE_OPTIONS}
                                     ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

%%%===================================================================
%%% gen_listener callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:debug("starting new callflow shared queue server"),
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
handle_call(_Msg, _From, State) ->
    {'noreply', State}.

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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), any()) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("callflow shared queue server ~p termination", [_Reason]).

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
