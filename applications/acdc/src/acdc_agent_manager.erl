%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%% Manages agent processes:
%%%   starting when an agent logs in
%%%   stopping when an agent logs out
%%%   collecting stats from agents
%%%   and more!!!
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(acdc_agent_manager).

-behaviour(gen_listener).

%% API
-export([start_link/0]).

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

-define(BINDINGS, [{'acdc_agent', [{'restrict_to', ['status', 'stats_req']}]}
                   ,{'notifications', [{'restrict_to', ['presence_probe']}]}
                   ,{'call', [{'restrict_to', ['new_channel']}]}
                   ,{'conf', [{'type', <<"user">>}]}
                   ,{'conf', [{'type', <<"device">>}]}
                  ]).
-define(RESPONDERS, [{{'acdc_agent_handler', 'handle_status_update'} 
                      ,[{<<"agent">>, <<"login">>}
                        ,{<<"agent">>, <<"logout">>}
                        ,{<<"agent">>, <<"pause">>}
                        ,{<<"agent">>, <<"resume">>}
                        ,{<<"agent">>, <<"login_queue">>}
                        ,{<<"agent">>, <<"logout_queue">>}
                       ]
                     }
                     ,{{'acdc_agent_handler', 'handle_new_channel'}
                       ,[{<<"channel">>, <<"new">>}]
                      }
                     ,{{'acdc_agent_handler', 'handle_stats_req'}
                       ,[{<<"agent">>, <<"stats_req">>}]
                      }
                     ,{{'acdc_agent_handler', 'handle_presence_probe'}
                       ,[{<<"notification">>, <<"presence_probe">>}]
                      }
                     ,{{'acdc_agent_handler', 'handle_config_change'}
                       ,[{<<"configuration">>, <<"*">>}]
                      }
                    ]).

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
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                             ]
                            ,[]
                           ).

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
init([]) -> {'ok', 'ok'}.

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
    Reply = 'ok',
    {'reply', Reply, State}.

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
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("agent manager terminating: ~p", [_Reason]).

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
