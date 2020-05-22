%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Manages agent processes:
%%%   starting when an agent logs in
%%%   stopping when an agent logs out
%%%   collecting stats from agents
%%%   and more!!!
%%%
%%% @author James Aimonetti
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_agent_manager).
-behaviour(gen_listener).

%% API
-export([start_link/0
        ,start_agent/3
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
-include_lib("kazoo_events/include/kz_hooks.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'acdc_agent', [{'restrict_to', ['status']}]}
                  ,{'presence', [{'restrict_to', ['probe']}]}
                  ,{'conf', [{'type', <<"user">>}
                            ,'federate'
                            ]}
                  ,{'conf', [{'type', <<"device">>}
                            ,'federate'
                            ]}
                  ]).
-define(RESPONDERS, [{{'acdc_agent_handler', 'handle_status_update'}
                     ,[{<<"agent">>, <<"login">>}
                      ,{<<"agent">>, <<"logout">>}
                      ,{<<"agent">>, <<"pause">>}
                      ,{<<"agent">>, <<"resume">>}
                      ,{<<"agent">>, <<"end_wrapup">>}
                      ,{<<"agent">>, <<"login_queue">>}
                      ,{<<"agent">>, <<"logout_queue">>}
                      ,{<<"agent">>, <<"restart">>}
                      ]
                     }
                    ,{{'acdc_agent_handler', 'handle_stats_req'}
                     ,[{<<"agent">>, <<"stats_req">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_presence_probe'}
                     ,[{<<"presence">>, <<"probe">>}]
                     }
                    ,{{'acdc_agent_handler', 'handle_config_change'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_term:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}, ?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[]
                           ).
%%------------------------------------------------------------------------------
%% @doc Start a new agent supervisor
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_agent(kz_term:ne_binary(), kz_term:ne_binary(), list()) -> kz_term:sup_startchild_ret().
start_agent(AccountId, AgentId, Args) ->
    gen_listener:call(?SERVER, {'start_agent', AccountId, AgentId, Args}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_hooks:register(),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_term:handle_call_ret_state(state()).
handle_call({'start_agent', AccountId, AgentId, Args}, _, State) ->
    case acdc_agents_sup:find_agent_supervisor(AccountId, AgentId) of
        'undefined' ->
            {'reply', supervisor:start_child('acdc_agents_sup', Args), State};
        Sup ->
            lager:error("agent ~s(~s) already started here: ~p", [AgentId, AccountId, Sup]),
            {'reply', {'already_started', Sup}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = 'ok',
    {'reply', Reply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_term:handle_cast_ret_state(state()).
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue',_QueueName}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_term:handle_info_ret_state(state()).
handle_info(?HOOK_EVT(AccountId, <<"CHANNEL_CREATE">>, JObj), State) ->
    lager:debug("channel_create event"),
    _ = kz_process:spawn(fun acdc_agent_handler:handle_new_channel/2, [JObj, AccountId]),
    {'noreply', State};
handle_info(?HOOK_EVT(AccountId, <<"CHANNEL_DESTROY">>, JObj), State) ->
    _ = kz_process:spawn(fun acdc_agent_handler:handle_destroyed_channel/2, [JObj, AccountId]),
    {'noreply', State};
handle_info(?HOOK_EVT(_AccountId, _EventName, _JObj), State) ->
    lager:debug("ignoring ~s for account ~s on call ~s", [_EventName, _AccountId, kz_json:get_value(<<"Call-ID">>, _JObj)]),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("agent manager terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
