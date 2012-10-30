%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
-export([start_link/0
         ,check_agent_status/1
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

-define(BINDINGS, [{conf, [{doc_type, <<"user">>}]}
                   ,{acdc_agent, [{restrict_to, [status, stats_req]}]}
                   ,{notifications, [{restrict_to, [presence_probe]}]}
                  ]).
-define(RESPONDERS, [{{acdc_agent_handler, handle_status_update} 
                      ,[{<<"agent">>, <<"login">>}
                        ,{<<"agent">>, <<"logout">>}
                        ,{<<"agent">>, <<"pause">>}
                        ,{<<"agent">>, <<"resume">>}
                        ,{<<"agent">>, <<"login_queue">>}
                        ,{<<"agent">>, <<"logout_queue">>}
                       ]
                     }
                     ,{{acdc_agent_handler, handle_stats_req}
                       ,[{<<"agent">>, <<"stats_req">>}]
                      }
                     ,{{acdc_agent_handler, handle_config_change}
                       ,[{<<"configuration">>, <<"*">>}]
                       }
                     ,{{acdc_agent_handler, handle_presence_probe}
                       ,[{<<"notification">>, <<"presence_probe">>}]
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
    gen_listener:start_link({local, ?SERVER}, ?MODULE
                            ,[{bindings, ?BINDINGS}
                              ,{responders, ?RESPONDERS}
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
init([]) ->
    Ref = start_agent_timer(),
    {ok, Ref}.

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
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(agent_check, State) ->
    Self = self(),
    _ = spawn(?MODULE, check_agent_status, [Self]),
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_agent_timer() ->
    start_agent_timer(self()).
start_agent_timer(Pid) ->
    erlang:send_after(whapps_config:get(?APP_NAME, <<"agent_timeout">>, 600000)
                      ,Pid
                      ,agent_check
                     ).

check_agent_status(Self) ->
    _ = [check_account(DB) || DB <- whapps_util:get_all_accounts(encoded)],
    start_agent_timer(Self).

check_account(AcctDb) ->
    case couch_mgr:get_results(AcctDb, <<"agents/agent_listing">>, []) of
        {ok, []} -> ok;
        {error, _} -> ok;
        {ok, Agents} -> [check_agent(AcctDb, wh_json:get_value(<<"id">>, Agent)) || Agent <- Agents]
    end.

check_agent(AcctDb, AgentId) ->
    AcctId = wh_util:format_account_id(AcctDb, raw),

    Req = [{<<"Account-ID">>, AcctId}
           ,{<<"Agent-ID">>, AgentId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_request(Req
                                       ,fun wapi_acdc_agent:publish_sync_req/1
                                       ,fun wapi_acdc_agent:sync_resp_v/1
                                       ,5000
                                      ) of
        {ok, _} -> ok;
        {error, _E} ->
            lager:debug("failed to get sync resp: ~p", [_E]),
            maybe_logout_agent(AcctDb, AgentId)
    end.

maybe_logout_agent(AcctDb, AgentId) ->
    case acdc_util:agent_status(AcctDb, AgentId) of
        <<"logout">> -> ok;
        _S ->
            lager:debug("logging agent out from status ~s", [_S]),
            logout_agent(AcctDb, AgentId)
    end.

logout_agent(AcctDb, AgentId) ->
    lager:debug("logging ~s out for not responding to sync req", [AgentId]),

    Doc = wh_json:from_list([{<<"agent_id">>, AgentId}
                             ,{<<"method">>, <<"acdc_agent_manager">>}
                             ,{<<"action">>, <<"logout">>}
                             ,{<<"pvt_type">>, <<"agent_activity">>}
                            ]),
    couch_mgr:save_doc(AcctDb, wh_doc:update_pvt_parameters(Doc, AcctDb)).
