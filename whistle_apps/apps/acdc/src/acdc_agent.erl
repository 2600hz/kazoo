%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent).

-behaviour(gen_listener).

%% API
-export([start_link/1, handle_call_event/2, update_agent/2
         ,maybe_handle_call/5
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("acdc.hrl").

-record(state, {
          acct_db :: ne_binary()
         ,agent_id :: ne_binary()
         ,queues :: [ne_binary(),...]
         ,call :: whapps_call:call()
         }).

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
start_link(_) ->
    gen_listener:start_link(?MODULE, [{bindings, []}
                                      ,{responders, [{{?MODULE, handle_call_event}, {<<"call_event">>, <<"*">>}}]}
                                     ], []).

handle_call_event(JObj, Props) ->
    gen_listener:cast(props:get_value(server, Props), {call_event, JObj}).

update_agent(Srv, IDandInfo) ->
    gen_listener:cast(Srv, {update, IDandInfo}).

maybe_handle_call(Srv, Call, AcctDb, QueueId, Timeout) ->
    gen_listener:call(Srv, {maybe_handle_call, Call, AcctDb, QueueId}, Timeout).

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
init(_) ->
    put(callid, ?LOG_SYSTEM_ID),
    lager:debug("acdc_agent starting"),
    {ok, #state{}}.

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
handle_call({maybe_handle_call, Call, AcctDb, QueueId}, _, #state{
                                                          acct_db=DB
                                                          ,queues=Qs
                                                          ,agent_id=AgentId
                                                         }=State) ->
    case AcctDb =:= DB andalso lists:member(QueueId, Qs) of
        false ->
            lager:debug("db ~s doesn't match ~s", [AcctDb, DB]),
            lager:debug("or agent isn't in queue ~s: ~p", [QueueId, Qs]),
            {reply, false, State};
        true ->
            case acdc_util:get_agent_status(AcctDb, AgentId) of
                <<"login">> ->
                    acdc_util:log_agent_activity(Call, <<"busy">>, AgentId),
                    {reply, true, State#state{call=Call}};
                <<"resume">> ->
                    acdc_util:log_agent_activity(Call, <<"busy">>, AgentId),
                    {reply, true, State#state{call=Call}};
                _Action ->
                    lager:debug("in non-ready state: ~s", [_Action]),
                    {reply, false, State}
            end
    end;
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
handle_cast({update, {AcctDb, AgentId, Info}}, State) ->
    lager:debug("updating agent ~s", [AgentId]),
    {noreply, State#state{
                acct_db=AcctDb
                ,agent_id=AgentId
                ,queues=wh_json:get_value(<<"queues">>, Info, [])
               }};
handle_cast(_Msg, State) ->
    lager:debug("cast: ~p", [_Msg]),
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
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
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
    ok.

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
