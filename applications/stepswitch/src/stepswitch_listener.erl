%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_listener).

-behaviour(gen_listener).

-include("stepswitch.hrl").

%% API
-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(BINDINGS, [{'route', []}
                   ,{'offnet_resource', []}
                   ,{'authn', []}
                  ]).
-define(RESPONDERS, [{'stepswitch_inbound'
                      ,[{<<"dialplan">>, <<"route_req">>}]}
                     ,{'stepswitch_outbound'
                       ,[{<<"resource">>, <<"offnet_req">>}]}
                     ,{'stepswitch_authn_req'
                       ,[{<<"directory">>, <<"authn_req">>}]}
                    ]).
-define(QUEUE_NAME, <<"stepswitch_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'bindings', ?BINDINGS}
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
init([]) ->
    _ = wh_couch_connections:add_change_handler(?RESOURCES_DB),
    lager:debug("starting new stepswitch outbound responder"),
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
handle_info({'document_changes', <<"_design/", _/binary>>, _}, State) ->
    {'noreply', State, 'hibernate'};
handle_info({'document_changes', _, _}, State) ->
    lager:info("flushing stepswitch cache due to changed offnet document"),
    _ =  wh_cache:flush_local(?STEPSWITCH_CACHE),
    {'noreply', State, 'hibernate'};
handle_info({'document_deleted', <<"_design/", _/binary>>}, State) ->
    {'noreply', State, 'hibernate'};
handle_info({'document_deleted', _}, State) ->
    lager:info("flushing stepswitch cache due to deleted offnet document"),
    _ =  wh_cache:flush_local(?STEPSWITCH_CACHE),
    {'noreply', State, 'hibernate'};
handle_info({'document_deleted', _DocId, 'undefined'}, State) ->
    {'noreply', State, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{}) ->
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
    lager:debug("stepswitch listener terminating: ~p", [_Reason]).

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
