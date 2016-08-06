%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(tasks_listener).
-behaviour(gen_listener).

-export([start_link/0]).

-export([handle_lookup_req/2
        ,handle_start_req/2
        ,handle_remove_req/2
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("tasks.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

-define(BINDINGS, [{'self', []}
                  ,{'tasks', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_lookup_req'}
                     ,[{<<"tasks">>, <<"lookup_req">>}]
                     }
                    ,{{?MODULE, 'handle_start_req'}
                     ,[{<<"tasks">>, <<"start_req">>}]
                     }
                    ,{{?MODULE, 'handle_remove_req'}
                     ,[{<<"tasks">>, <<"remove_req">>}]
                     }
                    ]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[]
                           ).


%%%===================================================================
%%% AMQP API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_lookup_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_lookup_req(JObj, _Props) ->
    'true' = kapi_tasks:lookup_req_v(JObj),
    Help =
        case help(JObj) of
            {'error', _R} ->
                lager:debug("lookup_req error: ~s", [_R]),
                kz_json:new();
            {'ok', JOk} -> JOk;
            JOk -> JOk
        end,
    Resp = kz_json:from_list(
             [{<<"Help">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_lookup_resp(kz_api:server_id(JObj), Resp).

%% @private
-spec help(kz_json:object()) -> kz_tasks:help_error().
help(JObj) ->
    case {kapi_tasks:category(JObj), kapi_tasks:action(JObj)} of
        {'undefined', 'undefined'} -> kz_tasks_scheduler:help();
        {Category, 'undefined'} -> kz_tasks_scheduler:help(Category);
        {Category, Action} -> kz_tasks_scheduler:help(Category, Action)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_start_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_start_req(JObj, _Props) ->
    'true' = kapi_tasks:start_req_v(JObj),
    Help =
        case kz_tasks_scheduler:start(kapi_tasks:task_id(JObj)) of
            {'ok', TaskJObj} -> TaskJObj;
            {'error', 'already_started'} -> <<"already_started">>
        end,
    Resp = kz_json:from_list(
             [{<<"Reply">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_start_resp(kz_api:server_id(JObj), Resp).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_remove_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_remove_req(JObj, _Props) ->
    'true' = kapi_tasks:remove_req_v(JObj),
    Help =
        case kz_tasks_scheduler:remove(kapi_tasks:task_id(JObj)) of
            {'ok', TaskJObj} -> TaskJObj;
            {'error', 'task_running'} -> <<"task_running">>
        end,
    Resp = kz_json:from_list(
             [{<<"Reply">>, Help}
             ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]
            ),
    kapi_tasks:publish_remove_resp(kz_api:server_id(JObj), Resp).


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
handle_info(_Info, State) ->
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

%% End of Module.
