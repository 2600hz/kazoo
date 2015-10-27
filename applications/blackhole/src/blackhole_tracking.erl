%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter DEfebvre
%%%-------------------------------------------------------------------
-module(blackhole_tracking).

-behaviour(gen_server).

-export([start_link/0
         ,add_socket/1
         ,remove_socket/1
         ,update_socket/1
         ,get_sockets/1
         ,get_socket/1
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("blackhole.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec add_socket(bh_context:context()) -> 'ok'.
add_socket(Context) ->
    gen_server:cast(?MODULE, {'add_socket', Context}).

-spec remove_socket(bh_context:context()) -> 'ok'.
remove_socket(Context) ->
    gen_server:cast(?MODULE, {'remove_socket', Context}).

-spec update_socket(bh_context:context()) -> 'ok'.
update_socket(Context) ->
    gen_server:cast(?MODULE, {'update_socket', Context}).

-spec get_sockets(ne_binary()) -> ne_binaries().
get_sockets(AccountId) ->
    gen_server:call(?MODULE, {'get_sockets', AccountId}).

-spec get_socket(ne_binary()) -> {'ok', bh_context:context()} | {'error', 'not_found'}.
get_socket(Id) ->
    gen_server:call(?MODULE, {'get_socket', Id}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

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
    wh_util:put_callid(?MODULE),
    lager:debug("started ~s", [?MODULE]),
    Tab = ets:new(?MODULE, ['set'
                            ,'protected'
                            ,'named_table'
                            ,{'keypos', #bh_context.websocket_session_id}
                           ]),
    {'ok', Tab}.

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
handle_call({'get_sockets', AccountId}, _From, State) ->
    Pattern = #bh_context{account_id=AccountId, _='_'},
    Result =
        case ets:match_object(State, Pattern) of
            [] -> {'error', 'not_found'};
            Contexts ->
                [bh_context:websocket_session_id(Context) || Context <- Contexts]
        end,
    {'reply', Result, State};
handle_call({'get_socket', Id}, _From, State) ->
    Pattern = #bh_context{websocket_session_id=Id, _='_'},
    Result =
        case ets:match_object(State, Pattern) of
            [] -> {'error', 'not_found'};
            [Context] ->
                {'ok', Context}
        end,
    {'reply', Result, State};
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
handle_cast({'add_socket', Context}, State) ->
    _ = ets:insert(State, Context),
    {noreply, State};
handle_cast({'remove_socket', Context}, State) ->
    _ = ets:delete_object(State, Context),
    {noreply, State};
handle_cast({'update_socket', Context}, State) ->
    _ = ets:insert(State, Context),
    {noreply, State};
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
