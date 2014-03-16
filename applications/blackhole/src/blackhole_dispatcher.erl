-module(blackhole_dispatcher).

-behavior(gen_server).

-define(MOD_CONFIG_CAT, <<(?BLACKHOLE_CONFIG_CAT)/binary>>).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,status/1
        ]).

-record(state, {event_list :: wh_proplist()
                ,loaded_modules :: wh_proplist()
       }).

-type state() :: #state{}.

-define(SERVER, ?MODULE).

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
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

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
    lager:debug("blackhole_dispatcher init"),
    gen_server:cast(self(), 'start_dispatcher'),
        {'ok', #state{}}.

-spec status(pid()) -> handle_call_ret() | wh_std_return().
status(ServerPid) ->
    case is_pid(ServerPid) of
        'true' ->    
            gen_server:call(ServerPid, 'status');
                'false' -> {'error', 'process_not_found'}
    end.

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
-spec handle_call(atom(), any(), state()) -> handle_call_ret().
handle_call('status', _, #state{bound_events=BoundEvents}=State) -> 
    Status = {'num_accounts_left', length(BoundEvents)},
    {'reply', Status, State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled handle_call executed ~p~p", [_Request, _From]),
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
handle_cast('start_dispatcher', State) ->
    Modules = whapps_config:get_list(?MOD_CONFIG_CAT, <<"autoload_modules">>, []),
    lager:debug("unhandled handle_cast ~p", [_Msg]),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled handle_cast ~p", [_Msg]),
    {'noreply', State}.


terminate(_Reason, #state{pid=PID}=_State) ->
    lager:debug("blackhole_event_dispatcher terminated: ~p", [_Reason]),
    case is_pid(PID) of
        'false' ->
            lager:debug("no pid reference");
                'true' -> exit(PID, 'ok')
    end,
    'ok'.

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.
