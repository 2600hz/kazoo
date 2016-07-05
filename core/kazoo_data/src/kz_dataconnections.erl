%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_dataconnections).

-behaviour(gen_server).

-export([start_link/0]).
-export([add/1]).
-export([update/1]).
-export([wait_for_connection/0, wait_for_connection/1, wait_for_connection/2]).
-export([get_server/0, get_server/1
	,test_conn/0, test_conn/1
        ]).

-export([init/1
	,handle_call/3
	,handle_cast/2
	,handle_info/2
	,terminate/2
	,code_change/3
        ]).

-include("kz_data.hrl").

-define(SERVER, ?MODULE).

-export_type([data_connection/0, data_connections/0]).

-record(state, {cookie = 'change_me'}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec update(data_connection()) -> 'ok'.
update(#data_connection{}=Connection) ->
    gen_server:cast(?SERVER, {'update_connection', Connection}).

-spec add(data_connection()) -> 'ok'.
add(#data_connection{tag='undefined'}=Connection) ->
    add(Connection#data_connection{tag='local'});
add(#data_connection{}=Connection) ->
    gen_server:cast(?SERVER, {'add_connection', Connection}).

-spec wait_for_connection() -> 'ok' | 'no_connection'.
-spec wait_for_connection(any()) -> 'ok' | 'no_connection'.
-spec wait_for_connection(any(), kz_timeout()) -> 'ok' | 'no_connection'.

wait_for_connection() ->
    wait_for_connection('local').

wait_for_connection(Tag) ->
    wait_for_connection(Tag, 'infinity').

wait_for_connection(_Tag, 0) -> 'no_connection';
wait_for_connection(Tag, Timeout) ->
    Start = os:timestamp(),
    try test_conn(Tag) of
        {'error', _E} ->
            timer:sleep(random:uniform(?MILLISECONDS_IN_SECOND) + 100),
            wait_for_connection(Tag, kz_util:decr_timeout(Timeout, Start));
        {'ok', Info} -> lager:info("connected to ~s : ~p", [Tag, Info])
    catch
        'error':{'badmatch','$end_of_table'} ->
            timer:sleep(random:uniform(?MILLISECONDS_IN_SECOND) + 100),
            wait_for_connection(Tag, kz_util:decr_timeout(Timeout, Start))
    end.


-spec get_server() -> server().
get_server() ->
    get_server('local').

-spec get_server(term()) -> server().
get_server(Tag) ->
    MatchSpec = [{#data_connection{ready = 'true'
				  ,app = '$1'
				  ,server = '$2'
				  ,tag = Tag
				  ,_ = '_'
                                  }
		 ,[]
		 ,[{{'$1', '$2'}}]
                 }],
    case ets:select(?MODULE, MatchSpec, 1) of
        {[{App, Server}], _} -> {App, Server};
        _ -> 'undefined'
    end.

-spec test_conn() -> {'ok', kz_json:object()} |
                     {'error', any()}.
-spec test_conn(term()) -> {'ok', kz_json:object()} |
                           {'error', any()}.

test_conn() -> test_conn('local').
test_conn(Tag) ->
    case get_server(Tag) of
        'undefined' -> {'error', 'server_not_available'};
        Server -> kzs_server:server_info(Server)
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
init([]) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(?LOG_SYSTEM_ID),
    _ = ets:new(?MODULE, ['ordered_set'
			 ,{'read_concurrency', 'true'}
			 ,{'keypos', #data_connection.id}
			 ,'named_table'
                         ]),
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
handle_cast({'add_connection', #data_connection{}=Connection}, State) ->
    lager:info("adding connection"),
    maybe_start_new_connection(Connection),
    {'noreply', State};
handle_cast({'update_connection', #data_connection{}=Connection}, State) ->
    'true' = ets:insert(?MODULE, Connection),
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:info("unhandle connection"),
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
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("couch connections terminating: ~p", [_Reason]).

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
-spec maybe_start_new_connection(data_connection()) -> any().
maybe_start_new_connection(Connection) ->
    _ = kz_dataconnection_sup:add(Connection),
    _ = ets:insert(?MODULE, Connection).
