%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Karls Hackity Hack....
%%% We want to block during startup until we have a AMQP connection
%%% but due to the way wh_amqp_mgr is structured we cant block in
%%% init there.  So this module will bootstrap wh_amqp_mgr
%%% and block until a connection becomes available, after that it
%%% removes itself....
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_amqp_bootstrap).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(SERVER, ?MODULE).
-define(STARTUP_FILE, [code:lib_dir(whistle_amqp, priv), "/startup.config"]).

-record(state, {}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    put(callid, ?LOG_SYSTEM_ID),
    Init = get_config(),
    UseFederation = props:get_value(use_federation, Init, false),
    URIs = case props:get_value(amqp_uri, Init, ?DEFAULT_AMQP_URI) of
               URI = "amqp://"++_ -> [URI];
               URI = "amqps://"++_ -> [URI];
               URI when is_list(URI) -> URI
           end,
    _ = [gen_server:cast(wh_amqp_mgr, {add_broker, Uri, UseFederation}) || Uri <- URIs],
    lager:info("waiting for first AMQP connection...", []),
    wh_amqp_mgr:wait_for_available_host(),
    lager:debug("host is available"),
    lager:debug("connection to use: ~p", [wh_amqp_mgr:get_connection()]),
    {ok, #state{}, 100}.

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
    {reply, {error, not_implemented}, State}.

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
handle_info(timeout, State) ->
    _ = wh_amqp_sup:stop_bootstrap(),
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

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
    lager:debug("amqp bootstrap terminating: ~p", [_Reason]).

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
-spec get_config/0 :: () -> wh_proplist().
get_config() ->
    case file:consult(?STARTUP_FILE) of
        {ok, Prop} -> Prop;
        E ->
            lager:debug("unable to load amqp manager configuration from '~s': ~p", [?STARTUP_FILE, E]),
            []
    end.
