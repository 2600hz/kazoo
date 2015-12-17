%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
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
    wh_util:put_callid(?LOG_SYSTEM_ID),
    add_zones(get_config()),
    lager:info("waiting for first amqp connection...", []),
    wh_amqp_connections:wait_for_available(),
    timer:sleep(2 * ?MILLISECONDS_IN_SECOND),
    amqp_util:targeted_exchange(),
    {'ok', #state{}, 100}.

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
handle_info('timeout', State) ->
    _ = wh_amqp_sup:stop_bootstrap(),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec add_zones(wh_proplist()) -> 'ok'.
add_zones([]) -> 'ok';
add_zones([{ZoneName, Brokers}|Zones]) ->
    _ = add_brokers(Brokers, ZoneName),
    add_zones(Zones).

-spec add_brokers(ne_binaries(), atom()) -> 'ok'.
add_brokers([], _) -> 'ok';
add_brokers([Broker|Brokers], ZoneName) ->
    _ = wh_amqp_connections:add(Broker, ZoneName),
    add_brokers(Brokers, ZoneName).

-spec get_config() -> wh_proplist().
get_config() ->
    case wh_config:get(wh_config:get_node_section_name(), 'zone') of
        [Zone] -> get_from_zone(Zone);
        _Else -> get_from_amqp()
    end.

-spec get_from_amqp() -> wh_proplist().
get_from_amqp() ->
    [{'local', wh_config:get('amqp', 'uri', ?DEFAULT_AMQP_URI)}].

-spec get_from_zone(atom()) -> wh_proplist().
get_from_zone(ZoneName) ->
    Zones = wh_config:get('zone'),
    Props = dict:to_list(get_from_zone(ZoneName, Zones, dict:new())),
    case props:get_value('local', Props, []) of
        [] -> [{'local', wh_config:get('amqp', 'uri', ?DEFAULT_AMQP_URI)}|Props];
        _Else -> Props
    end.

-spec get_from_zone(atom(), wh_proplist(), dict()) -> dict().
get_from_zone(_, [], Dict) -> Dict;
get_from_zone(ZoneName, [{_, Zone}|Zones], Dict) ->
    case props:get_value('name', Zone) of
        'undefined' -> get_from_zone(ZoneName, Zones, Dict);
        ZoneName ->
            get_from_zone(ZoneName, Zones, import_zone('local', Zone, Dict));
        RemoteZoneName ->
            get_from_zone(ZoneName, Zones, import_zone(RemoteZoneName, Zone, Dict))
    end.

-spec import_zone(atom(), wh_proplist(), dict()) -> dict().
import_zone(_, [], Dict) -> Dict;
import_zone(ZoneName, [{'amqp_uri', URI}|Props], Dict) ->
    case dict:find(ZoneName, Dict) of
        'error' ->
            import_zone(ZoneName, Props, dict:store(ZoneName, [URI], Dict));
         _ ->
            import_zone(ZoneName, Props, dict:append(ZoneName, URI, Dict))
    end;
import_zone(ZoneName, [_|Props], Dict) -> import_zone(ZoneName, Props, Dict).
