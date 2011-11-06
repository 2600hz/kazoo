%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Server to maintain a list of resources available and who can serve them;
%%% query for a resource type and get a server to handle the request.
%%% @end
%%% Created : 11 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(resource_mgr).

-behaviour(gen_listener).

%% API
-export([start_link/0, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{?MODULE, [{<<"resource">>, <<"originate_req">>}]}]).
-define(BINDINGS, [{resource, []}]).

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
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				     ,{bindings, ?BINDINGS}
				     ], []).

-spec handle_req/2 :: (json_object(), proplist()) -> 'ok' | 'fail'.
handle_req(JObj, _Prop) ->
    true = wapi_resource:req_v(JObj),

    Options = get_request_options(JObj),
    Nodes = get_resources(request_type(JObj), Options),

    Min = wh_util:to_integer(props:get_value(min_channels_requested, Options)),
    Max = wh_util:to_integer(props:get_value(max_channels_requested, Options)),

    Route = ecallmgr_fs_xml:build_route(wh_json:set_value(<<"Realm">>, ?DEFAULT_DOMAIN, JObj), wh_json:get_value(<<"Invite-Format">>, JObj)),

    case start_channels(Nodes, JObj, Route, Min, Max-Min) of
	{error, failed_starting, Failed} ->
	    send_failed_req(JObj, Failed),
	    fail;
	ok -> ok
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
    ?LOG_SYS("starting new resource manager"),
    {ok, ok}.

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
    {reply, ok, State}.

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
handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_,_) ->
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
    ?LOG_SYS("resource manager ~p termination", [_Reason]).

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
-spec get_resources/2 :: ({binary(), binary(), binary()}, proplist()) -> [proplist(),...] | [].
get_resources({<<"resource">>, <<"originate_req">>, <<"audio">>=Type}, Options) ->
    ?LOG("request to originate new ~s resource", [Type]),
    %% merge other switch results into this list as well (eventually)
    FSAvail = ecallmgr_fs_handler:request_resource(Type, Options),
    lists:usort(fun sort_resources/2, FSAvail);
get_resources(_Type, _Options) ->
    [].

-spec request_type/1 :: (json_object()) -> {binary(), binary(), binary()}.
request_type(JObj) ->
    {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Resource-Type">>, JObj)}.

-spec get_request_options/1 :: (json_object()) -> proplist().
get_request_options(JObj) ->
    Min = wh_util:to_integer(wh_json:get_value(<<"Resource-Minimum">>, JObj, 1)),
    [{min_channels_requested, Min}
     ,{max_channels_requested, wh_util:to_integer(wh_json:get_value(<<"Resource-Maximum">>, JObj, Min))}
    ].

-spec start_channels/5 :: ([proplist(),...] | [], json_object(), binary() | list(), integer(), integer()) -> {'error', 'failed_starting', integer()} | 'ok'.
start_channels(_Ns, _JObj, _Route, 0, 0) -> ok; %% started all channels requested
start_channels([], _JObj, _Route, 0, _) -> ok; %% started at least the minimum channels, but ran out of servers with available resources
start_channels([], _JObj, _Route, M, _) -> {error, failed_starting, M}; %% failed to start the minimum channels before server resources ran out
start_channels([N | Ns]=Nodes, JObj, Route, 0, Max) -> %% these are bonus channels not required but desired
    case start_channel(N, Route, JObj) of
	{ok, 0} -> start_channels(Ns, JObj, Route, 0, Max-1);
	{ok, _} -> start_channels(Nodes, JObj, Route, 0, Max-1);
	{error, _} -> start_channels(Ns, JObj, Route, 0, Max)
    end;
start_channels([N | Ns]=Nodes, JObj, Route, Min, Max) -> %% start the minimum channels
    case start_channel(N, Route, JObj) of
	{ok, 0} -> start_channels(Ns, JObj, Route, Min-1, Max);
	{ok, _} -> start_channels(Nodes, JObj, Route, Min-1, Max);
	{error, _} -> start_channels(Ns, JObj, Route, Min, Max)
    end.

-spec start_channel/3 :: (proplist(), binary() | list(), json_object()) -> {'ok', integer()} | {'error', 'timeout' | binary()}.
start_channel(N, Route, JObj) ->
    Pid = props:get_value(node, N),
    case ecallmgr_fs_node:resource_consume(Pid, Route, JObj) of
	{resource_consumed, UUID, CtlQ, AvailableChan} ->
	    spawn(fun() -> send_uuid_to_app(JObj, UUID, CtlQ) end),
	    {ok, AvailableChan};
	{resource_error, E} ->
	    ?LOG("Error starting channel on ~p: ~p", [Pid, E]),
	    spawn(fun() -> send_failed_consume(Route, JObj, E) end),
	    {error, E}
    end.

-spec send_uuid_to_app/3 :: (json_object(), binary(), binary()) -> 'ok'.
send_uuid_to_app(JObj, UUID, CtlQ) ->
    Msg = wh_json:get_value(<<"Msg-ID">>, JObj),
    AppQ = wh_json:get_value(<<"Server-ID">>, JObj),

    RespProp = [{<<"Msg-ID">>, Msg}
	       ,{<<"Call-ID">>, UUID}
	       ,{<<"Control-Queue">>, CtlQ}
		| wh_api:default_headers(CtlQ, <<"resource">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)],
    {ok, JSON} = wapi_resource:resp(RespProp),
    ?LOG("sending ~s", [JSON]),
    wapi_resource:publish_resp(AppQ, JSON).

-spec send_failed_req/2 :: (json_object(), integer()) -> 'ok'.
send_failed_req(JObj, Failed) ->
    Msg = wh_json:get_value(<<"Msg-ID">>, JObj),
    AppQ = wh_json:get_value(<<"Server-ID">>, JObj),

    RespProp = [{<<"Msg-ID">>, Msg}
		,{<<"Failed-Attempts">>, Failed}
		| wh_api:default_headers(<<>>, <<"resource">>, <<"resource_error">>, ?APP_NAME, ?APP_VERSION)],
    {ok, JSON} = wapi_resource:error(RespProp),
    ?LOG("sending resource error ~s", [JSON]),
    wapi_resource:publish_error(AppQ, JSON).

-spec send_failed_consume/3 :: (binary() | list(), json_object(), binary()) -> 'ok'.
send_failed_consume(Route, JObj, E) ->
    Msg = wh_json:get_value(<<"Msg-ID">>, JObj),
    AppQ = wh_json:get_value(<<"Server-ID">>, JObj),

    RespProp = [{<<"Msg-ID">>, Msg}
		,{<<"Failed-Route">>, Route}
		,{<<"Failure-Message">>, wh_util:to_binary(E)}
		| wh_api:default_headers(<<>>, <<"resource">>, <<"originate_error">>, ?APP_NAME, ?APP_VERSION)],
    {ok, JSON} = wapi_resource:error(RespProp),
    ?LOG("sending originate error ~s", [JSON]),
    wapi_resource:publish_error(AppQ, JSON).

%% sort first by percentage utilized (less utilized first), then by bias (larger goes first), then by available channels (more available first)
%% [
%%   [{node, 1}, {p_u, 80}, {a_c, 3}, ...]
%%  ,[{node, 2}, {p_u, 50}, {a_c, 5}, ...]
%%  ,[{node, 3}, {p_u, 50}, {a_c, 1}, ...]
%%  ,[{node, 4}, {p_u, 10}, {a_c, 10}, ...]
%% ] ==>
%% [ [{node, 4}], [{node, 2}], [{node, 3}], [{node, 1}] ]
%%    least util,  more chan    most chan    most util
-spec sort_resources/2 :: (proplist(), proplist()) -> boolean().
sort_resources(PropA, PropB) ->
    UtilB = props:get_value(percent_utilized, PropB),
    case props:get_value(percent_utilized, PropA) of
        UtilB ->                   % same utilization, use node with more available channels
	    BiasB = props:get_value(bias, PropB, 1),
	    case props:get_value(bias, PropA, 1) of
		BiasB -> %% same bias, use node with more available channels
		    ACB = props:get_value(available_channels, PropB),
		    case props:get_value(available_channels, PropA) of
			C when C >= ACB -> true;
			_ -> false
		    end;
		B when B > BiasB -> true; % A has bigger bias
		_B -> true
	    end;
	A when A > UtilB -> false; % B is less utilized
	_A -> true                 % A is less utilized
    end.
