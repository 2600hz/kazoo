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
-define(QUEUE_NAME, <<"resource_mgr">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).

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
    gen_listener:start_link(?MODULE,
                            [{responders, ?RESPONDERS}
                             ,{bindings, ?BINDINGS}
                             ,{queue_name, ?QUEUE_NAME}
                             ,{queue_options, ?QUEUE_OPTIONS}
                             ,{consume_options, ?CONSUME_OPTIONS}
                             ,{basic_qos, 1}
                            ], []).

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok' | 'fail'.
handle_req(JObj, _Prop) ->
    true = wapi_resource:req_v(JObj),
%%    put(callid, wh_util:put_callid(JObj)),
    put(callid, "0000000000"),

    Options = get_request_options(JObj),
    Nodes = get_resources(request_type(JObj), Options),

    Min = wh_util:to_integer(props:get_value(min_channels_requested, Options)),
    Max = wh_util:to_integer(props:get_value(max_channels_requested, Options)),

    Route = ecallmgr_fs_xml:build_route(wh_json:set_value(<<"Realm">>, ?DEFAULT_DOMAIN, JObj), wh_json:get_value(<<"Invite-Format">>, JObj)),

    case start_channels(Nodes, JObj, Route, Min, Max-Min, []) of
        {error, failed_starting, Errors} ->
            send_failed_req(JObj, Errors),
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
    lager:debug("starting new resource manager"),
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
    lager:debug("Unhandled message: ~p", [_Info]),
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
    lager:debug("resource manager terminating: ~p", [_Reason]).

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
    lager:debug("request to originate new ~s resource", [Type]),
    %% merge other switch results into this list as well (eventually)
    FSAvail = ecallmgr_fs_handler:request_resource(Type, Options),
    lists:usort(fun sort_resources/2, FSAvail);
get_resources(_Type, _Options) ->
    [].

-spec request_type/1 :: (wh_json:json_object()) -> {binary(), binary(), binary()}.
request_type(JObj) ->
    {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Resource-Type">>, JObj)}.

-spec get_request_options/1 :: (wh_json:json_object()) -> proplist().
get_request_options(JObj) ->
    Min = wh_util:to_integer(wh_json:get_value(<<"Resource-Minimum">>, JObj, 1)),
    [{min_channels_requested, Min}
     ,{max_channels_requested, wh_util:to_integer(wh_json:get_value(<<"Resource-Maximum">>, JObj, Min))}
    ].

-spec start_channels/6 :: ([proplist(),...] | [], wh_json:json_object(), binary() | list(), integer(), integer(), list()) -> {'error', 'failed_starting', list()} | 'ok'.
start_channels(_Ns, _JObj, _Route, 0, 0, _) -> ok; %% started all channels requested
start_channels([], _JObj, _Route, 0, _, _) -> ok; %% started at least the minimum channels, but ran out of servers with available resources
start_channels([], _JObj, _Route, _, _, Errors) -> {error, failed_starting, Errors}; %% failed to start the minimum channels before server resources ran out
start_channels([N | Ns]=Nodes, JObj, Route, 0, Max, Errors) -> %% these are bonus channels not required but desired
    case start_channel(N, Route, JObj) of
        {ok, 0} -> start_channels(Ns, JObj, Route, 0, Max-1, Errors);
        {ok, _} -> start_channels(Nodes, JObj, Route, 0, Max-1, Errors);
        {error, E} -> start_channels(Ns, JObj, Route, 0, Max, [E|Errors])
    end;
start_channels([N | Ns]=Nodes, JObj, Route, Min, Max, Errors) -> %% start the minimum channels
    case start_channel(N, Route, JObj) of
        {ok, 0} -> start_channels(Ns, JObj, Route, Min-1, Max, Errors);
        {ok, _} -> start_channels(Nodes, JObj, Route, Min-1, Max, Errors);
        {error, E} -> start_channels(Ns, JObj, Route, Min, Max, [E|Errors])
    end.

-spec start_channel/3 :: (proplist(), binary() | list(), wh_json:json_object()) -> {'ok', integer()} | {'error', 'timeout' | binary()}.
start_channel(N, Route, JObj) ->
    Pid = props:get_value(node, N),
    case ecallmgr_fs_node:resource_consume(Pid, Route, JObj) of
        {resource_consumed, UUID, CtlQ, AvailableChan} ->
            spawn(fun() -> send_uuid_to_app(JObj, UUID, CtlQ) end),
            {ok, AvailableChan};
        {resource_error, E} ->
            lager:debug("Error starting channel on ~p: ~p", [Pid, E]),
            spawn(fun() -> send_failed_consume(Route, JObj, E) end),
            {error, E}
    end.

-spec send_uuid_to_app/3 :: (wh_json:json_object(), binary(), binary()) -> 'ok'.
send_uuid_to_app(JObj, UUID, CtlQ) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, UUID}
            ,{<<"Control-Queue">>, CtlQ}
            | wh_api:default_headers(CtlQ, <<"resource">>, <<"originate_resp">>, ?APP_NAME, ?APP_VERSION)],
    lager:debug("sending resource response for ~s", [UUID]),
    wapi_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec send_failed_req/2 :: (wh_json:json_object(), list()) -> 'ok'.
send_failed_req(JObj, Errors) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Failed-Attempts">>, length(Errors)}
            ,{<<"Failure-Message">>, [wh_util:to_binary(E) || E <- Errors]}
            | wh_api:default_headers(<<"resource">>, <<"originate_error">>, ?APP_NAME, ?APP_VERSION)],
    lager:debug("sending resource error"),
    wapi_resource:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec send_failed_consume/3 :: (binary() | list(), wh_json:json_object(), binary()) -> 'ok'.
send_failed_consume(Route, JObj, E) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Failed-Route">>, Route}
            ,{<<"Failure-Message">>, wh_util:to_binary(E)}
            | wh_api:default_headers(<<"resource">>, <<"originate_error">>, ?APP_NAME, ?APP_VERSION)],
    lager:debug("sending originate error ~s", [E]),
    wapi_resource:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

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
