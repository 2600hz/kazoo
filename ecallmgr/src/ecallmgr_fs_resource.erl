%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%    Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_resource).

-behaviour(gen_listener).

-export([start_link/1, start_link/2]).
-export([handle_originate_req/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-record(state, {node :: atom()
               ,options :: proplist()}).

-define(BINDINGS, [{resource, []}
                   ,{self, []}
                  ]).
-define(RESPONDERS, [{{?MODULE, handle_originate_req}, [{<<"resource">>, <<"originate_req">>}]}]).
-define(QUEUE_NAME, <<"ecallmgr_fs_resource">>).
-define(QUEUE_OPTIONS, [{exclusive, false}]).
-define(CONSUME_OPTIONS, [{exclusive, false}]).
-define(ROUTE_OPTIONS, []).

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
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_listener:start_link(?MODULE, [{bindings, ?BINDINGS}
                                      ,{responders, ?RESPONDERS}
                                      ,{queue_name, ?QUEUE_NAME}
                                      ,{queue_options, ?QUEUE_OPTIONS}
                                      ,{consume_options, ?CONSUME_OPTIONS}
                                      ,{route_options, ?ROUTE_OPTIONS}
                                      ,{basic_qos, 1}
                                     ], [Node, Options]).

-spec handle_originate_req/2 :: (wh_json:json_object(), proplist()) -> 'ok' | 'fail'.
handle_originate_req(JObj, Props) ->
    Endpoints = wh_json:get_ne_value(<<"Endpoints">>, JObj, []),
    case wapi_resource:originate_req_v(JObj) of
        false -> {'error', <<"originate failed to execute as JObj did not validate">>};
        true when Endpoints =:= [] -> {'error', <<"originate request had no endpoints">>};
        true ->
            Node = props:get_value(node, Props),
            DialSeparator = case wh_json:get_value(<<"Dial-Endpoint-Method">>, JObj, <<"single">>) of
                                <<"simultaneous">> when length(Endpoints) > 1 -> <<",">>;
                                _Else -> <<"|">>
                            end,
            DialStrings = ecallmgr_util:build_bridge_string(Endpoints, DialSeparator),
            Action = get_originate_action(wh_json:get_value(<<"Application-Name">>, JObj), JObj),
            Args = list_to_binary([ecallmgr_fs_xml:get_channel_vars(JObj), DialStrings, " ", Action]),
%%            io:format("freeswitch:api(~p, 'originate', \"~s\").~n", [Node, Args]),
            ecallmgr_util:fs_log(Node, "whistle originating call: ~s", [Args]),
            Result = freeswitch:api(Node, 'originate', wh_util:to_list(Args)),
            io:format("Result: ~p~n", [Result])
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
init([Node, Options]) ->
    {ok, #state{node=Node, options=Options}}.

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
handle_info({update_options, NewOptions}, State) ->
    {noreply, State#state{options=NewOptions}, hibernate};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{node=Node}) ->
    {reply, [{node, Node}]}.

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
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec get_originate_action/2 :: (ne_binary(), wh_json:json_object()) -> ne_binary().
get_originate_action(<<"fax">>, JObj) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    <<"$txfax(${http_get(", Data/binary, ")})">>;
get_originate_action(<<"transfer">>, JObj) ->
    case wh_json:get_value([<<"Application-Data">>, <<"Route">>], JObj) of
        undefined -> <<"error">>;
        Route ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "transfer:", wnm_util:to_e164(Route), " XML context_2' inline"])
    end;
get_originate_action(<<"bridge">>, JObj) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj),
    case ecallmgr_fs_xml:build_sip_route(Data, wh_json:get_value(<<"Invite-Format">>, Data)) of
        {error, timeout} -> <<"error">>;
        EndPoint ->
            list_to_binary(["'m:^:", get_unset_vars(JObj), "bridge:", EndPoint, "' inline"])
    end;
get_originate_action(_, _) ->
    <<"&park()">>.

-spec get_unset_vars/1 :: (wh_json:json_object()) -> string().
get_unset_vars(JObj) ->
    %% Refactor (Karl wishes he had unit tests here for you to use)
    ExportProps = [{K, <<>>} || K <- wh_json:get_value(<<"Export-Custom-Channel-Vars">>, JObj, [])],

    Export = [K || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], [{<<"Custom-Channel-Vars">>, wh_json:from_list(ExportProps)}])
                       ,([K, _] = string:tokens(binary_to_list(KV), "=")) =/= undefined],
    case [[$u,$n,$s,$e,$t,$: | K] || KV <- lists:foldr(fun ecallmgr_fs_xml:get_channel_vars/2, [], wh_json:to_proplist(JObj))
                                         ,not lists:member(begin [K, _] = string:tokens(binary_to_list(KV), "="), K end, Export)] of
        [] -> "";
        Unset -> [string:join(Unset, "^"), "^"]
    end.



-spec originate_channel/5 :: (atom(), pid(), ne_binary(), integer(), wh_json:json_object()) -> no_return().
originate_channel(Node, Pid, Route, AvailChan, JObj) ->
    ChannelVars = ecallmgr_fs_xml:get_channel_vars(JObj),
    Action = get_originate_action(wh_json:get_value(<<"Application-Name">>, JObj), JObj),
    OrigStr = binary_to_list(list_to_binary([ChannelVars, Route, " ", Action])),
    lager:debug("originate ~s on node ~s", [OrigStr, Node]),
    _ = ecallmgr_util:fs_log(Node, "whistle originating call: ~s", [OrigStr]),

    case freeswitch:bgapi(Node, originate, OrigStr) of
        {ok, JobId} ->
            receive
                {bgok, JobId, X} ->
                    CallID = erlang:binary_part(X, {4, byte_size(X)-5}),
                    lager:debug("originate on ~s with job id ~s received bgok ~s", [Node, JobId, X]),
                    CtlQ = start_call_handling(Node, CallID),
                    Pid ! {resource_consumed, CallID, CtlQ, AvailChan-1};
                {bgerror, JobId, Y} ->
                    ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
                    lager:debug("~s failed to originate, ~p", [Node, ErrMsg]),
                    Pid ! {resource_error, ErrMsg}
            after
                9000 ->
                    lager:debug("~s originate timed out", [Node]),
                    Pid ! {resource_error, timeout}
            end;
        {error, Y} ->
            ErrMsg = erlang:binary_part(Y, {5, byte_size(Y)-6}),
            lager:debug("~s failed to originate ~p", [Node, ErrMsg]),
            Pid ! {resource_error, ErrMsg};
        timeout ->
            lager:debug("~s originate timed out", [Node]),
            Pid ! {resource_error, timeout}
    end.

-spec start_call_handling/2 :: (atom(), ne_binary()) -> ne_binary() | {'error', 'amqp_error'}.
start_call_handling(Node, UUID) ->
    try
        {ok, CtlPid} = ecallmgr_call_sup:start_control_process(Node, UUID),
        {ok, _} = ecallmgr_call_sup:start_event_process(Node, UUID),

        ecallmgr_call_control:queue_name(CtlPid)
    catch
        _:_ -> {error, amqp_error}
    end.
