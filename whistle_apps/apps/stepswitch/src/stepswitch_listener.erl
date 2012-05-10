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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          last_doc_change = {<<>>, [<<>>]}
          ,resrcs = []
         }).

-define(BINDINGS, [{route, []}
                   ,{offnet_resource, []}
                  ]).
-define(RESPONDERS, [{stepswitch_inbound, [{<<"dialplan">>, <<"route_req">>}]}
                     ,{stepswitch_outbound, [{<<"resource">>, <<"offnet_req">>}]}
                    ]).
-define(QUEUE_NAME, ?RESOURCE_QUEUE_NAME).
-define(QUEUE_OPTIONS, [{exclusive, false}, {auto_delete, true}, {nowait, false}]).
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
    gen_listener:start_link({local, ?SERVER}, ?MODULE, [{bindings, ?BINDINGS}
                                                        ,{responders, ?RESPONDERS}
                                                        ,{queue_name, ?QUEUE_NAME}
                                                        ,{queue_options, ?QUEUE_OPTIONS}
                                                        ,{consume_options, ?CONSUME_OPTIONS}
                                                        ,{basic_qos, 1}
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
    lager:debug("starting new stepswitch outbound responder"),
    stepswitch_maintenance:refresh(),
    {ok, #state{resrcs=get_resrcs()}}.

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
handle_call({lookup_number, Number}, From, State) ->
    spawn(fun() ->
                  gen_server:reply(From, stepswitch_util:lookup_number(Number))
          end),
    {noreply, State};

handle_call({reload_resrcs}, _, State) ->
    {reply, ok, State#state{resrcs=get_resrcs()}};

handle_call({process_number, Number}, From, #state{resrcs=Resrcs}=State) ->
    spawn(fun() ->
                  Num = wnm_util:to_e164(wh_util:to_binary(Number)),
                  EPs = print_endpoints(stepswitch_util:evaluate_number(Num, Resrcs), 0, []),
                  gen_server:reply(From, EPs)
          end),
    {noreply, State};

handle_call({process_number, Number, Flags}, From, #state{resrcs=R1}=State) ->
    spawn(fun() ->
                  R2 = stepswitch_util:evaluate_flags(Flags, R1),
                  Num = wnm_util:to_e164(wh_util:to_binary(Number)),
                  EPs = print_endpoints(stepswitch_util:evaluate_number(Num, R2), 0, []),
                  gen_server:reply(From, EPs)
          end),
    {noreply, State};

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
handle_info({document_changes, DocId, Changes}, #state{last_doc_change={DocId, Changes}}=State) ->
    %% Ignore the duplicate document change notifications used for keep alives
    {noreply, State};

handle_info({document_changes, DocId, [Changes]=C}, #state{resrcs=Resrcs}=State) ->
    Rev = wh_json:get_value(<<"rev">>, Changes),
    case lists:keysearch(DocId, #resrc.id, Resrcs) of
        {value, #resrc{rev=Rev}} -> {noreply, State#state{last_doc_change={DocId, C}}, hibernate};
        _ -> {noreply, State#state{resrcs=update_resrc(DocId, Resrcs), last_doc_change={DocId, C}}, hibernate}
    end;

handle_info({document_deleted, DocId}, #state{resrcs=Resrcs}=State) ->
    case lists:keyfind(DocId, #resrc.id, Resrcs) of
        false -> {noreply, State};
        _ ->
            lager:debug("resource ~s deleted", [DocId]),
            couch_mgr:rm_change_handler(?RESOURCES_DB, DocId),
            {noreply, State#state{resrcs=lists:keydelete(DocId, #resrc.id, Resrcs)}, hibernate}
    end;

handle_info(_Info, State) ->
    lager:debug("Unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{resrcs=Rs}) ->
    {reply, [{resources, Rs}]}.

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
terminate(_Reason, _) ->
    ok.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets a list of all active resources from the DB
%% @end
%%--------------------------------------------------------------------
-spec get_resrcs/0 :: () -> [#resrc{}].
get_resrcs() ->
    case couch_mgr:get_results(?RESOURCES_DB, ?LIST_RESOURCES_BY_ID, [{<<"include_docs">>, 'true'}]) of
        {ok, Resrcs} ->
            _ = [couch_mgr:add_change_handler(?RESOURCES_DB, wh_json:get_value(<<"id">>, R))
                 || R <- Resrcs],
            [create_resrc(wh_json:get_value(<<"doc">>, R))
             || R <- Resrcs, wh_util:is_true(wh_json:get_value([<<"doc">>, <<"enabled">>], R, 'true'))];
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Syncs a resource with the DB and updates it in the list
%% of resources
%% @end
%%--------------------------------------------------------------------
-spec update_resrc/2 :: (ne_binary(), [#resrc{},...] | []) -> [#resrc{},...] | [].
update_resrc(DocId, Resrcs) ->
    lager:debug("received notification that resource ~s has changed", [DocId]),
    case couch_mgr:open_doc(?RESOURCES_DB, DocId) of
        {ok, JObj} ->
            case wh_util:is_true(wh_json:get_value(<<"enabled">>, JObj)) of
                'true' ->
                    NewResrc = create_resrc(JObj),
                    lager:debug("resource ~s updated to rev ~s", [DocId, NewResrc#resrc.rev]),
                    [NewResrc|lists:keydelete(DocId, #resrc.id, Resrcs)];
                false ->
                    lager:debug("resource ~s disabled", [DocId]),
                    lists:keydelete(DocId, #resrc.id, Resrcs)
            end;
        {error, R} ->
            lager:debug("removing resource ~s, ~w", [DocId, R]),
            couch_mgr:rm_change_handler(?RESOURCES_DB, DocId),
            lists:keydelete(DocId, #resrc.id, Resrcs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a resrc JSON object it builds a resrc record and
%% populates it with all enabled gateways
%% @end
%%--------------------------------------------------------------------
-spec create_resrc/1 :: (wh_json:json_object()) -> #resrc{}.
create_resrc(JObj) ->
    Default = #resrc{},
    Id = wh_json:get_value(<<"_id">>, JObj),
    lager:debug("loading resource ~s", [Id]),

    #resrc{id = Id
           ,rev =
               wh_json:get_value(<<"_rev">>, JObj)
           ,weight_cost =
               constrain_weight(wh_json:get_value(<<"weight_cost">>, JObj, Default#resrc.weight_cost))
           ,grace_period =
               wh_json:get_integer_value(<<"grace_period">>, JObj, Default#resrc.grace_period)
           ,flags =
               wh_json:get_value(<<"flags">>, JObj, Default#resrc.flags)
           ,rules =
               [R2 || R1 <- wh_json:get_value(<<"rules">>, JObj, Default#resrc.rules)
                          ,(R2 = compile_rule(R1, Id)) =/= error]
           ,gateways =
               [create_gateway(G, Id) || G <- wh_json:get_value(<<"gateways">>, JObj, []),
                                         wh_json:is_true(<<"enabled">>, G, 'true')]
           ,is_emergency =
               wh_json:is_true(<<"emergency">>, JObj)
               orelse (wh_json:get_value([<<"caller_id_options">>, <<"type">>], JObj) =:= <<"emergency">>)
          }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec create_gateway/2 :: (wh_json:json_object(), ne_binary()) -> #gateway{}.
create_gateway(JObj, Id) ->
    Default = #gateway{},

    EndpointType = endpoint_type(JObj, Default),
    EndpointOptions = endpoint_options(JObj, EndpointType),

    #gateway{resource_id = Id
             ,server =
                 wh_json:get_value(<<"server">>, JObj, Default#gateway.server)
             ,realm =
                 wh_json:get_value(<<"realm">>, JObj, Default#gateway.realm)
             ,username =
                 wh_json:get_value(<<"username">>, JObj, Default#gateway.username)
             ,password =
                 wh_json:get_value(<<"password">>, JObj, Default#gateway.password)
             ,route =
                 wh_json:get_value(<<"route">>, JObj, Default#gateway.route)
             ,prefix =
                 wh_util:to_binary(wh_json:get_value(<<"prefix">>, JObj, Default#gateway.prefix))
             ,suffix =
                 wh_util:to_binary(wh_json:get_value(<<"suffix">>, JObj, Default#gateway.suffix))
             ,codecs =
                 wh_json:get_value(<<"codecs">>, JObj, Default#gateway.codecs)
             ,bypass_media =
                 wh_json:get_value(<<"bypass_media">>, JObj)
             ,caller_id_type =
                 wh_json:get_value(<<"caller_id_type">>, JObj, Default#gateway.caller_id_type)
             ,sip_headers =
                 wh_json:get_value(<<"custom_sip_headers">>, JObj, Default#gateway.sip_headers)
             ,progress_timeout =
                 wh_util:to_integer(wh_json:get_value(<<"progress_timeout">>, JObj, Default#gateway.progress_timeout))
             ,invite_format =
                 wh_json:get_value(<<"invite_format">>, JObj, Default#gateway.invite_format)
             ,endpoint_type =
                 EndpointType
             ,endpoint_options =
                 EndpointOptions
            }.

endpoint_type(JObj, Default) ->
    case wh_json:get_value(<<"span">>, JObj) of
        undefined -> Default#gateway.endpoint_type;
        _ -> <<"freetdm">>
    end.

endpoint_options(JObj, <<"freetdm">>) ->
    wh_json:from_list([{<<"Span">>, wh_json:get_value(<<"span">>, JObj)}
                       ,{<<"Channel-Selection">>, wh_json:get_value(<<"channel_selection">>, JObj, <<"ascending">>)}
                      ]);
endpoint_options(_, _) ->
    wh_json:new().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for re:compile so we can log rules that fail (including
%% which resource it was on).
%% @end
%%--------------------------------------------------------------------
-spec compile_rule/2 :: (ne_binary(), ne_binary()) -> re:mp() | 'error'.
compile_rule(Rule, Id) ->
    case re:compile(Rule) of
        {ok, MP} ->
            lager:debug("compiled ~s on resource ~s", [Rule, Id]),
            MP;
        {error, R} ->
            lager:debug("bad rule '~s' on resource ~s, ~p", [Rule, Id, R]),
            error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% constrain the weight on a scale from 1 to 100
%% @end
%%--------------------------------------------------------------------
-spec constrain_weight/1 :: (ne_binary() | integer()) -> integer().
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(wh_util:to_integer(W));
constrain_weight(W) when W > 100 -> 100;
constrain_weight(W) when W < 1 -> 1;
constrain_weight(W) -> W.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a list of tuples for humans from the lookup number request
%% @end
%%--------------------------------------------------------------------
-spec print_endpoints/3 :: (endpoints(), non_neg_integer(), list()) -> list().
print_endpoints([], _, Acc) ->
    lists:reverse(Acc);
print_endpoints([{_, GracePeriod, Number, [Gateway], _}|T], Delay, Acc0) ->
    print_endpoints(T, Delay + GracePeriod, [print_endpoint(Number, Gateway, Delay)|Acc0]);
print_endpoints([{_, GracePeriod, Number, Gateways, _}|T], Delay, Acc0) ->
    {D2, Acc1} = lists:foldl(fun(Gateway, {0, AccIn}) ->
                                     {2, [print_endpoint(Number, Gateway, 0)|AccIn]};
                                 (Gateway, {D0, AccIn}) ->
                                     {D0 + 2, [print_endpoint(Number, Gateway, D0)|AccIn]}
                            end, {Delay, Acc0}, Gateways),
    print_endpoints(T, D2 - 2 + GracePeriod, Acc1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a tuple for humans from the lookup number request
%% @end
%%--------------------------------------------------------------------
-spec print_endpoint/3 :: (ne_binary(), #gateway{}, non_neg_integer()) -> {ne_binary(), non_neg_integer(), ne_binary()}.
print_endpoint(Number, #gateway{resource_id=ResourceID}=Gateway, Delay) ->
    {ResourceID, Delay, stepswitch_util:get_dialstring(Gateway, Number)}.
