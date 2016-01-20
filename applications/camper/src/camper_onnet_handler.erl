%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(camper_onnet_handler).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).
-export([add_request/1
         ,available_device/2
        ]).

-include("camper.hrl").
-include_lib("whistle_apps/include/wh_hooks.hrl").

-define(RINGING_TIMEOUT, 30).

-record('state', {'requests' :: dict:dict()
                  ,'requestor_queues' :: dict:dict()
                  ,'sipnames' :: dict:dict()
                  ,'account_db' :: ne_binary()
                 }
       ).

-type state() :: #state{}.

-spec get_requests(state()) -> dict:dict().
get_requests(#state{'requests' = Val}) ->
    Val.

-spec get_requestor_queues(state()) -> dict:dict().
get_requestor_queues(#state{'requestor_queues' = Val}) ->
    Val.

-spec get_sipnames(state()) -> dict:dict().
get_sipnames(#state{'sipnames' = Val}) ->
    Val.

-spec get_account_db(state()) -> ne_binary().
get_account_db(#state{'account_db' = Val}) ->
    Val.

-spec set_requests(state(), dict:dict()) -> state().
set_requests(S, Val) ->
    S#state{'requests' = Val}.

-spec set_requestor_queues(state(), dict:dict()) -> state().
set_requestor_queues(S, Val) ->
    S#state{'requestor_queues' = Val}.

-spec add_request(wh_json:object()) -> 'ok'.
add_request(JObj) ->
    gen_server:cast(?MODULE, {'add_request', JObj}).

-spec available_device(ne_binary(), ne_binary()) -> 'ok'.
available_device(AccountId, SIPName) ->
    gen_server:cast(?MODULE, {'available_device', AccountId, SIPName}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
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
    lager:debug("onnet handler started"),
    {'ok', dict:new()}.

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
handle_cast({'add_request', JObj}, GlobalState) ->
    AccountDb = wh_json:get_value(<<"Account-DB">>, JObj),
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    Dev = {wh_json:get_value(<<"Authorizing-ID">>, JObj)
           ,wh_json:get_value(<<"Authorizing-Type">>, JObj)
          },
    Exten = wh_json:get_value(<<"Number">>, JObj),
    Targets = wh_json:get_value(<<"Targets">>, JObj, []),
    Timeout = timer:minutes(wh_json:get_value(<<"Timeout">>
                                              ,JObj
                                              ,whapps_config:get(?APP_NAME, ?TIMEOUT, ?DEFAULT_TIMEOUT)
                                             )),
    wh_hooks:register(AccountId, <<"CHANNEL_DESTROY">>),
    NewGlobal = with_state(AccountId
                          ,GlobalState
                          ,fun(Local) ->
                                   #state{'requests' = dict:merge(fun(_K, _V1, V2) -> V2 end
                                                                  ,get_requests(Local)
                                                                  ,make_requests(Targets, Dev, Exten, Timeout)
                                                                 )
                                          ,'sipnames' = dict:store(Exten, Targets, get_sipnames(Local))
                                          ,'requestor_queues' = maybe_update_queues(Targets, Dev, get_requestor_queues(Local))
                                          ,'account_db' = AccountDb
                                         }
                           end
                          ),
    {'noreply', NewGlobal};
handle_cast({'available_device', AccountId, SIPName}, GlobalState) ->
    NewGlobal = with_state(AccountId
                          ,GlobalState
                          ,fun(Local) ->
                               case dict:find(SIPName, get_requestor_queues(Local)) of
                                   {'ok', Q} ->
                                       maybe_handle_request(SIPName, Q, Local);
                                   _ ->
                                       lager:debug("Request not found"),
                                       Local
                               end
                           end
                         ),
    {'noreply', NewGlobal};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

-spec with_state(ne_binary(), dict:dict(), fun((state()) -> state())) -> dict:dict().
with_state(AccountId, Global, F) ->
    Local = case dict:find(AccountId, Global) of
                {'ok', #state{}=S} -> S;
                _ -> #state{'requests' = dict:new()
                            ,'requestor_queues' = dict:new()
                            ,'sipnames' = dict:new()
                           }
            end,
    dict:store(AccountId, F(Local), Global).

-spec maybe_handle_request(ne_binary(), queue:queue(), state()) -> state().
maybe_handle_request(SIPName, Q, Local) ->
    case queue:out(Q) of
        {{'value', Requestor}, NewQ} ->
            NewQueue = dict:store(SIPName, NewQ, get_requestor_queues(Local)),
            lager:debug("Request found, trying to handle"),
            handle_request(SIPName, Requestor, set_requestor_queues(Local, NewQueue));
        _ ->
            lager:debug("Request not found"),
            Local
    end.

-spec handle_request(ne_binary(), ne_binary(), state()) -> state().
handle_request(SIPName, Requestor, Local) ->
    Reqs = get_requests(Local),
    case dict:find({SIPName, Requestor}, Reqs) of
        {'ok', {Exten, Timeout}} ->
            {_, Now, _} = os:timestamp(),
            case Now > Timeout of
                'true' ->
                    lager:debug("Request(~s->~s) expired", [Requestor, Exten]);
                'false' ->
                    lager:debug("Originating call(~s->~s)", [Requestor, Exten]),
                    originate_call(Requestor, Exten, get_account_db(Local))
            end,
            lager:debug("Clearing request"),
            clear_request(Requestor, Exten, Local);
        _ ->
            lager:debug("Extension for request not found"),
            Local
    end.

-spec originate_call({ne_binary(), ne_binary()}, ne_binary(), ne_binary()) -> 'ok'.
originate_call({Id, Type}, Exten, AccountDb) ->
    Routines = [fun(C) -> whapps_call:set_account_db(AccountDb, C) end
                ,fun(C) -> whapps_call:set_account_id(wh_util:format_account_id(AccountDb, 'raw'), C) end
                ,fun(C) -> whapps_call:set_authorizing_id(Id, C) end
                ,fun(C) -> whapps_call:set_authorizing_type(Type, C) end
               ],
    Call = lists:foldl(fun(F, C) -> F(C) end, whapps_call:new(), Routines),
    case get_endpoints(Call, Id, Type) of
        [] ->
            lager:debug("Can't find endpoints");
        Endpoints ->
            originate_quickcall(Endpoints, Exten, Call)
    end.

-spec originate_quickcall(wh_json:objects(), ne_binary(), whapps_call:call()) -> 'ok'.
originate_quickcall(Endpoints, Exten, Call) ->
    CCVs = [{<<"Account-ID">>, whapps_call:account_id(Call)}
            ,{<<"Retain-CID">>, <<"true">>}
            ,{<<"Inherit-Codec">>, <<"false">>}
            ,{<<"Authorizing-Type">>, whapps_call:authorizing_type(Call)}
            ,{<<"Authorizing-ID">>, whapps_call:authorizing_id(Call)}
           ],
    MsgId = wh_util:rand_hex_binary(16),

    Request = [{<<"Application-Name">>, <<"transfer">>}
               ,{<<"Application-Data">>, wh_json:from_list([{<<"Route">>, Exten}])}
               ,{<<"Msg-ID">>, MsgId}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, ?RINGING_TIMEOUT}
               ,{<<"Outbound-Caller-ID-Name">>, <<"Campered call">>}
               ,{<<"Outbound-Caller-ID-Number">>, Exten}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Continue-On-Fail">>, 'false'}
               ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
               ,{<<"Export-Custom-Channel-Vars">>, [<<"Account-ID">>, <<"Retain-CID">>, <<"Authorizing-ID">>, <<"Authorizing-Type">>]}
               | wh_api:default_headers(<<"resource">>, <<"originate_req">>, ?APP_NAME, ?APP_VERSION)
              ],
    wapi_resource:publish_originate_req(props:filter_undefined(Request)),
    lager:debug("Originate request published").

-spec get_endpoints(whapps_call:call(), ne_binary(), ne_binary()) -> wh_json:objects().
get_endpoints(Call, EndpointId, <<"device">>) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                   ]),
    case cf_endpoint:build(EndpointId, Properties, Call) of
        {'error', _} -> [];
        {'ok', []} -> [];
        {'ok', Endpoints} -> Endpoints
    end;
get_endpoints(Call, UserId, <<"user">>) ->
    Properties = wh_json:from_list([{<<"can_call_self">>, 'true'}
                                    ,{<<"suppress_clid">>, 'true'}
                                   ]),
    lists:foldl(fun(EndpointId, Acc) ->
        case cf_endpoint:build(EndpointId, Properties, Call) of
            {'ok', Endpoint} -> Endpoint ++ Acc;
            {'error', _E} -> Acc
        end
    end, [], cf_attributes:owned_by(UserId, <<"device">>, Call));
get_endpoints(_, _, _) ->
    [].

-spec clear_request(ne_binary(), ne_binary(), state()) -> state().
clear_request(Requestor, Exten, Local) ->
    {'ok', SIPNames} = dict:find(Exten, get_sipnames(Local)),
    lists:foldl(fun(SIPName, Acc) ->
                        Qs = get_requestor_queues(Acc),
                        {'ok', Q} = dict:find(SIPName, Qs),
                        Queue = queue:filter(fun(X) -> X =/= Requestor end, Q),
                        Qs1 = dict:store(SIPName, Queue, Qs),
                        Reqs = dict:erase({SIPName, Requestor}, get_requests(Acc)),
                        set_requests(set_requestor_queues(Acc, Qs1), Reqs)
                end, Local, SIPNames).

-spec make_requests(ne_binaries(), {ne_binary(), ne_binary()}, ne_binary(), non_neg_integer()) -> dict:dict().
make_requests(SIPNames, Requestor, Exten, Timeout) ->
    R = lists:map(fun(SIPName) -> {SIPName, Requestor} end, SIPNames),
    {_, Seconds, _} = os:timestamp(),
    lists:foldl(fun(Req, Acc) -> dict:store(Req, {Exten, Seconds + Timeout}, Acc) end, dict:new(), R).

-spec maybe_update_queues(ne_binaries(), {ne_binary(), ne_binary()}, dict:dict()) -> dict:dict().
maybe_update_queues(SIPNames, Requestor, Queues) ->
    lists:foldl(fun(SIPName, Acc) ->
                        Q = case dict:find(SIPName, Acc) of
                                {'ok', Queue} -> Queue;
                                _ -> queue:new()
                            end,
                        case queue:member(Requestor, Q) of
                            'true' -> Acc;
                            'false' -> dict:store(SIPName,queue:in(Requestor, Q),Acc)
                        end
                end
                ,Queues
                ,SIPNames
               ).

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
handle_info(?HOOK_EVT(_AccountId, _, JObj), State) ->
    AcctId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    SIPName = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Username">>], JObj),
    lager:debug("available device: ~s(~s)", [SIPName, AcctId]),
    ?MODULE:available_device(AcctId, SIPName),
    {'noreply', State};
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
    lager:debug("onnet handler ~p termination", [_Reason]),
    'ok'.

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
