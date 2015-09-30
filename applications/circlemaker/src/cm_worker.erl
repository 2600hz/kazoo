%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is main application supervisor module
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_worker).

-behaviour(gen_server).

-include("circlemaker.hrl").
-include_lib("eradius/include/eradius_lib.hrl").

%% API
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
         ,start_link/1
         ,send_req/3
         ,send_req/2
        ]).

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
start_link(_) ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Make a AuthN request to a worker
%% @end
%%--------------------------------------------------------------------
-spec send_req(pid(), wh_json:object(), pid()) -> ok.
send_req(Worker, JObj, Caller) ->
    RequestType = cm_util:determine_aaa_request_type(JObj),
    lager:debug("Request type is ~p", [RequestType]),
    FixedRequestType = case RequestType of
                           'custom' -> 'custom_req';
                           'authn' -> 'auth_req';
                           'authz' -> 'auth_req';
                           'accounting' -> 'accounting_req'
                       end,
    gen_server:cast(Worker, {FixedRequestType, Caller, JObj}).

-spec send_req(pid(), wh_json:object()) -> ok.
send_req(Worker, JObj) ->
    RequestType = cm_util:determine_aaa_request_type(JObj),
    lager:debug("Request type is ~p", [RequestType]),
    FixedRequestType = case RequestType of
                           'custom' -> 'custom_req';
                           'authn' -> 'auth_req';
                           'authz' -> 'auth_req';
                           'accounting' -> 'accounting_req'
                       end,
    gen_server:cast(Worker, {FixedRequestType, self(), JObj}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init([]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {'ok', []}.

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
handle_cast({'auth_req', SenderPid, JObj}, State) ->
    wh_util:put_callid(JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj, <<"system_config">>),
    lager:debug("auth_req message for user ~p received for account ~p",
        [wh_json:get_value(<<"Auth-User">>, JObj), AccountId]),
    Response = maybe_aaa_mode(JObj, AccountId),
    cm_pool_mgr:send_authn_response(SenderPid, Response, JObj, self()),
    {'noreply', State};
handle_cast({'custom_req', SenderPid, JObj}, State) ->
    wh_util:put_callid(JObj),
    AccountId = wh_json:get_value([<<"Custom-Msg">>, <<"Account-ID">>], JObj, <<"system_config">>),
    lager:debug("custom_req message received"),
    Response = maybe_aaa_mode(JObj, AccountId),
    cm_pool_mgr:send_custom_response(SenderPid, Response, JObj, self()),
    {'noreply', State};
handle_cast({'accounting_req', SenderPid, JObj}, State) ->
    wh_util:put_callid(JObj),
    AccountId = wh_json:get_value([?CCV, <<"Account-ID">>], JObj, <<"system_config">>),
    lager:debug("accounting_req message received for account ~p", [AccountId]),
    Response = maybe_aaa_mode(JObj, AccountId),
    lager:debug("Accounting response is ~p", [Response]),
    cm_pool_mgr:send_accounting_response(SenderPid, Response, JObj, self()),
    {'noreply', State};
handle_cast(_Message, State) ->
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
handle_info(Info, State) ->
    lager:debug("unexpected info message is ~p", [Info]),
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
    [].

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Checks the aaa_mode for currently used account
%% @end
%%--------------------------------------------------------------------
-spec maybe_aaa_mode(wh_json:object(), api_binary()) -> {'ok', 'aaa_mode_off'} |
                                                        {'ok', tuple()} |
                                                        {'error', 'no_respond'}.
maybe_aaa_mode(_JObj, 'undefined') ->
    lager:error("error: call for undefined account"),
    {'error', 'no_respond'};
maybe_aaa_mode(JObj, <<"system_config">> = AccountId) ->
    % add system_config as source account
    JObj1 = wh_json:set_value(<<"Account-ID">>, AccountId, JObj),
    AaaProps = whapps_config:get_all_kvs(<<"circlemaker">>),
    case props:get_value(<<"aaa_mode">>, AaaProps) of
        <<"off">> ->
            lager:debug("AAA functionality disabled for system_config account"),
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            lager:debug("AAA functionality enabled for system_config"),
            Result = maybe_authn_special_case(JObj1, AaaProps, AccountId, 'undefined'),
            {'ok', Result}
    end;
maybe_aaa_mode(JObj, AccountId) when is_binary(AccountId) ->
    {'ok', Account} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Account),
    Realm = wh_json:get_value(<<"realm">>, Account),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(AccountDb, <<"aaa">>),
    AaaProps = wh_json:recursive_to_proplist(AaaDoc),
    JObj1 = wh_json:set_value([<<"Custom-Auth-Vars">>, <<"Account-Realm">>], Realm, JObj),
    case props:get_value(<<"aaa_mode">>, AaaProps) of
        <<"off">> ->
            lager:debug("AAA functionality disabled for the ~p account", [AccountId]),
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            % AAA functionality is on for this account
            lager:debug("AAA functionality enabled for the ~p account", [AccountId]),
            ParentAccountId = cm_util:parent_account_id(Account),
            maybe_authn_special_case(JObj1, AaaProps, AccountId, ParentAccountId);
        <<"inherit">> ->
            % AAA functionality is in the 'inherit' mode for this account
            lager:debug("AAA functionality enabled (inherit mode) for the ~p account", [AccountId]),
            ParentAccountId = cm_util:parent_account_id(Account),
            maybe_authn_special_case(JObj1, AaaProps, AccountId, ParentAccountId)
    end.

-spec maybe_authn_special_case(wh_json:object(), proplist(), api_binary(), api_binary()) -> {'ok', 'aaa_mode_off'} |
                                                                            {'ok', tuple()} |
                                                                            {'error', 'no_respond'}.
maybe_authn_special_case(JObj, AaaProps, AccountId, ParentAccountId) ->
    AaaRequestType = cm_util:determine_aaa_request_type(JObj),
    AuthnServersList = props:get_value(<<"authentication">>, AaaProps),
    AaaMode = props:get_value(<<"aaa_mode">>, AaaProps),
    case {AaaRequestType, AaaMode, length(AuthnServersList)} of
        % special case processing - if the "aaa_mode" = "on" and no authn servers in the "authentication" list,
        % then it's assumed that standart authentication is used, and the request is accepted
        %{'authn', <<"on">>, 0} -> {'ok', {{'radius_request', _, 'accept', [], _, _, _, _}, AccountId}};
        {'authn', <<"on">>, 0} -> {'ok', {'aaa_mode_on_and_no_servers', AccountId}};
        _ -> maybe_suitable_servers(JObj, AaaProps, AccountId, ParentAccountId)
    end.

-spec maybe_suitable_servers(wh_json:object(), proplist(), api_binary(), api_binary()) -> {'ok', 'aaa_mode_off'} |
                                                                            {'ok', tuple()} |
                                                                            {'error', 'no_respond'}.
maybe_suitable_servers(JObj, AaaProps, AccountId, ParentAccountId) ->
    AaaRequestType = cm_util:determine_aaa_request_type(JObj),
    ServersJson = props:get_value(<<"servers">>, AaaProps),
    lager:debug("Available servers count for this account is ~p", [length(ServersJson)]),
    Servers = [S || S <- wh_json:recursive_to_proplist(ServersJson), props:get_is_true(<<"enabled">>, S)],
    lager:debug("Active servers: ~p", [Servers]),
    ServersFilterArg = case AaaRequestType of
                           'authn' -> <<"authentication">>;
                           'authz' -> <<"authorization">>;
                           'accounting' -> <<"accounting">>;
                           'custom' -> <<"custom">>
                        end,
    FilteredServers = [S || S <- Servers,
        lists:member(props:get_value(<<"name">>, S), props:get_value(ServersFilterArg, AaaProps))],
    maybe_server_request(FilteredServers, JObj, AaaProps, AccountId, ParentAccountId).

-spec maybe_server_request(list(), wh_json:object(), proplist(), ne_binary(), ne_binary()) -> {'ok', 'aaa_mode_off'} |
                                                                                 {'ok', tuple()} |
                                                                                 {'error', 'no_respond'}.
maybe_server_request([], JObj, AaaProps, _AccountId, ParentAccountId) ->
    lager:debug("all active AAA servers for this account were checked"),
    case props:get_value(<<"aaa_mode">>, AaaProps) of
        <<"inherit">> ->
            % we are in the "inherit" mode so we should get parent account and use its AAA settings
            lager:debug("the 'inherit' mode used - switching to parent account"),
            maybe_aaa_mode(JObj, ParentAccountId);
        _ ->
            lager:debug("no 'inherit' mode - servers search stopped"),
            {'error', 'no_respond'}
    end;
maybe_server_request([Server | Servers] = AllServers, JObj, AaaProps, AccountId, ParentAccountId) ->
    case cm_util:network_address_to_ip_tuple(props:get_value(<<"address">>, Server)) of
        {'error', Error} ->
            lager:debug("server can't be unresolved: ~p", [Error]),
            maybe_server_request(Servers, JObj, AaaProps, AccountId, ParentAccountId);
        Address ->
            maybe_channel_blocked(AllServers, Address, JObj, AaaProps, AccountId, ParentAccountId)
    end.

-spec maybe_block_processing(wh_json:object(), wh_json:object(), ne_binary()) -> any().
maybe_block_processing(JObj, AaaProps, BlockKey) ->
    {ChannelProps, Type, _InboundOriginate} = cm_util:determine_channel_type(JObj),
    % check special case when a loopback channel transformed to normal,
    % so on CHANNEL_CREATE and authz operation he has loopback type, but on channel destroy it has loopback type.
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    Type1 = case {wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), Type} of
                {<<"call_event">>, <<"CHANNEL_CREATE">>, 'loopback'} ->
                    cm_util:mark_channel_as_loopback(CallId),
                    'loopback';
                {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
                    case cm_util:is_channel_loopback(CallId) of
                        'true' ->
                            % cleanup
                            cm_util:unmark_channel_as_loopback(CallId),
                            'loopback';
                        'false' ->
                            Type
                    end;
                _ ->
                    Type
            end,
    BlockChannelsList = props:get_value(BlockKey, AaaProps),
    lager:debug("BlockChannelsList is ~p", [BlockChannelsList]),
    case BlockChannelsList of
        'undefined' -> 'ok';
        _ ->
            FilteredList = props:filter(
                fun(PropEntry) ->
                    lager:debug("PropEntry entry is ~p", [PropEntry]),
                    Value = props:get_value(<<"channel">>, PropEntry),
                    lager:debug("Value of 'channel' is ~p", [Value]),
                    lager:debug("Left is ~p and Right is ~p", [ordsets:from_list(Value), ordsets:from_list(ChannelProps)]),
                    ordsets:from_list(Value) == ordsets:from_list(ChannelProps) end,
                BlockChannelsList),
            lager:debug("FilteredList is ~p and Type is ~p", [FilteredList, Type1]),
            case {length(FilteredList), Type1} of
                {_, 'loopback'} -> 'blocked';
                {0, 'normal'} -> 'ok';
                {_, 'normal'} -> 'blocked'
            end
    end.

maybe_channel_blocked(AllServers, Address, JObj, AaaProps, AccountId, ParentAccountId) ->
    AaaRequestType = cm_util:determine_aaa_request_type(JObj),
    case AaaRequestType of
        'authn' ->
            maybe_eradius_request(AllServers, Address, JObj, AaaProps, AccountId, ParentAccountId);
        'custom' ->
            maybe_eradius_request(AllServers, Address, JObj, AaaProps, AccountId, ParentAccountId);
        'authz' ->
            case maybe_block_processing(JObj, AaaProps, <<"block_authz">>) of
                'ok' ->
                    maybe_eradius_request(AllServers, Address, JObj, AaaProps, AccountId, ParentAccountId);
                'blocked' ->
                    lager:debug("Authz for this type of channel is blocked. Request bypassed."),
                    Queue = wh_json:get_value(<<"Server-ID">>, JObj),
                    JObj1 = wh_json:set_values([{<<"Event-Name">>, <<"authz.broadcast.resp">>}
                                                ,{<<"Is-Authorized">>, <<"true">>}
                                                ,{<<"App-Name">>, ?APP_NAME}
                                                ,{<<"App-Version">>, ?APP_VERSION}]
                                                ,JObj),
                    wapi_authz:publish_authz_resp(Queue, JObj1)
            end;
        'accounting' ->
            case maybe_block_processing(JObj, AaaProps, <<"block_accounting">>) of
                'ok' ->
                    maybe_eradius_request(AllServers, Address, JObj, AaaProps, AccountId, ParentAccountId);
                'blocked' ->
                    lager:debug("Accounting for this type of channel is blocked. Request bypassed.")
            end
    end.

-spec maybe_eradius_request(list(), ne_binary(), wh_json:object(), proplist(), ne_binary(), ne_binary()) ->
                                                                                            {'ok', 'aaa_mode_off'} |
                                                                                            {'ok', tuple()} |
                                                                                            {'error', 'no_respond'}.
maybe_eradius_request([Server | Servers], Address, JObj, AaaProps, AccountId, ParentAccountId) ->
    Port = props:get_value(<<"port">>, Server),
    Secret = props:get_value(<<"secret">>, Server),
    {'ok', Account} = couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId),
    AccountDb = wh_json:get_value(<<"pvt_account_db">>, Account),
    {'ok', AaaDoc} = couch_mgr:open_cache_doc(AccountDb, <<"aaa">>),
    % TODO: next line should be moved to the cm_util
    CustomData = wh_json:set_values([{<<"account_id">>, AccountId}
                                     ,{<<"account_name">>, wh_json:get_value(<<"name">>, Account)}
                                     ,{<<"account_realm">>, wh_json:get_value(<<"realm">>, Account)}
                                    ], wh_json:new()),
    {AllAVPs, RadiusCmdType} = case cm_util:determine_aaa_request_type(JObj) of
                  'authz' = RequestType ->
                      lager:debug("Operation is authz"),
                      WholeRequest = wh_json:get_value(<<"Custom-Auth-Vars">>, JObj),
                      WholeRequest1 = cm_util:append_resource_name_to_request(WholeRequest),
                      WholeRequest2 = cm_util:insert_device_info_if_needed(WholeRequest1, RequestType),
                      % TODO: it's a quick hack, should be removed.
                      WholeRequestTemp = wh_json:set_values([{<<"Event-Category">>, <<"authz">>}
                                                             ,{<<"Event-Name">>, <<"authz">>}
                                                            ], WholeRequest2),
                      WholeRequest3 = cm_util:transform_leg_kvs(WholeRequestTemp, CustomData, AaaDoc),
                      WholeRequest4 = wh_json:delete_keys([<<"Event-Category">>, <<"Event-Name">>], WholeRequest3),
                      lager:debug("Request is: ~p", [WholeRequest4]),
                      {cm_util:maybe_translate_kv_into_avps(WholeRequest4, AaaProps, RequestType), 'request'};
                  'authn' = RequestType ->
                      lager:debug("Operation is authn"),
                      lager:debug("Request is: ~p", [JObj]),
                      {cm_util:maybe_translate_kv_into_avps(JObj, AaaProps, RequestType), 'request'};
                  'custom' = RequestType ->
                      lager:debug("Operation is custom"),
                      JObjUnwrapped = wh_json:get_value(<<"Custom-Msg">>, JObj),
                      lager:debug("Request is: ~p", [JObjUnwrapped]),
                      {cm_util:maybe_translate_kv_into_avps(JObjUnwrapped, AaaProps, RequestType), 'request'};
                  'accounting' = RequestType ->
                      lager:debug("Operation is accounting"),
                      JObj1 = cm_util:append_resource_name_to_request(JObj),
                      % delete device from ETS if It's accounting Stop
                      case wh_json:get_value(<<"Acct-Status-Type">>, JObj1) of
                          <<"Stop">> ->
                              CallId = wh_json:get_value(<<"Call-ID">>, JObj1),
                              lager:debug("Delete SIP Device Info from ETS for CallId ~p", [CallId]),
                              ets:delete(?ETS_DEVICE_INFO, CallId);
                          _ ->
                              'ok'
                      end,
                      JObj2 = cm_util:transform_leg_kvs(JObj1, CustomData, AaaDoc),
                      lager:debug("Request is: ~p", [JObj2]),
                      {cm_util:maybe_translate_kv_into_avps(JObj2, AaaProps, RequestType), 'accreq'}
              end,
    lager:debug("trying to resolve the next AVPs: ~p", [AllAVPs]),
    % prepare attribute param list
    ParamList = [{eradius_dict:lookup_by_name(AccountId, 'attribute2', Key)
                  ,lookup_value_by_name(AccountId, Value)} || {Key, Value} <- AllAVPs],
    ParamList1 = [{Key, Value} || {Key, Value} <- ParamList, Key =/= 'undefined'],
    Request = eradius_lib:set_attributes(#radius_request{cmd = RadiusCmdType}, ParamList1),
    lager:debug("checking next server ~p (~p:~p)", [Server, Address, Port]),
    lager:debug("RADIUS param list is ~p", [ParamList1]),
    lager:debug("final AAA-request is ~p", [Request]),
    case eradius_client:send_request({Address, Port, Secret}, Request) of
        {'ok', Response, _Authenticator} ->
            Result = eradius_lib:decode_request(Response, Secret),
            lager:debug("response received: ~p", [Result]),
            {'ok', {Result, AccountId}};
        Error ->
            lager:debug("error response received: ~p", [Error]),
            maybe_server_request(Servers, JObj, AaaProps, AccountId, ParentAccountId)
    end.

lookup_value_by_name(AccountId, ValueAsName) when is_binary(ValueAsName) ->
    case eradius_dict:lookup_by_name(AccountId, 'value2', ValueAsName) of
        'undefined' -> ValueAsName;
        {_AttrId, Value} when is_integer(Value) -> Value;
        Value when is_integer(Value) ->Value
    end;
lookup_value_by_name(_AccountId, ValueAsName) ->
    ValueAsName.
