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
         ,handle_authn/3]).

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
-spec handle_authn(pid(), wh_json:object(), pid()) -> ok.
handle_authn(Worker, JObj, Caller) ->
    gen_server:cast(Worker, {'authn_req', Caller, JObj}).

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
handle_cast({'authn_req', SenderPid, JObj}, State) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj, <<"system_config">>),
    lager:debug("authn_req message ~p received for account ~p", [JObj, AccountId]),
    case maybe_aaa_mode(JObj, AccountId) of
        {'ok', Response} ->
            cm_pool_mgr:send_authn_response(SenderPid, Response, JObj, self());
        {'error', Reason} ->
            cm_pool_mgr:send_authn_error(SenderPid, Reason, JObj, self())
    end,
    {'noreply', State};
handle_cast(Message, State) ->
    lager:debug("Message=~p~n", [Message]),
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
    {'reply', {'error', 'not_implemented'}, State}.

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
    lager:debug("AccountId=~p~n", [AccountId]),
    % used system_config-related AAA settings
    AAAProtocol = wh_json:get_value(<<"AAA-Protocol">>, JObj),
    % add system_config as source account
    JObj1 = wh_json:set_value(<<"Account-ID">>, AccountId, JObj),
    AaaProps = whapps_config:get_all_kvs(<<"circlemaker">>),
    case props:get_value(<<"aaa_mode">>, AaaProps) of
        <<"off">> ->
            lager:debug("AAA functionality disabled for system_config account"),
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            lager:debug("AAA functionality enabled for system_config"),
            Result = maybe_suitable_servers(JObj1, AaaProps, AAAProtocol, 'undefined'),
            {'ok', Result}
    end;
maybe_aaa_mode(JObj, AccountId) when is_binary(AccountId) ->
    AAAProtocol = wh_json:get_value(<<"AAA-Protocol">>, JObj),
    {ok, Account} = couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId),
    Value = wh_json:get_value(<<"value">>, Account),
    AccountDb = wh_json:get_value(<<"account_db">>, Value, Account),
    {ok, AaaDoc} = couch_mgr:open_doc(AccountDb, <<"aaa">>),
    case wh_json:get_value(<<"aaa_mode">>, AaaDoc) of
        <<"off">> ->
            lager:debug("AAA functionality disabled for the ~p account", [AccountId]),
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            % AAA functionality is on for this account
            lager:debug("AAA functionality enabled for the ~p account", [AccountId]),
            ParentAccountId = cm_util:parent_account_id(Account),
            Result = maybe_suitable_servers(JObj, AaaDoc, AAAProtocol, ParentAccountId),
            {'ok', Result};
        <<"inherit">> ->
            % AAA functionality is in the 'inherit' mode for this account
            lager:debug("AAA functionality enabled (inherit mode) for the ~p account", [AccountId]),
            ParentAccountId = cm_util:parent_account_id(Account),
            Result = maybe_suitable_servers(JObj, AaaDoc, AAAProtocol, ParentAccountId),
            {'ok', Result}
    end.

-spec maybe_suitable_servers(wh_json:object(), proplist(), ne_binary(), api_binary()) -> {'ok', 'aaa_mode_off'} |
                                                                                         {'ok', tuple()} |
                                                                                         {'error', 'no_respond'}.
maybe_suitable_servers(JObj, AaaDoc, AAAProtocol, ParentAccountId) ->
    Servers = [S || S <- wh_json:get_value(<<"servers">>, AaaDoc),
        wh_json:get_value(<<"enabled">>, S) == 'true', wh_json:get_value(<<"aaa_engine">>, S) == AAAProtocol],
    lager:debug("AAAProtocol=~p~nServersBefore=~p~nServersAfter=~p~n",
        [AAAProtocol, wh_json:get_value(<<"servers">>, AaaDoc), Servers]),
    maybe_server_request(Servers, JObj, AaaDoc, ParentAccountId).

-spec maybe_server_request(list(), wh_json:object(), proplist(), ne_binary()) -> {'ok', 'aaa_mode_off'} |
                                                                                 {'ok', tuple()} |
                                                                                 {'error', 'no_respond'}.
maybe_server_request([], JObj, AaaDoc, ParentAccountId) ->
    case wh_json:get_value(<<"aaa_mode">>, AaaDoc) of
        <<"inherit">> ->
            % we are in the "inherit" mode so we should get parent account and use its AAA settings
            lager:debug("the 'inherit' mode used - switching to parent account"),
            maybe_aaa_mode(JObj, ParentAccountId);
        _ ->
            lager:debug("no 'inherit' mode - servers search stopped"),
            {'error', 'no_respond'}
    end;
maybe_server_request([Server | Servers], JObj, AaaProps, ParentAccountId) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Address = cm_util:network_address_to_ip_tuple(props:get_value(<<"address">>, Server)),
    Port = props:get_value(<<"port">>, Server),
    Secret = props:get_value(<<"secret">>, Server),
    lager:debug("trying to resolve the next AVPs: ~p", wh_json:to_proplist(JObj)),
    ParamList = [{eradius_dict:lookup_by_name(AccountId, 'attribute2', Key), Value} ||
                    {Key, Value} <- wh_json:to_proplist(JObj)],
    Request = eradius_lib:set_attributes(#radius_request{cmd = request}, ParamList),
    lager:debug("Address=~p~nPort=~p~nSecret=~p~nRequest=~p~n", [Address, Port, Secret, Request]),
    case eradius_client:send_request({Address, Port, Secret}, Request) of
        {'ok', Response, _Authenticator} ->
            Result = eradius_lib:decode_request(Response, Secret),
            lager:debug("response received: ~p", [Result]),
            {'ok', Result};
        Error ->
            lager:debug("error response received: ~p", [Error]),
            maybe_server_request(Servers, JObj, AaaProps, ParentAccountId)
    end.
