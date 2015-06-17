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

init(_) ->
    {'ok', []}.

handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

handle_cast({'authn_req', SenderPid, JObj}, State) ->
    AccountId = wh_json:get_value(<<"Account-Id">>, JObj, <<"system_config">>),
    lager:debug("JObj=~p~nAccountId=~p~n", [JObj, AccountId]),
    case maybe_aaa_mode(JObj, AccountId) of
        {'ok', Response} ->
            gen_server:cast(SenderPid, {'response', Response, self()});
        {'error', Reason} ->
            gen_server:cast(SenderPid, {'error', Reason, self()})
    end,
    {'noreply', State};
handle_cast(_Message, State) ->
    lager:debug("Message=~p~n"),
    {'noreply', State}.

handle_info(_Info, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

terminate(_Reason, _State) ->
    [].

code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

maybe_aaa_mode(_JObj, 'undefined') ->
    {'error', 'no_respond'};
maybe_aaa_mode(JObj, <<"system_config">> = AccountId) ->
    lager:debug("AccountId=~p~n", [AccountId]),
    % used system_config-related AAA settings
    AAAProtocol = wh_json:get_value(<<"AAA-Protocol">>, JObj),
    % add system_config as source account
    JObj1 = wh_json:set_value(<<"Account-Id">>, AccountId, JObj),
    {ok, AaaDoc} = couch_mgr:open_doc(?WH_CONFIG_DB, <<"circlemaker">>),
    case wh_json:get_value(<<"aaa_mode">>, AaaDoc) of
        <<"off">> ->
            % AAA functionality is off for this account
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            % 'on' mode
            Result = maybe_suitable_servers(JObj1, AaaDoc, AAAProtocol, 'undefined'),
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
            % AAA functionality is off for this account
            {'ok', 'aaa_mode_off'};
        <<"on">> ->
            % AAA functionality is on for this account
            ParentAccountId = cm_util:parent_account_id(Account),
            Result = maybe_suitable_servers(JObj, AaaDoc, AAAProtocol, ParentAccountId),
            {'ok', Result};
        <<"inherit">> ->
            % AAA functionality is in the 'inherit' mode for this account
            ParentAccountId = cm_util:parent_account_id(Account),
            Result = maybe_suitable_servers(JObj, AaaDoc, AAAProtocol, ParentAccountId),
            {'ok', Result}
    end.

maybe_suitable_servers(JObj, AaaDoc, AAAProtocol, ParentAccountId) ->
    Servers = [S || S <- wh_json:get_value(<<"servers">>, AaaDoc),
        wh_json:get_value(<<"enabled">>, S) == 'true', wh_json:get_value(<<"aaa_engine">>, S) == AAAProtocol],
    lager:debug("AAAProtocol=~p~nServersBefore=~p~nServersAfter=~p~n",
        [AAAProtocol, wh_json:get_value(<<"servers">>, AaaDoc), Servers]),
    maybe_server_request(Servers, JObj, AaaDoc, ParentAccountId).

maybe_server_request([], JObj, AaaDoc, ParentAccountId) ->
    case wh_json:get_value(<<"aaa_mode">>, AaaDoc) of
        <<"inherit">> ->
            % we are in the "inherit" mode so we should get parent account and use its AAA settings
            maybe_aaa_mode(JObj, ParentAccountId);
        <<"on">> ->
            % 'on' mode
            {'error', 'no_respond'}
    end;
maybe_server_request([Server | Servers], JObj, AaaDoc, ParentAccountId) ->
    AccountId = wh_json:get_value(<<"Account-Id">>, JObj),
    Address = cm_util:binary_ip_to_tuple(wh_json:get_value(<<"address">>, Server)),
    Port = wh_json:get_value(<<"port">>, Server),
    Secret = wh_json:get_value(<<"secret">>, Server),
    ParamList = [{eradius_dict:lookup_by_name(AccountId, 'attribute2', Key), Value} ||
                    {Key, Value} <- wh_json:to_proplist(JObj)],
    Request = eradius_lib:set_attributes(#radius_request{cmd = request}, ParamList),
    lager:debug("Address=~p~nPort=~p~nSecret=~p~nRequest=~p~n", [Address, Port, Secret, Request]),
    case eradius_client:send_request({Address, Port, Secret}, Request) of
        {'ok', Result} ->
            Response = eradius_lib:decode_request(Result, Secret),
            {'ok', Response};
        _Error ->
            maybe_server_request(Servers, JObj, AaaDoc, ParentAccountId)
    end.
