%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_maintenance).

-export([migrate/0]).
-export([flush/0]).
-export([start_module/1]).
-export([stop_module/1]).
-export([running_modules/0]).
-export([refresh/0, refresh/1]).
-export([find_account_by_number/1]).
-export([find_account_by_name/1]).
-export([find_account_by_realm/1]).
-export([enable_account/1, disable_account/1]).
-export([promote_account/1, demote_account/1]).
-export([allow_account_number_additions/1, disallow_account_number_additions/1]).
-export([create_account/4]).

-include_lib("crossbar/include/crossbar.hrl").

-type input_term() :: atom() | string() | ne_binary().


%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec migrate/0 :: () -> 'no_return'.
migrate() ->
    io:format("updating default crossbar modules~n", []),
    whapps_config:flush(),
    StartModules = sets:from_list(whapps_config:get(<<"crossbar">>, <<"autoload_modules">>, [])),
    XbarUpdates = [fun(L) -> sets:del_element(<<"cb_cdr">>, L) end
                   ,fun(L) -> sets:del_element(<<"cb_signups">>, L) end
                   ,fun(L) -> sets:del_element(<<"cb_resources">>, L) end
                   ,fun(L) -> sets:del_element(<<"cb_provisioner_templates">>, L) end
                   ,fun(L) -> sets:del_element(<<"cb_ts_accounts">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_phone_numbers">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_templates">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_onboard">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_connectivity">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_local_provisioner_templates">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_global_provisioner_templates">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_queues">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_schemas">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_configs">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_limits">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_whitelabel">>, L) end
                   ,fun(L) -> sets:add_element(<<"cb_braintree">>, L) end
                  ],
    UpdatedModules = sets:to_list(lists:foldr(fun(F, L) -> F(L) end, StartModules, XbarUpdates)),
    _ = whapps_config:set_default(<<"crossbar">>, <<"autoload_modules">>, UpdatedModules),
    case whapps_controller:stop_app(crossbar) of 
        ok -> whapps_controller:start_app(crossbar);
        _Else -> ok
    end,
    no_return.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the crossbar local cache
%% @end
%%--------------------------------------------------------------------
-spec flush/0 :: () -> 'ok'.
flush() ->
    wh_cache:flush_local(?CROSSBAR_CACHE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'ok'.
-spec refresh/1 :: (input_term()) -> 'ok'.

refresh() ->
    io:format("please use whapps_maintenance:refresh().", []).

refresh(Value) ->
    io:format("please use whapps_maintenance:refresh(~p).", [Value]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_module/1 :: (text()) -> 'ok' | {'error', _}.
start_module(Module) ->
    crossbar:start_mod(Module).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_module/1 :: (text()) -> 'ok' | {'error', _}.
stop_module(Module) ->
    crossbar:stop_mod(Module).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec running_modules/0 :: () -> [atom(),...] | [].
running_modules() ->
    crossbar_bindings:modules_loaded().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_number/1 :: (input_term()) -> {'ok', ne_binary()} | {'error', term()}.
find_account_by_number(Number) when not is_binary(Number) ->
    find_account_by_number(wh_util:to_binary(Number));
find_account_by_number(Number) ->
    case wh_number_manager:lookup_account_by_number(Number) of
        {ok, AccountId, _, _} -> 
            AccountDb = wh_util:format_account_id(AccountId, encoded),
            print_account_info(AccountDb, AccountId);
        {error, {not_in_service, AssignedTo}} ->
            AccountDb = wh_util:format_account_id(AssignedTo, encoded),
            print_account_info(AccountDb, AssignedTo);
        {error, {account_disabled, AssignedTo}} ->
            AccountDb = wh_util:format_account_id(AssignedTo, encoded),
            print_account_info(AccountDb, AssignedTo);
        {error, Reason}=E ->
            io:format("failed to find account assigned to number '~s': ~p~n", [Number, Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_name/1 :: (input_term()) -> {'ok', ne_binary()} | 
                                                  {'multiples', [ne_binary(),...]} |
                                                  {'error', term()}.
find_account_by_name(Name) when not is_binary(Name) ->
    find_account_by_name(wh_util:to_binary(Name)); 
find_account_by_name(Name) ->
    case whapps_util:get_accounts_by_name(Name) of
        {ok, AccountDb} ->
            print_account_info(AccountDb);
        {multiples, AccountDbs} ->
            AccountIds = [begin
                              {ok, AccountId} = print_account_info(AccountDb),
                              AccountId
                          end || AccountDb <- AccountDbs
                         ],
            {multiples, AccountIds};
        {error, Reason}=E ->
            io:format("failed to find account: ~p~n", [Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_realm/1 :: (input_term()) -> {'ok', ne_binary()} | 
                                                  {'multiples', [ne_binary(),...]} |
                                                  {'error', term()}.
find_account_by_realm(Realm) when not is_binary(Realm) ->
    find_account_by_realm(wh_util:to_binary(Realm));
find_account_by_realm(Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {ok, AccountDb} ->
            print_account_info(AccountDb);
        {multiples, AccountDbs} ->
            AccountIds = [begin
                              {ok, AccountId} = print_account_info(AccountDb),
                              AccountId
                          end || AccountDb <- AccountDbs
                         ],
            {multiples, AccountIds};
        {error, Reason}=E ->
            io:format("failed to find account: ~p~n", [Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec allow_account_number_additions/1 :: (input_term()) -> 'ok' | 'failed'.
allow_account_number_additions(AccountId) ->
    case update_account(AccountId, <<"pvt_wnm_allow_additions">>, true) of
        {ok, _} ->
            io:format("allowing account '~s' to added numbers~n", [AccountId]),
            ok;
        {error, Reason} ->
            io:format("failed to find account: ~p~n", [Reason]),
            failed
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec disallow_account_number_additions/1 :: (input_term()) -> 'ok' | 'failed'.
disallow_account_number_additions(AccountId) ->
    case update_account(AccountId, <<"pvt_wnm_allow_additions">>, false) of
        {ok, _} ->
            io:format("disallowed account '~s' to added numbers~n", [AccountId]),
            ok;
        {error, Reason} ->
            io:format("failed to find account: ~p~n", [Reason]),
            failed
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec enable_account/1 :: (input_term()) -> 'ok' | 'failed'.
enable_account(AccountId) ->
    case update_account(AccountId, <<"pvt_enabled">>, true) of
        {ok, _} ->
            io:format("enabled account '~s'~n", [AccountId]),
            ok;
        {error, Reason} ->
            io:format("failed to enable account: ~p~n", [Reason]),
            failed
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec disable_account/1 :: (input_term()) -> 'ok' | 'failed'.
disable_account(AccountId) ->
    case update_account(AccountId, <<"pvt_enabled">>, false) of
        {ok, _} ->
            io:format("disabled account '~s'~n", [AccountId]),
            ok;
        {error, Reason} ->
            io:format("failed to disable account: ~p~n", [Reason]),
            failed
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec promote_account/1 :: (input_term()) -> 'ok' | 'failed'.
promote_account(AccountId) ->
    case update_account(AccountId, <<"pvt_superduper_admin">>, true) of
        {ok, _} ->
            io:format("promoted account '~s', this account now has permission to change system settings~n", [AccountId]),
            ok;
        {error, Reason} ->
            io:format("failed to promote account: ~p~n", [Reason]),
            failed
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec demote_account/1 :: (input_term()) -> 'ok' | 'failed'.
demote_account(AccountId) ->
    case update_account(AccountId, <<"pvt_superduper_admin">>, false) of
        {ok, _} ->
            io:format("promoted account '~s', this account can no longer change system settings~n", [AccountId]),
            ok;
        {error, Reason} ->
            io:format("failed to demote account: ~p~n", [Reason]),
            failed
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec create_account/4 :: (input_term(), input_term(), input_term(), input_term()) -> 'ok' | 'failed'.
create_account(AccountName, Realm, Username, Password) when not is_binary(AccountName) ->
    create_account(wh_util:to_binary(AccountName), Realm, Username, Password);
create_account(AccountName, Realm, Username, Password) when not is_binary(Realm) ->
    create_account(AccountName, wh_util:to_binary(Realm), Username, Password);
create_account(AccountName, Realm, Username, Password) when not is_binary(Username) ->
    create_account(AccountName, Realm, wh_util:to_binary(Username), Password);
create_account(AccountName, Realm, Username, Password) when not is_binary(Password) ->
    create_account(AccountName, Realm, Username, wh_util:to_binary(Password));
create_account(AccountName, Realm, Username, Password) ->
    Account = wh_json:from_list([{<<"_id">>, couch_mgr:get_uuid()}
                                 ,{<<"name">>, AccountName}
                                 ,{<<"realm">>, Realm}
                                ]),
    User = wh_json:from_list([{<<"_id">>, couch_mgr:get_uuid()}
                              ,{<<"username">>, Username}
                              ,{<<"password">>, Password}
                              ,{<<"first_name">>, <<"Account">>}
                              ,{<<"last_name">>, <<"Admin">>}
                              ,{<<"priv_level">>, <<"admin">>}
                             ]),
    try
        {ok, C1} = validate_account(Account, #cb_context{}),
        {ok, C2} = validate_user(User, C1),
        {ok, #cb_context{db_name=Db, account_id=AccountId}} = create_account(C1),
        {ok, _} = create_user(C2#cb_context{db_name=Db, account_id=AccountId}),
        case whapps_util:get_all_accounts() of
            [Db] -> promote_account(AccountId);
            _Else -> ok
        end,
        ok
    catch
        _:_ -> 
            failed
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_account/2 :: (wh_json:json_object(), #cb_context{}) -> {'ok', #cb_context{}} |
                                                                      {'error', wh_json:json_object()}.
validate_account(JObj, Context) ->
    Payload = [Context#cb_context{req_data=JObj
                                  ,req_nouns=[{?WH_ACCOUNTS_DB, []}]
                                  ,req_verb = <<"put">>
                                 }               
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.accounts">>, Payload) of
        #cb_context{resp_status=success}=Context1 -> {ok, Context1};
        #cb_context{resp_data=Errors} -> 
            io:format("failed to validate account properties: '~s'~n", [wh_json:encode(Errors)]),
            {error, Errors}
    end.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec validate_user/2 :: (wh_json:json_object(), #cb_context{}) -> {'ok', #cb_context{}} |
                                                                   {'error', wh_json:json_object()}.    
validate_user(JObj, Context) ->
    Payload = [Context#cb_context{req_data=JObj
                                  ,req_nouns=[{?WH_ACCOUNTS_DB, []}]
                                  ,req_verb = <<"put">>
                                 }               
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.users">>, Payload) of
        #cb_context{resp_status=success}=Context1 -> {ok, Context1};
        #cb_context{resp_data=Errors} -> 
            io:format("failed to validate user properties: '~s'~n", [wh_json:encode(Errors)]),
            {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec create_account/1 :: (#cb_context{}) -> {'ok', #cb_context{}} |
                                             {'error', wh_json:json_object()}.
create_account(Context) ->
    case crossbar_bindings:fold(<<"v1_resource.execute.put.accounts">>, [Context]) of
        #cb_context{resp_status=success, db_name=AccountDb, account_id=AccountId, doc=Doc}=Context1 ->
            io:format("created new account '~s' in db '~s'~n", [AccountId, AccountDb]),
            {ok, Context1};
        #cb_context{resp_data=Errors} ->
            io:format("failed to create account: '~s'~n", [list_to_binary(wh_json:encode(Errors))]),
            AccountId = wh_json:get_value(<<"_id">>, Context#cb_context.req_data),
            couch_mgr:db_delete(wh_util:format_account_id(AccountId, encoded)),
            {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec create_user/1 :: (#cb_context{}) -> {'ok', #cb_context{}} |
                                             {'error', wh_json:json_object()}.
create_user(Context) ->
    case crossbar_bindings:fold(<<"v1_resource.execute.put.users">>, [Context]) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            io:format("created new account admin user '~s'~n", [wh_json:get_value(<<"_id">>, JObj)]),
            {ok, Context1};
        #cb_context{resp_data=Errors} ->
            io:format("failed to create account admin user: '~s'~n", [list_to_binary(wh_json:encode(Errors))]),
            {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update_account/3 :: (input_term(), ne_binary(), term()) -> {'ok', wh_json:json_object()} |
                                                                 {'error', term()}.
update_account(AccountId, Key, Value) when not is_binary(AccountId) ->
    update_account(wh_util:to_binary(AccountId), Key, Value);
update_account(AccountId, Key, Value) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    Updaters = [fun({error, _}=E) -> E;
                    ({ok, J}) -> couch_mgr:ensure_saved(AccountDb, wh_json:delete_key(<<"_rev">>, J))
                 end
                 ,fun({error, _}=E) -> E;
                     ({ok, J}) -> couch_mgr:save_doc(AccountDb, wh_json:set_value(Key, Value, J))
                  end
                ],
    lists:foldr(fun(F, J) -> F(J) end, couch_mgr:open_doc(AccountDb, AccountId), Updaters).

print_account_info(AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, raw),
    print_account_info(AccountDb, AccountId).    
print_account_info(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {ok, JObj} ->
            io:format("Account ID: ~s (~s)~n", [AccountId, AccountDb]),
            io:format("  Name: ~s~n", [wh_json:get_value(<<"name">>, JObj)]),
            io:format("  Realm: ~s~n", [wh_json:get_value(<<"realm">>, JObj)]),
            io:format("  Enabled: ~s~n", [not wh_json:is_false(<<"pvt_enabled">>, JObj)]),
            io:format("  System Admin: ~s~n", [wh_json:is_true(<<"pvt_superduper_admin">>, JObj)]);
        {error, _} ->
            io:format("Account ID: ~s (~s)~n", [AccountId, AccountDb])
    end,
    {ok, AccountId}.
