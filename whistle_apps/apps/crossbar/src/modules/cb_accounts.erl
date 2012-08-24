%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_accounts).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).
-export([create_account/1
         ,get_realm_from_db/1
         ,ensure_parent_set/0
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".accounts">>).

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(AGG_VIEW_PARENT, <<"accounts/listing_by_parent">>).
-define(AGG_VIEW_CHILDREN, <<"accounts/listing_by_children">>).
-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).
-define(AGG_VIEW_REALM, <<"accounts/listing_by_realm">>).

-define(PVT_TYPE, <<"account">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_realm_from_db/1 :: (ne_binary()) -> {'ok', ne_binary()} | {'error', atom()}.
get_realm_from_db(DBName) ->
    Doc = wh_util:format_account_id(DBName, raw),
    case couch_mgr:open_doc(DBName, Doc) of
        {ok, JObj} -> {ok, wh_json:get_value(<<"realm">>, JObj)};
        {error, _}=E -> E
    end.

%% Iterate through all account docs in the accounts DB and ensure each
%% has a parent
-spec ensure_parent_set/0 :: () -> 'ok' | {'error', 'no_accounts' | atom()}.
ensure_parent_set() ->
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_REALM, [{<<"include_docs">>, true}]) of
        {error, _}=E -> E;
        {ok, []} -> {error, no_accounts};
        {ok, AcctJObjs} ->
            case whapps_util:get_master_account_id() of
                {error, _}=E -> E;
                {ok, MasterAccountId} -> 
                    _ = [ensure_parent_set(MasterAccountId, wh_json:get_value(<<"doc">>, AcctJObj))
                         || AcctJObj <- AcctJObjs,
                            wh_json:get_value(<<"id">>, AcctJObj) =/= MasterAccountId, % not the default parent                            
                            (Tree = wh_json:get_value([<<"doc">>, <<"pvt_tree">>], AcctJObj)) =:= [] orelse % empty tree (should have at least the parent)
                                Tree =:= <<>> orelse % Tree is an empty string only
                                Tree =:= [""] orelse % Tree is bound in the prior bit, and might be a list of an empty string
                                Tree =:= [<<>>] orelse % Tree is a list of an empty string
                                Tree =:= undefined % if the pvt_tree key doesn't exist
                        ],
                    ok
            end
    end.

-spec ensure_parent_set/2 :: (ne_binary(), wh_json:json_object()) -> 'ok' | #cb_context{}.
ensure_parent_set(DefaultParentID, JObj) ->
    ParentTree = [DefaultParentID, wh_json:get_value(<<"_id">>, JObj)],
    lager:debug("pvt_tree before: ~p after: ~p", [wh_json:get_value(<<"pvt_tree">>, JObj), ParentTree]),
    [JObj1] = update_doc_tree(ParentTree
                              ,wh_json:set_values([{<<"id">>, wh_json:get_value(<<"_id">>, JObj)}
                                                   ,{<<"pvt_tree">>, []}
                                                  ], JObj)
                              ,[]),
    case couch_mgr:save_doc(?WH_ACCOUNTS_DB, JObj1) of
        {ok, _} ->
            lager:debug("saved ~s with updated tree", [wh_json:get_value(<<"_id">>, JObj)]);
        {error, _E} ->
            lager:debug("failed to save ~s with updated tree: ~p", [wh_json:get_value(<<"_id">>, JObj), _E])
    end.

%% Bindings callbacks
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.accounts">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.accounts">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.accounts">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.accounts">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), ne_binary()) -> http_methods().
allowed_methods() ->
    ['PUT'].
allowed_methods(_) ->
    ['GET', 'PUT', 'POST', 'DELETE'].
allowed_methods(_, Path) ->
    case lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]) of
        true -> ['GET'];
        false -> []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_tokens()) -> 'true'.
-spec resource_exists/2 :: (path_tokens(), ne_binary()) -> boolean().
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, Path) ->
    lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), ne_binary()) -> #cb_context{}.
validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}]}=Context) ->
    validate_req(Context#cb_context{db_name=?WH_ACCOUNTS_DB}).

validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}]}=Context, Id) ->
    validate_req(Context#cb_context{db_name=?WH_ACCOUNTS_DB}, Id);
validate(Context, Id) ->
    load_account_db(Id, Context).

validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}]}=Context, Id, Relationship) ->
    validate_req(Context#cb_context{db_name=?WH_ACCOUNTS_DB}, Id, Relationship);
validate(Context, Id, Relationship) ->
    validate_req(Context, Id, Relationship).

-spec validate_req/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate_req/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate_req/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate_req(#cb_context{req_verb = <<"put">>}=Context) ->
    create_account(Context).

validate_req(#cb_context{req_verb = <<"put">>}=Context, ParentId) ->
    create_account(Context, ParentId);
validate_req(#cb_context{req_verb = <<"get">>}=Context, AccountId) ->
    load_account(AccountId, Context);
validate_req(#cb_context{req_verb = <<"post">>}=Context, AccountId) ->
    update_account(AccountId, Context);
validate_req(#cb_context{req_verb = <<"delete">>}=Context, AccountId) ->
    load_account(AccountId, Context).

validate_req(#cb_context{req_verb = <<"get">>}=Context, AccountId, <<"children">>) ->
    load_children(AccountId, Context);
validate_req(#cb_context{req_verb = <<"get">>}=Context, AccountId, <<"descendants">>) ->
    load_descendants(AccountId, Context);
validate_req(#cb_context{req_verb = <<"get">>}=Context, AccountId, <<"siblings">>) ->
    load_siblings(AccountId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{doc=Doc}=Context, AccountId) ->
    _ = crossbar_util:put_reqid(Context),
    %% this just got messy
    %% since we are not replicating, the accounts rev and the account rev on
    %% this doc can drift.... so set it to account save, then set it to
    %% accounts for the final operation... good times
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    AccountsRev = wh_json:get_value(<<"_rev">>, Doc, <<>>),
    case couch_mgr:lookup_doc_rev(AccountDb, AccountId) of
        {ok, Rev} ->
            case crossbar_doc:save(Context#cb_context{db_name=AccountDb
                                                      ,doc=wh_json:set_value(<<"_rev">>, Rev, Doc)
                                                     }) of
                #cb_context{resp_status=success, doc=Doc1}=Context1 ->
                    _ = couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, AccountsRev, Doc1)),
                    support_depreciated_billing_id(wh_json:get_value(<<"billing_id">>, Doc1), AccountId, Context1);
                Else ->
                    Else
            end;
        _ ->
            case crossbar_doc:save(Context#cb_context{db_name=AccountDb
                                                      ,doc=wh_json:delete_key(<<"_rev">>, Doc)
                                                     }) of
                #cb_context{resp_status=success, doc=Doc1}=Context1 ->
                    couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, AccountsRev, Doc1)),
                    support_depreciated_billing_id(wh_json:get_value(<<"billing_id">>, Doc1), AccountId, Context1);
                Else ->
                    Else
            end
    end.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
put(Context) ->
    create_new_account_db(Context).
put(Context, _) ->
    create_new_account_db(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, AccountId) ->
    _ = crossbar_util:put_reqid(Context),
    %% dont use the account id in cb_context as it may not represent the db_name...
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    delete_stop_if_decedants(AccountId, AccountDb, Context).

delete_stop_if_decedants(AccountId, AccountDb, Context) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_DESCENDANTS, ViewOptions) of
        {error, _} -> crossbar_util:response(error, <<"unable to count descendants">>, 500, Context);
        {ok, JObjs} -> 
            case [JObj || JObj <- JObjs, wh_json:get_value(<<"id">>, JObj) =/= AccountId] of
                [] -> delete_free_numbers(AccountId, AccountDb, Context);
                _Else ->
                    crossbar_util:response(error, <<"account has descendants">>, 400, Context)
            end
    end.

delete_free_numbers(AccountId, AccountDb, Context) ->
    case wh_number_manager:free_numbers(AccountId) of
        ok -> delete_remove_services(AccountId, AccountDb, Context);
        _ -> crossbar_util:response(error, <<"unable to free numbers">>, 500, Context)
    end.

delete_remove_services(AccountId, AccountDb, Context) ->
    case wh_services:delete(AccountId) of
        {ok, _} -> delete_remove_db(AccountId, AccountDb, Context);
        _ -> crossbar_util:response(error, <<"unable to cancel services">>, 500, Context)
    end.

delete_remove_db(AccountId, AccountDb, Context) ->
   Removed = case couch_mgr:open_doc(AccountDb, AccountId) of
                 {ok, _} -> couch_mgr:db_delete(AccountDb);
                 {error, not_found} -> true;
                 {error, _R} ->
                     lager:debug("failed to open account defintion ~s: ~p", [AccountId, _R]),
                     false
             end,    
    case Removed of
        true ->
            lager:debug("deleted db ~s", [AccountDb]),
            delete_remove_from_accounts(AccountId, AccountDb, Context);
        false ->
            lager:debug("failed to remove database ~s", [AccountDb]),
            crossbar_util:response(error, <<"unable to remove database">>, 500, Context)
    end.

delete_remove_from_accounts(AccountId, _, Context) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
        {ok, JObj} ->
            crossbar_doc:delete(Context#cb_context{db_name=?WH_ACCOUNTS_DB
                                                   ,doc=JObj
                                                  });
        {error, not_found} -> crossbar_util:response(wh_json:new(), Context);
        {error, _R} ->
            crossbar_util:response(error, <<"unable to remove account definition">>, 500, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new account document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_account/1 :: (#cb_context{}) -> #cb_context{}.
-spec create_account/2 :: (#cb_context{}, 'undefined' | ne_binary()) -> #cb_context{}.
create_account(Context) ->
    case whapps_util:get_master_account_id() of
        {ok, MasterAccountId} -> create_account(Context, MasterAccountId);
        {error, _R} ->
            lager:debug("unabled to determine the system admin account: ~p", [_R]),
            create_account(Context, undefined)
    end.

create_account(#cb_context{req_data=ReqData}=Context, ParentId) ->
    Data = case wh_json:get_ne_value(<<"realm">>, ReqData) of
               undefined ->
                   RealmSuffix = whapps_config:get_binary(?ACCOUNTS_CONFIG_CAT, <<"account_realm_suffix">>, <<"sip.2600hz.com">>),
                   Strength = whapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"random_realm_strength">>, 3),
                   wh_json:set_value(<<"realm">>, list_to_binary([wh_util:rand_hex_binary(Strength), ".", RealmSuffix]), ReqData);
               _Else ->
                   ReqData
           end,
    UniqueRealm = is_unique_realm(undefined, Context#cb_context{req_data=Data}),
    case wh_json_validator:is_valid(Data, ?WH_ACCOUNTS_DB) of
        {fail, Errors} when UniqueRealm ->
            crossbar_util:response_invalid_data(Errors, Context);
        {fail, Errors} ->
            E = wh_json:set_value([<<"realm">>, <<"unique">>], <<"Realm is not unique for this system">>, Errors),
            crossbar_util:response_invalid_data(E, Context);
        {pass, _} when not UniqueRealm ->
            E = wh_json:set_value([<<"realm">>, <<"unique">>], <<"Realm is not unique for this system">>, wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            Context#cb_context{
              doc=set_private_fields(JObj, Context, ParentId)
              ,resp_status=success
             }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_account/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_account(AccountId, Context) ->
    #cb_context{resp_data=RespData, doc=Doc}=Context1=crossbar_doc:load(AccountId, Context),
    PvtIncludes=[<<"pvt_wnm_allow_additions">>, <<"pvt_superduper_admin">>],
    RespData1 = lists:foldl(fun(<<"pvt_", Key/binary>> = Pvt, Data) ->
                                    case wh_json:get_ne_value(Pvt, Doc) of
                                        undefined ->
                                            wh_json:delete_key(Key, Data);
                                        Value ->
                                            wh_json:set_value(Key, Value, Data)
                                    end;
                               (_NotPvt, Data) ->
                                    Data
                            end
                            ,RespData
                            ,PvtIncludes
                           ),
    Context1#cb_context{resp_data=RespData1}.
     

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing account document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_account/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_account(AccountId, #cb_context{req_data=Data}=Context) ->
    UniqueRealm = is_unique_realm(AccountId, Context),
    case wh_json_validator:is_valid(Data, ?WH_ACCOUNTS_DB) of
        {fail, Errors} when UniqueRealm ->
            crossbar_util:response_invalid_data(Errors, Context);
        {fail, Errors} ->
            E = wh_json:set_value([<<"realm">>, <<"unique">>], <<"Realm is not unique for this system">>, Errors),
            crossbar_util:response_invalid_data(E, Context);
        {pass, _} when not UniqueRealm ->
            E = wh_json:set_value([<<"realm">>, <<"unique">>], <<"Realm is not unique for this system">>, wh_json:new()),
            crossbar_util:response_invalid_data(E, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(AccountId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the children of this account
%% @end
%%--------------------------------------------------------------------
-spec load_children/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_children(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_CHILDREN, [{<<"startkey">>, [AccountId]}
                                                ,{<<"endkey">>, [AccountId, wh_json:new()]}
                                               ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the descendants of this account
%% @end
%%--------------------------------------------------------------------
-spec load_descendants/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_descendants(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS, [{<<"startkey">>, [AccountId]}
                                                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                                                  ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the siblngs of this account
%% @end
%%--------------------------------------------------------------------
-spec load_siblings/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_siblings(AccountId, Context) ->
    case crossbar_doc:load_view(?AGG_VIEW_PARENT, [{<<"startkey">>, AccountId}
                                                   ,{<<"endkey">>, AccountId}
                                                  ], Context) of
        #cb_context{resp_status=success, doc=[JObj|_]} ->
            Parent = wh_json:get_value([<<"value">>, <<"id">>], JObj),
            load_children(Parent, Context);
        _Else ->
            crossbar_util:response_bad_identifier(AccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc_tree/3 :: ([ne_binary(),...], wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
update_doc_tree([_|_]=ParentTree, JObj, Acc) ->
    AccountId = wh_json:get_value(<<"id">>, JObj),
    PvtTree = wh_json:get_value(<<"pvt_tree">>, JObj),

    {Tree, MyDoc} = case is_list(PvtTree) of
                        true -> {PvtTree, JObj};
                        false ->
                            #cb_context{resp_status=success, doc=Doc} = crossbar_doc:load(AccountId, #cb_context{db_name=?WH_ACCOUNTS_DB}),
                            {wh_json:get_value(<<"pvt_tree">>, Doc, []), Doc}
                    end,

    ParentId = lists:last(ParentTree),
    MyTree = case lists:dropwhile(fun(E)-> E =/= ParentId end, Tree) of
                 [] -> ParentTree;
                 [_|List] -> ParentTree ++ List
             end,
    Trimmed = [E || E <- MyTree, E =/= AccountId],

    [wh_json:set_value(<<"pvt_tree">>, Trimmed, MyDoc) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the private fields to be added to a new account
%% document
%% @end
%%--------------------------------------------------------------------
-spec set_private_fields/3 :: (wh_json:json_object(), #cb_context{}, 'undefined' | ne_binary()) -> wh_json:json_object().
set_private_fields(JObj0, Context, undefined) ->
    lists:foldl(fun(Fun, JObj1) ->
                        Fun(JObj1, Context)
                end, JObj0, [fun add_pvt_type/2
                             ,fun add_pvt_api_key/2
                             ,fun add_pvt_tree/2
                            ]);
set_private_fields(JObj0, Context, ParentId) ->
    case is_binary(ParentId) andalso couch_mgr:open_doc(wh_util:format_account_id(ParentId, encoded), ParentId) of
        {ok, ParentJObj} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, ParentJObj, []) ++ [ParentId],
            Enabled = wh_json:is_false(<<"pvt_enabled">>, ParentJObj) =/= true,
            AddPvtTree = fun(JObj, _) -> wh_json:set_value(<<"pvt_tree">>, Tree, JObj) end,
            AddPvtEnabled = fun(JObj, _) -> wh_json:set_value(<<"pvt_enabled">>, Enabled, JObj) end,
            lists:foldl(fun(Fun, JObj1) ->
                                Fun(JObj1, Context)
                        end, JObj0, [fun add_pvt_type/2
                                     ,fun add_pvt_api_key/2
                                     ,AddPvtTree
                                     ,AddPvtEnabled
                                    ]);
        false ->
            set_private_fields(JObj0, Context, undefined);
        _ ->
            set_private_fields(JObj0, Context, undefined)
    end.

add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).

add_pvt_api_key(JObj, _) ->
    wh_json:set_value(<<"pvt_api_key">>, wh_util:to_hex_binary(crypto:rand_bytes(32)), JObj).

add_pvt_tree(JObj, #cb_context{auth_doc=undefined}) ->
    case whapps_config:get(?CONFIG_CAT, <<"default_parent">>) of
        undefined ->
            lager:debug("there really should be a parent unless this is the first ever account"),
            wh_json:set_value(<<"pvt_tree">>, [], JObj);
        ParentId ->
            lager:debug("setting tree to [~s]", [ParentId]),
            wh_json:set_value(<<"pvt_tree">>, [ParentId], JObj)
    end;
add_pvt_tree(JObj, #cb_context{auth_doc=Token}) ->
    AuthAccId = wh_json:get_value(<<"account_id">>, Token),
    case is_binary(AuthAccId) andalso couch_mgr:open_doc(wh_util:format_account_id(AuthAccId, encoded), AuthAccId) of
        {ok, AuthJObj} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, AuthJObj, []) ++ [AuthAccId],
            Enabled = wh_json:is_false(<<"pvt_enabled">>, AuthJObj) =/= true,
            lager:debug("setting parent tree to ~p", [Tree]),
            lager:debug("setting initial pvt_enabled to ~s", [Enabled]),
            wh_json:set_value(<<"pvt_tree">>, Tree
                              ,wh_json:set_value(<<"pvt_enabled">>, Enabled, JObj));
        false ->
            add_pvt_tree(JObj, #cb_context{auth_doc=undefined});
        _ ->
            lager:debug("setting parent tree to [~s]", [AuthAccId]),
            wh_json:set_value(<<"pvt_tree">>, [AuthAccId]
                              ,wh_json:set_value(<<"pvt_enabled">>, false, JObj))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_account_db/2 :: (ne_binary() | [ne_binary(),...], #cb_context{}) -> #cb_context{}.
load_account_db([AccountId|_], Context) ->
    load_account_db(AccountId, Context);
load_account_db(AccountId, Context) when is_binary(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    lager:debug("account determined that db name: ~s", [AccountDb]),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, _} ->
            lager:debug("account ~s db exists , setting operating databse as ~s", [AccountId, AccountDb]),
            Context#cb_context{
              resp_status = success
              ,db_name = AccountDb
              ,account_id = AccountId
             };
        {error, _R} ->
            lager:debug("unable to open account definition ~s/~s: ~p", [AccountDb, AccountId, _R]),
            crossbar_util:response_db_missing(Context)
    end.            

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create a new account and corresponding database
%% then spawn a short initial function
%% @end
%%--------------------------------------------------------------------
-spec create_new_account_db/1 :: (#cb_context{}) -> #cb_context{}.
create_new_account_db(#cb_context{doc=Doc}=Context) ->
    AccountId = wh_json:get_value(<<"_id">>, Doc, couch_mgr:get_uuid()),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        true -> ok;
        false ->
            couch_mgr:db_create(?WH_ACCOUNTS_DB),
            couch_mgr:revise_doc_from_file(?WH_ACCOUNTS_DB, crossbar, ?ACCOUNTS_AGG_VIEW_FILE),
            couch_mgr:revise_doc_from_file(?WH_ACCOUNTS_DB, crossbar, ?MAINTENANCE_VIEW_FILE)
    end,
    case couch_mgr:db_create(AccountDb) of
        false ->
            lager:debug("failed to create database: ~s", [AccountDb]),
            crossbar_util:response_db_fatal(Context);
        true ->
            lager:debug("created DB for account id ~s", [AccountId]),
            Generators = [fun(J) -> wh_json:set_value(<<"_id">>, AccountId, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, AccountDb, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_id">>, AccountId, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, AccountDb, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_created">>, wh_util:current_tstamp(), J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_wnm_numbers">>, [], J) end
                         ],
            JObj = lists:foldr(fun(F, J) -> F(J) end, Doc, Generators),
            case crossbar_doc:save(Context#cb_context{db_name=AccountDb, account_id=AccountId, doc=JObj}) of
                #cb_context{resp_status=success}=Context1 ->
                    whapps_maintenance:refresh(AccountDb),
                    _ = crossbar_bindings:map(<<"account.created">>, Context1),
                    _ = couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, JObj),
                    _ = notfy_new_account(Context1),
                    Context1;
                Else ->
                    lager:debug("other PUT resp: ~s: ~p~n", [Else#cb_context.resp_status, Else#cb_context.doc]),
                    Else
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the realm in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec is_unique_realm/2 :: (ne_binary() | 'undefined', #cb_context{}) -> boolean().
is_unique_realm(AccountId, #cb_context{req_data=JObj}=Context) ->
    is_unique_realm(AccountId, Context, wh_json:get_ne_value(<<"realm">>, JObj)).

is_unique_realm(_, _, undefined) ->
    lager:debug("invalid or non-unique realm: undefined"),
    false;
is_unique_realm(undefined, _, Realm) ->
    %% unique if Realm doesn't exist in agg DB
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_REALM, [{<<"key">>, Realm}]) of
        {ok, []} ->
            lager:debug("realm ~s is valid and unique", [Realm]),
            true;
        {ok, [_|_]} ->
            lager:debug("invalid or non-unique realm: ~s", [Realm]),
            false
    end;

is_unique_realm(AccountId, Context, Realm) ->
    {ok, Doc} = couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId),
    %% Unique if, for this account, request and account's realm are same
    %% or request Realm doesn't exist in DB (cf is_unique_realm(undefined ...)
    case wh_json:get_value(<<"realm">>, Doc) of
        Realm ->
            lager:debug("realm ~s is valid and unique", [Realm]),
            true;
        _ ->
            is_unique_realm(undefined, Context, Realm)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a notification that the account has been created
%% @end
%%--------------------------------------------------------------------
-spec notfy_new_account/1 :: (#cb_context{}) -> ok.
%% NOTE: when the auth token is empty either signups or onboard allowed this request
%%       and they will notify once complete...
notfy_new_account(#cb_context{auth_doc = undefined}) ->
    ok;
notfy_new_account(#cb_context{doc = JObj}) ->
    Notify = [{<<"Account-Name">>, wh_json:get_value(<<"name">>, JObj)}
              ,{<<"Account-Realm">>, wh_json:get_value(<<"realm">>, JObj)}
              ,{<<"Account-API-Key">>, wh_json:get_value(<<"pvt_api_key">>, JObj)}
              ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
              ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, JObj)}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_new_account(Notify).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Support the depreciated billing id on the account definition, will 
%% be phased out shortly
%% @end
%%--------------------------------------------------------------------
-spec support_depreciated_billing_id/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
support_depreciated_billing_id(undefined, _, Context) ->
    Context;
support_depreciated_billing_id(BillingId, AccountId, Context) ->
    io:format("~p ~p~n", [BillingId, AccountId]),
    try wh_services:set_billing_id(BillingId, AccountId) of
        undefined -> Context;
        Services ->
            _ = wh_services:save(Services),
            Context
    catch
        throw:{Error, Reason} ->
            R = wh_json:set_value([<<"billing_id">>, <<"invalid">>], Reason, wh_json:new()),
            crossbar_util:response(error, wh_util:to_binary(Error), 400, R, Context)
    end.

%% for testing purpose, don't forget to export !
%% is_unique_realm({AccountId, Realm}) -> is_unique_realm(AccountId, #cb_context{}, Realm).

