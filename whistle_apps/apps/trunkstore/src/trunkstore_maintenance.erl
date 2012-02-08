%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% Created : 21 Jan 2012 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(trunkstore_maintenance).

-export([migrate/0]).

-include("ts.hrl").

migrate() ->
    ?LOG("migrating trunkstore information from the ts database"),

    {ok, TSAccts} = couch_mgr:get_results(?TS_DB, <<"ts_accounts/crossbar_listing">>, [{<<"include_docs">>, true}]),
    ?LOG("trying ~b ts accounts", [length(TSAccts)]),

    _ = [maybe_migrate(wh_json:set_value(<<"_rev">>, <<>>, wh_json:get_value(<<"doc">>, Acct))) || Acct <- TSAccts],

    ?LOG("migration complete").

maybe_migrate(AcctJObj) ->
    Realm = wh_json:get_value([<<"account">>, <<"auth_realm">>], AcctJObj),
    case account_exists_with_realm(Realm) of
        {true, AcctDB, AcctID} ->
            ?LOG("account with realm ~s exists in ~s", [Realm, AcctDB]),
            move_doc(AcctDB, AcctID, AcctJObj);
        false ->
            ?LOG("account with realm ~s does not exist", [Realm]),
            case create_account(Realm) of
                {ok, AcctDB, AcctID} ->
                    ?LOG("account db ~s created for realm ~s", [AcctDB, Realm]),
                    move_doc(AcctDB, AcctID, AcctJObj);
                ignore ->
                    ?LOG("failed to create an account db for realm ~s", [Realm])
            end;
        ignore ->
            ?LOG("ignoring ts account with realm ~s", [Realm])
    end.

move_doc(AcctDB, AcctID, TSJObj) ->
    case has_ts_doc(AcctDB) of
        true -> ?LOG("looks like trunkstore account has been moved already");
        false ->
            {ok, AcctTSJObj} = create_ts_doc(AcctDB, AcctID, TSJObj),
            {ok, _} = create_credit_doc(AcctDB, AcctID, AcctTSJObj),
            _ = whapps_maintenance:refresh(AcctID),
            ok
    end.

has_ts_doc(AcctDB) ->
    case couch_mgr:get_results(AcctDB, <<"trunkstore/crossbar_listing">>, []) of
        {ok, []} -> false;
        {ok, _} -> true;
        _ -> false
    end.

create_ts_doc(AcctDB, AcctID, TSJObj) ->
    ?LOG("creating the ts doc in ~s", [AcctDB]),
    JObj = wh_json:set_values([{<<"pvt_type">>, <<"sys_info">>}
                               ,{<<"pvt_account_db">>, AcctDB}
                               ,{<<"pvt_account_id">>, AcctID}
                              ], wh_json:delete_key(<<"_id">>, wh_json:delete_key(<<"_rev">>, TSJObj))),
    ?LOG("saving ts doc ~s into ~s", [wh_json:get_value(<<"_id">>, TSJObj), AcctDB]),
    {ok, _} = couch_mgr:save_doc(AcctDB, JObj).

create_credit_doc(AcctDB, AcctID, TSJObj) ->    
    Credit = wh_json:get_value([<<"account">>, <<"credits">>, <<"prepay">>], TSJObj, 0.0),
    Units = wapi_money:dollars_to_units(wh_util:to_float(Credit)),
    ?LOG("Putting ~p units", [Units]),
    Transaction = wh_json:from_list([{<<"amount">>, Units}
                                     ,{<<"pvt_type">>, <<"credit">>}
                                     ,{<<"pvt_description">>, <<"initial account balance">>}
                                     ,{<<"pvt_account_db">>, AcctDB}
                                     ,{<<"pvt_account_id">>, AcctID}
                                    ]),
    couch_mgr:save_doc(AcctDB, Transaction).
    
account_exists_with_realm(Realm) ->
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_realm">>, [{<<"key">>, Realm}]) of
        {ok, []} -> false;
        {ok, [AcctObj]} -> {true, wh_json:get_value([<<"value">>, <<"account_db">>], AcctObj), wh_json:get_value([<<"value">>, <<"account_id">>], AcctObj)};
        {error, _E} ->
            ?LOG("failed to lookup account view: ~p", [_E]),
            ignore
    end.

create_account(Realm) ->
    AcctID = couch_mgr:get_uuid(),
    AcctDB = wh_util:format_account_id(AcctID, encoded),
    case couch_mgr:db_create(AcctDB) of
        true ->
            ?LOG("created account db: ~s", [AcctDB]),

            case create_account_doc(Realm, AcctID, AcctDB) of
                ignore ->
                    ?LOG("failed to create account doc for ~s", [Realm]),
                    ignore;
                _ ->
                    {ok, AcctDB, AcctID}
            end;
        false ->
            ?LOG("failed to create account db: ~s", [AcctDB]),
            ignore
    end.

create_account_doc(Realm, AcctID, AcctDB) ->
    ?LOG("creating the account doc in ~s and ~s", [AcctDB, ?WH_ACCOUNTS_DB]),

    Default = whapps_config:get(<<"crossbar.accounts">>, <<"default_parent">>, <<>>),

    Doc = wh_json:from_list([{<<"realm">>, Realm}
                             ,{<<"name">>, Realm}
                             ,{<<"pvt_account_id">>, AcctID}
                             ,{<<"pvt_account_db">>, AcctDB}
                             ,{<<"pvt_type">>, <<"account">>}
                             ,{<<"pvt_account_from">>, <<"trunkstore">>}
                             ,{<<"pvt_enabled">>, <<"true">>}
                             ,{<<"pvt_tree">>, [Default]}
                             ,{<<"_id">>, AcctID}
                            ]),
    case couch_mgr:save_doc(AcctDB, Doc) of
        {ok, _} ->
            {ok, _} = couch_mgr:save_doc(?WH_ACCOUNTS_DB, Doc);
        {error, _E} ->
            ?LOG("failed to save account doc into ~s: ~p", [AcctDB, _E]),
            ignore
    end.
