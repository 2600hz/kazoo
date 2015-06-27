%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(trunkstore_maintenance).

-export([migrate/0
         ,i_understand_migrate/0
         ,clear_old_calls/0
         ,classifier_inherit/2
         ,classifier_deny/2
         ,flush/0, flush/1
        ]).

-include("ts.hrl").

-spec flush() -> 'ok'.
-spec flush(ne_binary()) -> 'ok'.
flush() ->
    wh_cache:flush_local(?TRUNKSTORE_CACHE).

flush(Account) ->
    AccountId = wh_util:format_account_id(Account),

    Flush = wh_cache:filter_local(?TRUNKSTORE_CACHE
                                  ,fun(Key, _Value) -> is_ts_cache_object(Key, AccountId) end
                                 ),
    _ = [wh_cache:erase_local(?TRUNKSTORE_CACHE, Key) || {Key, _Value} <- Flush],
    'ok'.

migrate() ->
    io:format("This command is ancient, if you REALLY know what you are duing run:~n"),
    io:format(" sup trunkstore_maintenance i_understand_migrate~n").

i_understand_migrate() ->
    lager:info("migrating trunkstore information from the ts database"),
    case couch_mgr:get_results(?TS_DB, <<"ts_accounts/crossbar_listing">>, [{<<"include_docs">>, 'true'}]) of
        {'error', 'not_found'} ->
            lager:info("ts database not found or ts_account/crossbar_listing view missing");
        {'ok', TSAccts} ->
            lager:info("trying ~b ts accounts", [length(TSAccts)]),
            _ = [maybe_migrate(wh_doc:set_revision(wh_json:get_value(<<"doc">>, Acct), <<>>))
                 || Acct <- TSAccts
                ],
            lager:info("migration complete")
    end.

maybe_migrate(AcctJObj) ->
    Realm = wh_json:get_value([<<"account">>, <<"auth_realm">>], AcctJObj),
    case account_exists_with_realm(Realm) of
        {'true', AcctDB, AcctID} ->
            lager:info("account with realm ~s exists in ~s", [Realm, AcctDB]),
            move_doc(AcctDB, AcctID, AcctJObj);
        'false' ->
            lager:info("account with realm ~s does not exist", [Realm]),
            case create_account(Realm) of
                {'ok', AcctDB, AcctID} ->
                    lager:info("account db ~s created for realm ~s", [AcctDB, Realm]),
                    move_doc(AcctDB, AcctID, AcctJObj);
                'ignore' ->
                    lager:info("failed to create an account db for realm ~s", [Realm])
            end;
        'ignore' -> lager:info("ignoring ts account with realm ~s", [Realm])
    end.

move_doc(AcctDB, AcctID, TSJObj) ->
    case has_ts_doc(AcctDB) of
        'true' -> lager:info("looks like trunkstore account has been moved already");
        'false' ->
            {'ok', AcctTSJObj} = create_ts_doc(AcctDB, AcctID, TSJObj),
            {'ok', _} = create_credit_doc(AcctDB, AcctID, AcctTSJObj),
            _ = whapps_maintenance:refresh(AcctID),
            'ok'
    end.

has_ts_doc(AcctDB) ->
    case couch_mgr:get_results(AcctDB, <<"trunkstore/crossbar_listing">>, []) of
        {'ok', []} -> 'false';
        {'ok', _} -> 'true';
        _ -> 'false'
    end.

create_ts_doc(AcctDB, AcctID, TSJObj) ->
    lager:info("creating the ts doc in ~s", [AcctDB]),
    JObj = wh_json:set_values([{<<"pvt_type">>, <<"sys_info">>}
                               ,{<<"pvt_account_db">>, AcctDB}
                               ,{<<"pvt_account_id">>, AcctID}
                              ], wh_json:delete_key(<<"_id">>, wh_doc:delete_revision(TSJObj))),
    lager:info("saving ts doc ~s into ~s", [wh_doc:id(TSJObj), AcctDB]),
    {'ok', _} = couch_mgr:save_doc(AcctDB, JObj).

create_credit_doc(AcctDB, AcctID, TSJObj) ->
    Credit = wh_json:get_value([<<"account">>, <<"credits">>, <<"prepay">>], TSJObj, 0.0),
    Units = wht_util:dollars_to_units(wh_util:to_float(Credit)),
    lager:info("Putting ~p units", [Units]),
    Transaction = wh_json:from_list([{<<"amount">>, Units}
                                     ,{<<"pvt_type">>, <<"credit">>}
                                     ,{<<"pvt_description">>, <<"initial account balance">>}
                                     ,{<<"pvt_account_db">>, AcctDB}
                                     ,{<<"pvt_account_id">>, AcctID}
                                    ]),
    couch_mgr:save_doc(AcctDB, Transaction).

account_exists_with_realm(Realm) ->
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Realm)}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, <<"accounts/listing_by_realm">>, ViewOptions) of
        {'ok', []} -> 'false';
        {'ok', [AcctObj]} ->
            {'true'
             ,wh_json:get_value([<<"value">>, <<"account_db">>], AcctObj)
             ,wh_json:get_value([<<"value">>, <<"account_id">>], AcctObj)};
        {'error', _E} ->
            lager:info("failed to lookup account view: ~p", [_E]),
            'ignore'
    end.

create_account(Realm) ->
    AcctID = couch_mgr:get_uuid(),
    AcctDB = wh_util:format_account_id(AcctID, 'encoded'),
    case couch_mgr:db_create(AcctDB) of
        'true' ->
            lager:info("created account db: ~s", [AcctDB]),
            case create_account_doc(Realm, AcctID, AcctDB) of
                'ignore' ->
                    lager:info("failed to create account doc for ~s", [Realm]),
                    'ignore';
                _ ->
                    {'ok', AcctDB, AcctID}
            end;
        'false' ->
            lager:info("failed to create account db: ~s", [AcctDB]),
            'ignore'
    end.

create_account_doc(Realm, AcctID, AcctDB) ->
    lager:info("creating the account doc in ~s and ~s", [AcctDB, ?WH_ACCOUNTS_DB]),
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
        {'ok', _} ->
            {'ok', _} = couch_mgr:save_doc(?WH_ACCOUNTS_DB, Doc);
        {'error', _E} ->
            lager:info("failed to save account doc into ~s: ~p", [AcctDB, _E]),
            'ignore'
    end.

%% some calls get stuck if they miss the CDR
%% this clears them out
clear_old_calls() ->
    _ = clear_old_calls('ts_offnet_sup'),
    _ = clear_old_calls('ts_onnet_sup'),
    'ok'.

clear_old_calls(Super) ->
    Ps = [P || {_,P,_,_} <- supervisor:which_children(Super)],
    [begin
         {'dictionary', D} = erlang:process_info(P, 'dictionary'),
         C = proplists:get_value('callid', D),
         case whapps_call_command:channel_status(C) of
             {'error', _} -> {P, C, exit(P, 'kill')};
             _ -> {P, C, 'ok'}
         end
     end || P <- Ps
    ].

%%
%% Usage example: sup trunkstore_maintenance classifier_inherit international pbx_username@realm.domain.tld
%%
classifier_inherit(Classifier, UserR) ->
    set_classifier_action(<<"inherit">>, Classifier, UserR).

%%
%% Usage example: sup trunkstore_maintenance classifier_deny international pbx_username@realm.domain.tld
%%
classifier_deny(Classifier, UserR) ->
    set_classifier_action(<<"deny">>, Classifier, UserR).

set_classifier_action(Action, Classifier, UserR) ->
    io:format("Classifier: ~p",[Classifier]),
    Classifiers = wnm_util:available_classifiers(),
    case lists:member(Classifier, wh_json:get_keys(Classifiers)) of
        'false' ->
            io:format("\nNo ~p classifier among configured classifiers ~p\n",[Classifier, wh_json:get_keys(Classifiers)]),
            exit(no_such_classifier);
        _ ->
            io:format("  ... found\n")
    end,
    [User, Realm] = re:split(UserR, <<"@">>, [{return,binary},{parts,2}]),
    case account_exists_with_realm(Realm) of
        {'true', AcctDB, AcctID} ->
            {'ok', Opts} = ts_util:lookup_user_flags(User, Realm, AcctID),
            TSDocId = wh_json:get_first_defined([<<"_id">>, <<"id">>], Opts),
            couch_mgr:update_doc(AcctDB, TSDocId, [{[<<"call_restriction">>, Classifier, <<"action">>], Action}]),
            io:format("Success\n");
        'false' ->
            io:format("Failed: account with realm ~p does not exist\n", [Realm])
    end.

-spec is_ts_cache_object(tuple(), ne_binary()) -> boolean().
is_ts_cache_object({'lookup_user_flags', _Realm, _User, AccountId}, AccountId) ->
    'true';
is_ts_cache_object({'lookup_did', _DID, AccountId}, AccountId) ->
    'true';
is_ts_cache_object(_Key, _AccountId) -> 'false'.
