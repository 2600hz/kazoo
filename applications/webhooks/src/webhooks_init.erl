%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_init).

-export([start_link/0
        ,init_modules/0
        ,existing_modules/0
        ,maybe_init_account/2
        ]).

-include("webhooks.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    kz_log:put_callid(?MODULE),
    _ = kz_process:spawn(fun do_init/0),
    'ignore'.

-spec do_init() -> 'ok'.
do_init() ->
    init_dbs(),
    init_modules().

-spec do_init(kz_term:ne_binary()) -> 'ok'.
do_init(MasterAccountDb) ->
    init_master_account_db(MasterAccountDb),
    init_modules().

-spec init_dbs() -> 'ok'.
init_dbs() ->
    _ = init_master_account_db(),
    webhooks_util:init_webhook_db().

-spec maybe_init_account(kz_json:object(), kz_term:proplist()) -> 'ok' | 'false'.
maybe_init_account(JObj, _Props) ->
    Database = kapi_conf:get_database(JObj),
    kz_datamgr:db_classification(Database) =:= 'account'
        andalso do_init(Database).

-spec init_master_account_db() -> 'ok'.
init_master_account_db() ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            init_master_account_db(MasterAccountDb),
            remove_old_notifications_webhooks(MasterAccountDb);
        {'error', _} ->
            lager:debug("master account hasn't been created yet"),
            webhooks_shared_listener:add_account_bindings()
    end.

-spec init_master_account_db(kz_term:ne_binary()) -> 'ok'.
init_master_account_db(MasterAccountDb) ->
    _ = kapps_maintenance:refresh(MasterAccountDb),
    lager:debug("loaded view into master db ~s", [MasterAccountDb]).

-spec remove_old_notifications_webhooks(kz_term:ne_binary()) -> 'ok'.
remove_old_notifications_webhooks(MasterAccountDb) ->
    ToRemove = [<<"webhooks_callflow">>
               ,<<"webhooks_inbound_fax">>
               ,<<"webhooks_outbound_fax">>
               ],
    case kz_datamgr:del_docs(MasterAccountDb, ToRemove) of
        {'ok', _} ->
            lager:debug("old notifications webhooks deleted");
        {'error', _Reason} ->
            lager:debug("failed to remove old notifications webhooks: ~p", [_Reason])
    end.

-spec init_modules() -> 'ok'.
init_modules() ->
    lists:foreach(fun init_module/1, existing_modules()),
    lager:debug("finished initializing modules").

-spec init_module(atom()) -> 'ok'.
init_module(Module) ->
    lager:debug("initializing ~s", [Module]),
    try Module:init() of
        _ -> lager:debug("~s initialized", [Module])
    catch
        'error':'undef' ->
            lager:debug("~s doesn't export init/0", [Module]);
        _E:_R ->
            lager:debug("~s failed: ~s: ~p", [Module, _E, _R])
    end.

-spec existing_modules() -> kz_term:atoms().
existing_modules() ->
    existing_modules(code:lib_dir(kz_term:to_atom(?APP_NAME))).

-spec existing_modules(string()) -> kz_term:atoms().
existing_modules(WebhooksRoot) ->
    ModulesDirectory = filename:join(WebhooksRoot, "ebin"),
    Extension = ".beam",
    Utils = ["webhooks_app"
            ,"webhooks_channel_util"
            ,"webhooks_disabler"
            ,"webhooks_init"
            ,"webhooks_listener"
            ,"webhooks_maintenance"
            ,"webhooks_shared_listener"
            ,"webhooks_skel"
            ,"webhooks_sup"
            ,"webhooks_util"
            ],
    Pattern = filename:join(ModulesDirectory, "*"++Extension),
    [kz_term:to_atom(Module, 'true')
     || Path <- filelib:wildcard(Pattern),
        not lists:member((Module=filename:basename(Path, Extension)), Utils)
    ].
