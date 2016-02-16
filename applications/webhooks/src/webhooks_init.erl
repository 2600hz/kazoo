%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_init).

-export([start_link/0
         ,init_modules/0
         ,existing_modules/0
         ,maybe_init_account/2
        ]).

-include("webhooks.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    wh_util:put_callid(?MODULE),
    _ = wh_util:spawn(fun do_init/0),
    'ignore'.

-spec do_init() -> 'ok'.
-spec do_init(ne_binary()) -> 'ok'.
do_init() ->
    init_dbs(),
    init_modules().

do_init(MasterAccountDb) ->
    init_master_account_db(MasterAccountDb),
    init_modules().

-spec init_dbs() -> 'ok'.
init_dbs() ->
    _ = init_master_account_db(),
    webhooks_util:init_webhook_db().

-spec maybe_init_account(wh_json:object(), wh_proplist()) -> 'ok' | 'false'.
maybe_init_account(JObj, _Props) ->
    Database = wapi_conf:get_database(JObj),
    couch_util:db_classification(Database) =:= 'account'
        andalso do_init(Database).

-spec init_master_account_db() -> 'ok'.
-spec init_master_account_db(ne_binary()) -> 'ok'.
init_master_account_db() ->
    case whapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            init_master_account_db(MasterAccountDb);
        {'error', _} ->
            lager:debug("master account hasn't been created yet"),
            webhooks_shared_listener:add_account_bindings()
    end.

init_master_account_db(MasterAccountDb) ->
    _ = kz_datamgr:revise_doc_from_file(MasterAccountDb
                                      ,'webhooks'
                                      ,<<"webhooks.json">>
                                      ),
    lager:debug("loaded view into master db ~s", [MasterAccountDb]).

-spec init_modules() -> 'ok'.
init_modules() ->
    _ = [init_module(Mod)
         || Mod <- existing_modules()
        ],
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

-spec existing_modules() -> atoms().
existing_modules() ->
    existing_modules(code:lib_dir(wh_util:to_atom(?APP_NAME))).

-spec existing_modules(string()) -> atoms().
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
    [wh_util:to_atom(Module)
     || Path <- filelib:wildcard(Pattern),
        not lists:member((Module=filename:basename(Path, Extension)), Utils)
    ].
