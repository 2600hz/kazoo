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
        ]).

-include("webhooks.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    init_dbs(),
    init_modules(),
    'ignore'.

init_dbs() ->
    init_master_account_db(),
    webhooks_util:init_webhook_db().

init_master_account_db() ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    _ = couch_mgr:revise_doc_from_file(MasterAccountDb
                                       ,'webhooks'
                                       ,<<"webhooks.json">>
                                      ),
    'ok'.

-spec init_modules() -> 'ok'.
init_modules() ->
    Init = [{Mod, init_module(Mod)}
            || Mod <- existing_modules()
           ],
    lager:debug("initializing: ~p", [Init]).

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
    ModulesDirectory =
        filename:join([code:lib_dir('webhooks')
                       ,"src"
                       ,"modules"
                      ]),
    existing_modules(ModulesDirectory).

-spec existing_modules(text()) -> atoms().
existing_modules(ModulesDirectory) ->
    filelib:fold_files(ModulesDirectory
                       ,"\\.erl"
                       ,'false'
                       ,fun fold_files/2
                       ,[]
                      ).

-spec fold_files(text(), atoms()) -> atoms().
fold_files(File, Acc) ->
    [wh_util:to_atom(
       filename:basename(File, ".erl")
       ,'true'
      )
     | Acc
    ].
