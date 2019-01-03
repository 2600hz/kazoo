%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Crawl accounts and disable notify settings so that we use Teletype instead
%%% @author Mark Magnusson
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_migration_disable_notify).

-export([perform_migration/2]).

-include("crossbar.hrl").

-spec perform_migration(binary(), cb_context:context()) -> cb_context:context().
perform_migration(Account, Context) ->
    lager:info("migrating account ~p from notify to teletype...", [Account]),

    AccountDb = kz_util:format_account_id(Account, 'encoded'),

    {'ok', BaseDoc} = kz_datamgr:open_cache_doc(AccountDb, Account),
    NewDoc = lists:foldl(fun(X, A) -> X(A) end, BaseDoc, [
                                                          fun remove_vm_to_email/1
                                                         ,fun remove_fax_to_email/1
                                                         ]),

    kz_datamgr:ensure_saved(AccountDb, NewDoc),
    cb_context:set_resp_status(Context, 'success').

-spec remove_vm_to_email(kz_json:object()) -> kz_json:object().
remove_vm_to_email(Doc) ->
    kz_json:delete_key([<<"notifications">>, <<"voicemail_to_email">>], Doc).

-spec remove_fax_to_email(kz_json:object()) -> kz_json:object().
remove_fax_to_email(Doc) ->
    kz_json:delete_key([<<"notifications">>, <<"fax_to_email">>], Doc).
