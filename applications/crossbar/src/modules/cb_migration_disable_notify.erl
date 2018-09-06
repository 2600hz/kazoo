%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
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

    Updates = [{[<<"notifications">>, <<"voicemail_to_email">>], 'null'}
              ,{[<<"notifications">>, <<"fax_to_email">>], 'null'}
              ],
    {'ok', _} = kzd_accounts:update(Account, Updates),
    cb_context:set_resp_status(Context, 'success').
