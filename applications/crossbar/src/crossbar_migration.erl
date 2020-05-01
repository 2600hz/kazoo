%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Crawl accounts and disable notify settings so that we use Teletype instead
%%% @author Mark Magnusson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_migration).

-export([perform/3
        ,list/0
        ]).

-include("crossbar.hrl").

%% Id, Description, Callback Module
-define(MIGRATIONS_LIST, kz_json:from_list(
                           [{<<"notify_to_teletype">>, <<"Migrate account and all sub accounts to Teletype">>}
                           ]
                          )
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec list() -> kz_json:object().
list() -> ?MIGRATIONS_LIST.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec perform(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
perform(<<"notify_to_teletype">>, Account, Context) ->
    lager:info("migrating account ~p from notify to teletype...", [Account]),

    Updates = [{[<<"notifications">>, <<"voicemail_to_email">>], 'null'}
              ,{[<<"notifications">>, <<"fax_to_email">>], 'null'}
              ],
    {'ok', _} = kzd_accounts:update(Account, Updates),
    cb_context:set_resp_status(Context, 'success').
