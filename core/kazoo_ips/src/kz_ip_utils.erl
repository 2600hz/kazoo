%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ip_utils).

-include("kazoo_ips.hrl").

-export([refresh_database/0, refresh_database/1]).

-spec refresh_database() -> 'ok'.
refresh_database() ->
    init_db(),
    _ = kapps_maintenance:refresh(?KZ_DEDICATED_IP_DB),
    'ok'.

init_db() ->
    case kz_datamgr:db_exists(?KZ_DEDICATED_IP_DB) of
        'false' ->
            Result = kz_datamgr:db_create(?KZ_DEDICATED_IP_DB),
            lager:debug("~s is created: ~p", [?KZ_DEDICATED_IP_DB, Result]);
        'true' -> 'ok'
    end.

-spec refresh_database(function()) -> any().
refresh_database(Callback) ->
    refresh_database(),
    case refresh_database_retries() < 2 of
        'false' -> {'error', 'not_found'};
        'true' -> Callback()
    end.

-spec refresh_database_retries() -> non_neg_integer().
refresh_database_retries() ->
    case get('dedicated_ip_retries') of
        'undefined' ->
            put('dedicated_ip_retries', 0),
            0;
        Count ->
            put('dedicated_ip_retries', Count + 1)
    end.
