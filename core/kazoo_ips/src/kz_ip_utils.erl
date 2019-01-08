%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ip_utils).

-include("kazoo_ips.hrl").

-export([refresh_database/0, refresh_database/1]).

-spec refresh_database() -> 'ok'.
refresh_database() ->
    _ = kapi_maintenance:refresh_database(?KZ_DEDICATED_IP_DB),
    _ = kapi_maintenance:refresh_views(?KZ_DEDICATED_IP_DB),
    'ok'.

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
