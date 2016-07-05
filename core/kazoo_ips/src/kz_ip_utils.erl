%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_ip_utils).

-include("kazoo_ips.hrl").

-export([refresh_database/0
        ,refresh_database/1
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh_database() -> 'ok'.
refresh_database() ->
    _ = kz_datamgr:db_create(?KZ_DEDICATED_IP_DB),
    _ = kz_datamgr:revise_docs_from_folder(?KZ_DEDICATED_IP_DB
                                          ,'kazoo_ips'
                                          ,"views"
                                          ),
    'ok'.

-spec refresh_database(function()) -> any().
refresh_database(Callback) ->
    _ = refresh_database(),
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
