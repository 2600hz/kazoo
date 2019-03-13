%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc kazoo data utils
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzs_util).

-export([db_classification/1
        ,db_priority/1
        ,bind_db_classify/3, unbind_db_classify/3
        ,sort_by_priority/1
        ]).

-export([get_view_json/1, get_view_json/2, get_views_json/2]).

-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").
-include_lib("kazoo_documents/include/kzd_ratedeck.hrl").
-include("kz_data.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_classification(kz_term:text()) -> db_classification().
db_classification(Db) when not is_binary(Db) ->
    db_classification(kz_term:to_binary(Db));
db_classification(<<"_users">>) -> 'external';
db_classification(<<"_dbs">>) -> 'external';
db_classification(<<"users">>) -> 'external';
db_classification(<<"dbs">>) -> 'external';
db_classification(<<"_metadata">>) -> 'external';
db_classification(<<"_nodes">>) -> 'external';
db_classification(<<"nodes">>) -> 'external';
db_classification(<<"_replicator">>) -> 'external';
db_classification(<<"_global_changes">>) -> 'external';
db_classification(<<"ts">>) -> 'deprecated';
db_classification(<<"crossbar_schemas">>) -> 'deprecated';
db_classification(<<"registrations">>) -> 'deprecated';
db_classification(<<"crossbar%2Fsessions">>) -> 'deprecated';
db_classification(<<"sms">>) -> 'deprecated';
db_classification(<<"cccps">>) -> 'system';
db_classification(<<"signups">>) -> 'deprecated';
db_classification(?KZ_RATES_DB) -> 'ratedeck';
db_classification(?MATCH_RATEDECK_DB_ENCODED(_)) -> 'ratedeck';
db_classification(?MATCH_RATEDECK_DB_encoded(_)) -> 'ratedeck';
db_classification(?MATCH_RATEDECK_DB_UNENCODED(_)) -> 'ratedeck';
db_classification(?KZ_ALERTS_DB) -> 'system';
db_classification(?KZ_OFFNET_DB) -> 'system';
db_classification(?KZ_ANONYMOUS_CDR_DB) -> 'system';
db_classification(?KZ_DEDICATED_IP_DB) -> 'system';
db_classification(?KZ_CONFIG_DB) -> 'system';
db_classification(?KZ_MEDIA_DB) -> 'system';
db_classification(?KZ_SCHEMA_DB) -> 'system';
db_classification(?KZ_OAUTH_DB) -> 'system';
db_classification(?KZ_AUTH_DB) -> 'system';
db_classification(?KZ_DATA_DB) -> 'system';
db_classification(?KZ_TASKS_DB) -> 'system';
db_classification(?KZ_PENDING_NOTIFY_DB) -> 'system';
db_classification(?KZ_PROVISIONER_DB) -> 'system'; %% Soon to be deprecated
db_classification(?KZ_ACCOUNTS_DB) -> 'aggregate';
db_classification(?KZ_TOKEN_DB) -> 'aggregate';
db_classification(?KZ_SIP_DB) -> 'aggregate';
db_classification(?KZ_FAXES_DB) -> 'aggregate';
db_classification(?KZ_ACDC_DB) -> 'aggregate';
db_classification(?KZ_SERVICES_DB) -> 'aggregate';
db_classification(?KZ_PORT_REQUESTS_DB) -> 'aggregate';
db_classification(?KZ_WEBHOOKS_DB) -> 'aggregate';
db_classification(<<?KNM_DB_PREFIX, _/binary>>) -> 'numbers';
db_classification(<<?KNM_DB_PREFIX_ENCODED, _/binary>>) -> 'numbers';
db_classification(<<?KNM_DB_PREFIX_encoded, _/binary>>) -> 'numbers';
db_classification(?MATCH_RESOURCE_SELECTORS_UNENCODED(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_RESOURCE_SELECTORS_encoded(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_RESOURCE_SELECTORS_ENCODED(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_RESOURCE_SELECTORS_RAW(_AccountId)) -> 'resource_selectors';
db_classification(?MATCH_MODB_SUFFIX_UNENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';% these only need to match
db_classification(?MATCH_MODB_SUFFIX_ENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   "account..." then the
db_classification(?MATCH_MODB_SUFFIX_encoded(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   right size.
db_classification(?MATCH_MODB_SUFFIX_RAW(_Account,_Year,_Month)) -> 'modb';%   right size.
db_classification(?MATCH_ACCOUNT_UNENCODED(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_encoded(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_ENCODED(_AccountId)) -> 'account';
db_classification(?MATCH_PROVISIONER_RAW(_AccountId)) -> 'provisioner';
db_classification(?MATCH_PROVISIONER_ENCODED(_AccountId)) -> 'provisioner';
db_classification(?MATCH_PROVISIONER_encoded(_AccountId)) -> 'provisioner';
db_classification(Database) ->
    case kazoo_bindings:map(binding_db_classify(Database), [Database]) of
        [] -> unknown_db_classification(Database);
        Classifications -> find_first(Database, Classifications)
    end.

-spec find_first(kz_term:ne_binary(), kz_term:atoms()) -> atom().
find_first(Database, []) -> unknown_db_classification(Database);
find_first(Database, ['undefined' | Cs]) -> find_first(Database, Cs);
find_first(_Database, [Classification | _]) -> Classification.

-spec unknown_db_classification(kz_term:ne_binary()) -> 'undefined'.
unknown_db_classification(_Database) ->
    lager:warning("unknown type for database ~s", [_Database]),
    {'current_stacktrace', ST} = erlang:process_info(self(), 'current_stacktrace'),
    kz_util:log_stacktrace(ST),
    'undefined'.

-spec binding_db_classify(kz_term:ne_binary()) -> kz_term:ne_binary().
binding_db_classify(Database) ->
    Encoded = kz_amqp_util:encode(Database),
    <<"db.classify.", Encoded/binary>>.

-spec bind_db_classify(kz_term:ne_binary(), module(), atom()) -> any().
bind_db_classify(Database, M, F) ->
    kazoo_bindings:bind(binding_db_classify(Database), M, F).

-spec unbind_db_classify(kz_term:ne_binary(), module(), atom()) -> any().
unbind_db_classify(Database, M, F) ->
    kazoo_bindings:unbind(binding_db_classify(Database), M, F).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sort_by_priority(kz_term:ne_binaries()) -> kz_term:ne_binaries().
sort_by_priority(Databases) ->
    MODBPriority = modb_priority(),
    lists:sort(fun(Db1, Db2) -> sort_by_priority(Db1, Db2, MODBPriority) end, lists:usort(Databases)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sort_by_priority(kz_term:ne_binary(), kz_term:ne_binary(), map()) -> boolean().
sort_by_priority(Db1, Db2, MODBPriority) ->
    db_priority(Db1, MODBPriority) < db_priority(Db2, MODBPriority).

-spec modb_priority() -> map().
modb_priority() ->
    {Year, Month, _Day} = erlang:date(),
    {PrevYear1, PrevMonth1} = prev_year_month(Year, Month),
    {PrevYear2, PrevMonth2} = prev_year_month(Year, Month),
    #{current=> {kz_term:to_binary(Year), kz_date:pad_month(Month)}
     ,prev => {kz_term:to_binary(PrevYear1), kz_date:pad_month(PrevMonth1)}
     ,preprev => {kz_term:to_binary(PrevYear2), kz_date:pad_month(PrevMonth2)}
     }.

-spec prev_year_month(kz_time:year(), kz_time:month()) -> {kz_time:year(), kz_time:month()}.
prev_year_month(Year, 1) -> {Year - 1, 12};
prev_year_month(Year, Month) -> {Year, Month - 1}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_priority(kz_term:text()) -> non_neg_integer().
db_priority(Db) ->
    db_priority(Db, modb_priority()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_priority(kz_term:text(), map()) -> non_neg_integer().
db_priority(Db, MODBPriority) when not is_binary(Db) ->
    db_priority(kz_term:to_binary(Db), MODBPriority);
db_priority(?KZ_CONFIG_DB, _) -> 0;
db_priority(?KZ_DATA_DB, _) -> 1;
db_priority(?KZ_OFFNET_DB, _) -> 2;
db_priority(?KZ_ACCOUNTS_DB, _) -> 3;
db_priority(?KZ_SIP_DB, _) -> 4;
db_priority(?KZ_AUTH_DB, _) -> 5;
db_priority(?KZ_WEBHOOKS_DB, _) -> 6;
db_priority(?KZ_RATES_DB, _) -> 7;
db_priority(?KZ_ACDC_DB, _) -> 8;
db_priority(?KZ_FAXES_DB, _) -> 9;
db_priority(?KZ_SCHEMA_DB, _) -> 10;
db_priority(?KZ_SERVICES_DB, _) -> 11;
db_priority(?KZ_PORT_REQUESTS_DB, _) -> 12;
db_priority(?KZ_TASKS_DB, _) -> 13;
db_priority(?KZ_PENDING_NOTIFY_DB, _) -> 13;
db_priority(?KZ_DEDICATED_IP_DB, _) -> 14;
db_priority(?KZ_ALERTS_DB, _) -> 15;
db_priority(?KZ_MEDIA_DB, _) -> 16;
db_priority(?KZ_OAUTH_DB, _) -> 17;
db_priority(?KZ_TOKEN_DB, _) -> 18;
db_priority(<<?KNM_DB_PREFIX, _/binary>>, _) -> 19;
db_priority(<<?KNM_DB_PREFIX_ENCODED, _/binary>>, _) -> 19;
db_priority(<<?KNM_DB_PREFIX_encoded, _/binary>>, _) -> 19;
db_priority(?MATCH_ACCOUNT_UNENCODED(_AccountId), _) -> 21;
db_priority(?MATCH_ACCOUNT_encoded(_AccountId), _) -> 21;
db_priority(?MATCH_ACCOUNT_ENCODED(_AccountId), _) -> 21;
db_priority(?MATCH_MODB_SUFFIX_UNENCODED(_A,_B,_Rest,_Year,_Month) = Db, MODBPriority) ->
    db_priority_modb(kz_util:format_account_modb(Db, 'raw'), MODBPriority);
db_priority(?MATCH_MODB_SUFFIX_ENCODED(_A,_B,_Rest,_Year,_Month) = Db, MODBPriority) ->
    db_priority_modb(kz_util:format_account_modb(Db, 'raw'), MODBPriority);
db_priority(?MATCH_MODB_SUFFIX_encoded(_A,_B,_Rest,_Year,_Month) = Db, MODBPriority) ->
    db_priority_modb(kz_util:format_account_modb(Db, 'raw'), MODBPriority);
db_priority(?MATCH_MODB_SUFFIX_RAW(_Account,_Year,_Month) = Db, MODBPriority) ->
    db_priority_modb(kz_util:format_account_modb(Db, 'raw'), MODBPriority);
db_priority(?MATCH_RESOURCE_SELECTORS_UNENCODED(_AccountId), _) -> 26;
db_priority(?MATCH_RESOURCE_SELECTORS_encoded(_AccountId), _) -> 26;
db_priority(?MATCH_RESOURCE_SELECTORS_ENCODED(_AccountId), _) -> 26;
db_priority(?MATCH_RESOURCE_SELECTORS_RAW(_AccountId), _) -> 26;
db_priority(?MATCH_PROVISIONER_ENCODED(_AccountId), _) -> 27;
db_priority(?MATCH_PROVISIONER_encoded(_AccountId), _) -> 27;
db_priority(_Database, _) -> 27.

db_priority_modb(?MATCH_MODB_SUFFIX_RAW(_Account, Year, Month)
                ,#{current := {Year, Month}}
                ) ->
    22;
db_priority_modb(?MATCH_MODB_SUFFIX_RAW(_Account, Year, Month)
                ,#{prev := {Year, Month}}
                ) ->
    23;
db_priority_modb(?MATCH_MODB_SUFFIX_RAW(_Account, Year, Month)
                ,#{preprev := {Year, Month}}
                ) ->
    24;
db_priority_modb(_, _) ->
    25.

-spec get_views_json(atom(), string()) -> kz_datamgr:views_listing().
get_views_json(App, Folder) ->
    Pattern = filename:join([code:priv_dir(App), "couchdb", Folder, "*.json"]),
    [ViewListing
     || File <- filelib:wildcard(Pattern),
        {?NE_BINARY,_}=ViewListing <- [catch get_view_json(File)]
    ].

-spec get_view_json(atom(), kz_term:text()) -> kz_datamgr:view_listing().
get_view_json(App, File) ->
    Path = filename:join([code:priv_dir(App), "couchdb", File]),
    get_view_json(Path).

-spec get_view_json(kz_term:text()) -> kz_datamgr:view_listing().
get_view_json(Path) ->
    lager:debug("fetching view from ~s", [Path]),
    {'ok', Bin} = file:read_file(Path),
    JObj = kz_json:decode(Bin),
    {kz_doc:id(JObj), JObj}.
