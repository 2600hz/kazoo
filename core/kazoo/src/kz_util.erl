%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_util).

-export([log_stacktrace/0, log_stacktrace/1
        ,format_account_id/1, format_account_id/2, format_account_id/3
        ,format_account_mod_id/1, format_account_mod_id/2, format_account_mod_id/3
        ,format_account_db/1
        ,format_account_modb/1, format_account_modb/2
        ,format_resource_selectors_id/1, format_resource_selectors_id/2
        ,format_resource_selectors_db/1
        ,normalize_account_name/1
        ,account_update/1, account_update/2
        ]).
-export([is_in_account_hierarchy/2, is_in_account_hierarchy/3]).
-export([is_system_admin/1]).
-export([get_account_realm/1, get_account_realm/2]).
-export([is_account_enabled/1, is_account_expired/1]).
-export([maybe_disable_account/1
        ,disable_account/1
        ,enable_account/1
        ,set_superduper_admin/2
        ,set_allow_number_additions/2
        ]).

-export([try_load_module/1]).
-export([shuffle_list/1]).

-export([to_integer/1, to_integer/2
        ,to_float/1, to_float/2
        ,to_number/1
        ,to_hex/1, to_hex_binary/1, rand_hex_binary/1
        ,hexencode_binary/1
        ,from_hex_binary/1, from_hex_string/1
        ,to_list/1, to_binary/1
        ,to_atom/1, to_atom/2
        ,to_date/1
        ,to_datetime/1
        ,error_to_binary/1
        ]).
-export([to_boolean/1, is_boolean/1
        ,is_true/1, is_false/1
        ,is_ne_binary/1
        ,is_empty/1, is_not_empty/1
        ,is_proplist/1
        ,identity/1
        ,always_true/1, always_false/1
        ]).
-export([to_lower_binary/1, to_upper_binary/1
        ,to_lower_string/1, to_upper_string/1
        ,ucfirst_binary/1, lcfirst_binary/1
        ,strip_binary/1, strip_binary/2
        ,strip_left_binary/2, strip_right_binary/2
        ,suffix_binary/2
        ,truncate_binary/2, truncate_binary/3
        ,truncate_left_binary/2, truncate_right_binary/2
        ]).

-export([clean_binary/1, clean_binary/2
        ,remove_white_spaces/1
        ]).

-export([uri_encode/1
        ,uri_decode/1
        ,resolve_uri/2
        ,safe_urlencode/1
        ]).

-export([uri/2]).

-export([pad_month/1]).

-export([binary_md5/1]).
-export([pad_binary/3, pad_binary_left/3
        ,join_binary/1, join_binary/2
        ,binary_reverse/1
        ]).
-export([a1hash/3, floor/1, ceiling/1]).

-export([current_tstamp/0, current_unix_tstamp/0
        ,gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1
        ,unix_timestamp_to_gregorian_seconds/1
        ,pretty_print_datetime/1
        ,rfc1036/1, rfc1036/2
        ,iso8601/1
        ,pretty_print_elapsed_s/1
        ,decr_timeout/2
        ,pretty_print_bytes/1, pretty_print_bytes/2
        ,bin_usage/0, mem_usage/0
        ]).
-export([microseconds_to_seconds/1
        ,milliseconds_to_seconds/1
        ,elapsed_s/1, elapsed_ms/1, elapsed_us/1
        ,elapsed_s/2, elapsed_ms/2, elapsed_us/2
        ,now/0, now_s/0, now_ms/0, now_us/0
        ,now_s/1, now_ms/1, now_us/1
        ]).

-export([runs_in/3]).
-export([put_callid/1, get_callid/0
        ,spawn/1, spawn/2
        ,spawn_link/1, spawn_link/2
        ,spawn_monitor/2
        ,set_startup/0, startup/0
        ]).
-export([get_event_type/1]).
-export([get_xml_value/2]).

-export([kazoo_version/0, write_pid/1]).

-export([change_console_log_level/1
        ,change_error_log_level/1
        ,change_syslog_log_level/1
        ]).

-export([format_date/0, format_date/1]).
-export([format_time/0, format_time/1]).
-export([format_datetime/0, format_datetime/1]).

-export([node_name/0, node_hostname/0]).

-export([anonymous_caller_id_name/0
        ,anonymous_caller_id_number/0
        ]).

-export([write_file/2, write_file/3
        ,rename_file/2
        ,delete_file/1
        ,make_dir/1
        ]).

-export([calling_app/0]).
-export([calling_app_version/0]).
-export([calling_process/0]).
-export([get_app/1]).

-export([application_version/1]).

-export([iolist_join/2]).

-include_lib("kernel/include/inet.hrl").

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_api_literals.hrl").

-define(KAZOO_VERSION_CACHE_KEY, {?MODULE, 'kazoo_version'}).

-export_type([account_format/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Standardized way of logging the stacktrace...
%% @end
%%--------------------------------------------------------------------
-spec log_stacktrace() -> 'ok'.
-spec log_stacktrace(list()) -> 'ok'.
log_stacktrace() ->
    ST = erlang:get_stacktrace(),
    log_stacktrace(ST).

log_stacktrace(ST) ->
    lager:error("stacktrace:"),
    _ = [log_stacktrace_mfa(M, F, A, Info)
         || {M, F, A, Info} <- ST
        ],
    'ok'.

log_stacktrace_mfa(M, F, Arity, Info) when is_integer(Arity) ->
    lager:error("st: ~s:~s/~b at (~b)", [M, F, Arity, props:get_value('line', Info, 0)]);
log_stacktrace_mfa(M, F, Args, Info) ->
    lager:error("st: ~s:~s at ~p", [M, F, props:get_value('line', Info, 0)]),
    _ = [lager:error("args: ~p", [Arg]) || Arg <- Args],
    'ok'.

-define(LOG_LEVELS, ['emergency'
                    ,'alert'
                    ,'critical'
                    ,'error'
                    ,'warning'
                    ,'notice'
                    ,'info'
                    ,'debug'
                    ]).
-type log_level() :: 'emergency'
                   | 'alert'
                   | 'critical'
                   | 'error'
                   | 'warning'
                   | 'notice'
                   | 'info'
                   | 'debug'
                   | ne_binary().

-spec change_console_log_level(log_level()) -> 'ok'.
change_console_log_level(L) when is_atom(L) ->
    lager:info("updated console_log to level ~s", [L]),
    lager:set_loglevel('lager_console_backend', L);
change_console_log_level(L) ->
    change_console_log_level(to_atom(L)).

-spec change_error_log_level(log_level()) -> 'ok'.
change_error_log_level(L) when is_atom(L) ->
    lager:info("updated error_log to level ~s", [L]),
    lager:set_loglevel({'lager_file_backend', "log/error.log"}, L);
change_error_log_level(L) ->
    change_error_log_level(to_atom(L)).

-spec change_syslog_log_level(log_level()) -> 'ok'.
change_syslog_log_level(L) when is_atom(L) ->
    lager:info("updated syslog_log to level ~s", [L]),
    lager:set_loglevel({'lager_syslog_backend',{"2600hz",'local0'}}, L);
change_syslog_log_level(L) ->
    change_syslog_log_level(to_atom(L)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account return it in a 'encoded',
%% unencoded or 'raw' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% Note: if given (Account, GregorianSeconds), it will return
%%   an MODb in the 'encoded' format.
%% @end
%%--------------------------------------------------------------------
-type account_format() :: 'unencoded' | 'encoded' | 'raw'.
-spec format_account_id(api_binary()) -> api_binary().
-spec format_account_id(api_binary(), account_format()) -> api_binary();
                       (api_binary(), gregorian_seconds()) -> api_binary(). %% MODb!

format_account_id(Account) ->
    format_account_id(Account, 'raw').

format_account_id('undefined', _Encoding) -> 'undefined';
format_account_id(DbName, Timestamp)
  when is_integer(Timestamp)
       andalso Timestamp > 0 ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(DbName, Year, Month);
format_account_id(<<"accounts">>, _) -> <<"accounts">>;

format_account_id(?MATCH_ACCOUNT_RAW(_)=AccountId, 'raw') ->
    AccountId;
format_account_id(?MATCH_ACCOUNT_ENCODED(_)=AccountDb, 'encoded') ->
    AccountDb;
format_account_id(?MATCH_ACCOUNT_UNENCODED(_)=AccountDbUn, 'unencoded') ->
    AccountDbUn;

format_account_id(AccountId, 'raw') ->
    raw_account_id(AccountId);
format_account_id(AccountId, 'unencoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(AccountId),
    to_binary(["account/", A, "/", B, "/", Rest]);
format_account_id(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(AccountId),
    to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

%% @private
%% Returns account_id() | any()
%% Passes input along if not account_id() | account_db() | account_db_unencoded().
-spec raw_account_id(ne_binary()) -> ne_binary().
raw_account_id(?MATCH_ACCOUNT_RAW(AccountId)) ->
    AccountId;
raw_account_id(?MATCH_ACCOUNT_UNENCODED(A, B, Rest)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_ACCOUNT_ENCODED(A, B, Rest)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_MODB_SUFFIX_RAW(AccountId, _, _)) ->
    AccountId;
raw_account_id(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, _, _)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, _, _)) ->
    ?MATCH_ACCOUNT_RAW(A, B, Rest);
raw_account_id(?MATCH_RESOURCE_SELECTORS_RAW(AccountId)) ->
    AccountId;
raw_account_id(?MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_account_id(?MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_account_id(<<"number/", _/binary>>=Other) ->
    Other;
raw_account_id(Other) ->
    case lists:member(Other, ?KZ_SYSTEM_DBS) of
        'true' -> Other;
        'false' ->
            lager:warning("raw account id doesn't process '~p'", [Other]),
            Other
    end.

%% @private
%% (modb()) -> modb_id() when modb() :: modb_id() | modb_db() | modb_db_unencoded()
%% Crashes if given anything else.
-spec raw_account_modb(ne_binary()) -> ne_binary().
raw_account_modb(?MATCH_MODB_SUFFIX_RAW(_, _, _) = AccountId) ->
    AccountId;
raw_account_modb(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month)) ->
    ?MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month);
raw_account_modb(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month)) ->
    ?MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account resource_selectors return it in a 'encoded',
%% unencoded or 'raw' format.
%% @end
%%--------------------------------------------------------------------
-spec format_resource_selectors_id(api_binary()) -> api_binary().
-spec format_resource_selectors_id(api_binary(), account_format()) -> api_binary();
                                  (api_binary(), gregorian_seconds()) -> api_binary(). %% MODb!

format_resource_selectors_id(Account) ->
    format_resource_selectors_id(Account, 'raw').

format_resource_selectors_id('undefined', _Encoding) -> 'undefined';

format_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_RAW(_)=AccountId, 'raw') ->
    AccountId;
format_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_ENCODED(_)=AccountDb, 'encoded') ->
    AccountDb;
format_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_UNENCODED(_)=AccountDbUn, 'unencoded') ->
    AccountDbUn;
format_resource_selectors_id(?MATCH_ACCOUNT_RAW(A, B, Rest), 'raw') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
format_resource_selectors_id(?MATCH_ACCOUNT_RAW(A, B, Rest), 'encoded') ->
    ?MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest);
format_resource_selectors_id(?MATCH_ACCOUNT_RAW(A, B, Rest), 'unencoded') ->
    ?MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest);

format_resource_selectors_id(AccountId, 'raw') ->
    raw_resource_selectors_id(AccountId);
format_resource_selectors_id(AccountId, 'unencoded') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A,B,Rest) = raw_resource_selectors_id(AccountId),
    to_binary(["account/", A, "/", B, "/", Rest]);
format_resource_selectors_id(AccountId, 'encoded') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A,B,Rest) = raw_resource_selectors_id(AccountId),
    to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

%% @private
%% Returns account_id() | any()
%% Passes input along if not account_id() | account_db() | account_db_unencoded().
-spec raw_resource_selectors_id(ne_binary()) -> ne_binary().
raw_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_RAW(AccountId)) ->
    AccountId;
raw_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_resource_selectors_id(?MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest)) ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest);
raw_resource_selectors_id(Other) ->
    case lists:member(Other, ?KZ_SYSTEM_DBS) of
        'true' -> Other;
        'false' ->
            lager:warning("raw account resource_selectors id doesn't process '~p'", [Other]),
            Other
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account resource_selectors return it in a 'encoded',
%% @end
%%--------------------------------------------------------------------
-spec format_resource_selectors_db(api_binary()) -> api_binary().
format_resource_selectors_db(AccountId) ->
    format_resource_selectors_id(AccountId, 'encoded').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account, build an MODb in an 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_id(api_binary(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                               api_binary().
format_account_id('undefined', _Year, _Month) -> 'undefined';
format_account_id(AccountId, Year, Month) when not is_integer(Year) ->
    format_account_id(AccountId, to_integer(Year), Month);
format_account_id(AccountId, Year, Month) when not is_integer(Month) ->
    format_account_id(AccountId, Year, to_integer(Month));
format_account_id(Account, Year, Month) when is_integer(Year),
                                             is_integer(Month) ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(Account),
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, to_binary(Year), pad_month(Month)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account, build an MODb in an 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_mod_id(api_binary()) -> api_binary().
-spec format_account_mod_id(api_binary(), gregorian_seconds() | kz_now()) -> api_binary().
-spec format_account_mod_id(api_binary(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                                   api_binary().
format_account_mod_id(Account) ->
    format_account_mod_id(Account, os:timestamp()).

format_account_mod_id(AccountId, {_,_,_}=Timestamp) ->
    {{Year, Month, _}, _} = calendar:now_to_universal_time(Timestamp),
    format_account_id(AccountId, Year, Month);
format_account_mod_id(AccountId, Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(AccountId, Year, Month).

format_account_mod_id(AccountId, Year, Month) ->
    format_account_id(AccountId, Year, Month).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an account return it in a 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_db(api_binary()) -> api_binary().
format_account_db(AccountId) ->
    format_account_id(AccountId, 'encoded').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a representation of an MODb return the MODb in the specified format.
%% Note: crashes if given anything but an MODb (in any format).
%% @end
%%--------------------------------------------------------------------
-spec format_account_modb(ne_binary()) -> ne_binary().
-spec format_account_modb(ne_binary(), account_format()) -> ne_binary().
format_account_modb(AccountId) ->
    format_account_modb(AccountId, 'raw').
format_account_modb(AccountId, 'raw') ->
    raw_account_modb(AccountId);
format_account_modb(AccountId, 'unencoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    to_binary(["account/", A, "/", B, "/", Rest]);
format_account_modb(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

-spec pad_month(kz_month() | ne_binary()) -> ne_binary().
pad_month(<<_/binary>> = Month) ->
    pad_month(to_integer(Month));
pad_month(Month) when Month < 10 ->
    <<"0", (to_binary(Month))/binary>>;
pad_month(Month) ->
    to_binary(Month).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Normalize the account name by converting the name to lower case
%% and then removing all non-alphanumeric characters.
%%
%% This can possibly return an empty binary.
%% @end
%%--------------------------------------------------------------------
-spec normalize_account_name(api_binary()) -> api_binary().
normalize_account_name('undefined') -> 'undefined';
normalize_account_name(AccountName) ->
    << <<Char>>
       || <<Char>> <= to_lower_binary(AccountName),
          is_alphanumeric(Char)
    >>.

is_alphanumeric(Char) ->
    (Char >= $a
     andalso Char =< $z
    )
        orelse (Char >= $0
                andalso Char =< $9
               ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determine if the given account id/db exists in the hierarchy of
%% the provided account id/db. Optionally consider the account in
%% its own hierarchy.
%% @end
%%--------------------------------------------------------------------
-spec is_in_account_hierarchy(api_binary(), api_binary()) -> boolean().
-spec is_in_account_hierarchy(api_binary(), api_binary(), boolean()) -> boolean().

is_in_account_hierarchy(CheckFor, InAccount) ->
    is_in_account_hierarchy(CheckFor, InAccount, 'false').

is_in_account_hierarchy('undefined', _, _) -> 'false';
is_in_account_hierarchy(_, 'undefined', _) -> 'false';
is_in_account_hierarchy(CheckFor, InAccount, IncludeSelf) ->
    CheckId = format_account_id(CheckFor),
    AccountId = format_account_id(InAccount),
    case (IncludeSelf
          andalso AccountId =:= CheckId
         )
        orelse kz_account:fetch(AccountId)
    of
        'true' ->
            lager:debug("account ~s is the same as the account to fetch the hierarchy from", [CheckId]),
            'true';
        {'ok', JObj} ->
            Tree = kz_account:tree(JObj),
            case lists:member(CheckId, Tree) of
                'true' ->
                    lager:debug("account ~s is in the account hierarchy of ~s", [CheckId, AccountId]),
                    'true';
                'false' ->
                    lager:debug("account ~s was not found in the account hierarchy of ~s", [CheckId, AccountId]),
                    'false'
            end;
        {'error', _R} ->
            lager:debug("failed to get the ancestory of the account ~s: ~p", [AccountId, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_system_admin(api_binary()) -> boolean().
is_system_admin('undefined') -> 'false';
is_system_admin(Account) ->
    case kz_account:fetch(Account) of
        {'ok', JObj} -> kz_account:is_superduper_admin(JObj);
        {'error', _R} ->
            lager:debug("unable to open account definition for ~s: ~p", [Account, _R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% checks the pvt_enabled flag and returns 'false' only if the flag is
%% specificly set to 'false'.  If it is missing or set to anything else
%% return 'true'.  However, if we cant find the account doc then return
%% 'false'.
%% @end
%%--------------------------------------------------------------------
-spec is_account_enabled(api_binary()) -> boolean().
is_account_enabled('undefined') -> 'false';
is_account_enabled(Account) ->
    case kz_account:fetch(Account) of
        {'error', _E} ->
            lager:error("could not open account ~s", [Account]),
            'false';
        {'ok', JObj} ->
            kz_account:is_enabled(JObj)
    end.

-spec is_account_expired(api_binary()) -> 'false' | {'true', gregorian_seconds()}.
is_account_expired('undefined') -> 'false';
is_account_expired(Account) ->
    case kz_account:fetch(Account) of
        {'error', _R} ->
            lager:debug("failed to check if expired token auth, ~p", [_R]),
            'false';
        {'ok', JObj} ->
            kz_account:is_expired(JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_disable_account(ne_binary()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
maybe_disable_account(Account) ->
    case is_account_enabled(Account) of
        'false' -> 'ok';
        'true' ->
            disable_account(Account)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disable_account(ne_binary()) ->
                             {'ok', kz_json:object()} |
                             {'error', any()}.
disable_account(Account) ->
    account_update(Account, fun kz_account:disable/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enable_account(ne_binary()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
enable_account(Account) ->
    account_update(Account, fun kz_account:enable/1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_superduper_admin(ne_binary(), boolean()) ->
                                  {'ok', kz_json:object()} |
                                  {'error', any()}.
set_superduper_admin(Account, IsAdmin) ->
    account_update(Account, fun(J) -> kz_account:set_superduper_admin(J, IsAdmin) end).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_allow_number_additions(ne_binary(), boolean()) ->
                                        {'ok', kz_json:object()} |
                                        {'error', any()}.
set_allow_number_additions(Account, IsAllowed) ->
    account_update(Account, fun(J) -> kz_account:set_allow_number_additions(J, IsAllowed) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_update(kz_account:doc()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
-spec account_update(ne_binary(), function()) -> 'ok' | {'error', any()}.
account_update(AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case kz_datamgr:ensure_saved(AccountDb, AccountJObj) of
        {'error', _R}=E -> E;
        {'ok', SavedJObj} ->
            kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, SavedJObj)
    end.

account_update(Account, UpdateFun) ->
    case kz_account:fetch(Account) of
        {'error', _R}=E -> E;
        {'ok', AccountJObj} ->
            account_update(UpdateFun(AccountJObj))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieves the account realm
%% @end
%%--------------------------------------------------------------------
-spec get_account_realm(api_binary()) -> api_binary().
-spec get_account_realm(api_binary(), ne_binary()) -> api_binary().
get_account_realm(Account) ->
    get_account_realm(format_account_db(Account), format_account_id(Account)).

get_account_realm('undefined', _) -> 'undefined';
get_account_realm(Db, AccountId) ->
    case kz_datamgr:open_cache_doc(Db, AccountId) of
        {'ok', JObj} -> kz_account:realm(JObj);
        {'error', _R} ->
            lager:debug("error while looking up account realm in ~s: ~p", [AccountId, _R]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a module name try to verify its existance, loading it into the
%% the vm if possible.
%% @end
%%--------------------------------------------------------------------
-spec try_load_module(string() | binary()) -> atom() | 'false'.
try_load_module(Name) ->
    Module = to_atom(Name, 'true'),
    try Module:module_info('exports') of
        _ when Module =:= 'undefined' -> 'false';
        _ ->
            {'module', Module} = code:ensure_loaded(Module),
            Module
    catch
        'error':'undef' ->
            lager:debug("module ~s not found", [Name]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure a binary is a minimum size, padding it if not with a given
%% value.
%% @end
%%--------------------------------------------------------------------
-spec pad_binary(binary(), non_neg_integer(), binary()) -> binary().
pad_binary(Bin, Size, Value) when byte_size(Bin) < Size ->
    pad_binary(<<Bin/binary, Value/binary>>, Size, Value);
pad_binary(Bin, _, _) -> Bin.

-spec pad_binary_left(binary(), non_neg_integer(), binary()) -> binary().
pad_binary_left(Bin, Size, Value) when byte_size(Bin) < Size ->
    pad_binary_left(<<Value/binary, Bin/binary>>, Size, Value);
pad_binary_left(Bin, _Size, _Value) -> Bin.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Join a binary together with a seperator.
%%
%% @end
%%--------------------------------------------------------------------
-spec join_binary([text() | atom(),...]) -> binary().
-spec join_binary([text() | atom(),...], iodata()) -> binary().

join_binary(Bins) -> join_binary(Bins, <<", ">>).
join_binary([], _) -> <<>>;
join_binary([Bin], _) -> to_binary(Bin);
join_binary([Bin|Bins], Sep) ->
    iolist_to_binary(
      [to_binary(Bin)] ++ [[Sep, to_binary(B)] || B <- Bins]
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec shuffle_list(list()) -> list().
shuffle_list([]) -> [];
shuffle_list(List) when is_list(List) ->
    Len = length(List),
    randomize_list(round(math:log(Len) + 0.5), List).

-spec randomize_list(list()) -> list().
-spec randomize_list(pos_integer(), list()) -> list().

randomize_list(List) ->
    D = lists:keysort(1, [{rand:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.

randomize_list(1, List) -> randomize_list(List);
randomize_list(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize_list(Acc)
                end, randomize_list(List), lists:seq(1, (T - 1))).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an JSON Object extracts the Call-ID into the processes
%% dictionary, failing that the Msg-ID and finally a generic
%% @end
%%--------------------------------------------------------------------
-spec put_callid(kz_json:object() | kz_proplist() | ne_binary() | atom()) ->
                        api_binary().
put_callid(?NE_BINARY = CallId) ->
    lager:md([{'callid', CallId}]),
    erlang:put('callid', CallId);
put_callid(Atom) when is_atom(Atom) ->
    lager:md([{'callid', Atom}]),
    erlang:put('callid', Atom);
put_callid(APITerm) ->
    put_callid(callid(APITerm)).

-spec get_callid() -> ne_binary().
get_callid() -> erlang:get('callid').

-spec callid(api_terms()) -> api_binary().
callid(APITerm) when is_list(APITerm) ->
    find_callid(APITerm, fun props:get_first_defined/3);
callid(APITerm) ->
    find_callid(APITerm, fun kz_json:get_first_defined/3).

-spec find_callid(api_terms(), fun()) -> api_binary().
find_callid(APITerm, GetFun) ->
    GetFun([?KEY_LOG_ID, ?KEY_API_CALL_ID, ?KEY_MSG_ID]
          ,APITerm
          ,?LOG_SYSTEM_ID
          ).

%% @public
%% @doc
%% Gives `MaxTime' milliseconds to `Fun' of `Arguments' to apply.
%% If time is elapsed, the sub-process is killed & function returns `timeout'.
%% @end
-spec runs_in(number(), fun(), list()) -> {ok, any()} | timeout.
runs_in(MaxTime, Fun, Arguments)
  when is_integer(MaxTime), MaxTime > 0 ->
    {Parent, Ref} = {self(), erlang:make_ref()},
    Child = ?MODULE:spawn(fun () -> Parent ! {Ref, erlang:apply(Fun, Arguments)} end),
    receive {Ref, Result} -> {ok, Result}
    after MaxTime ->
            exit(Child, kill),
            timeout
    end;
runs_in(MaxTime, Fun, Arguments)
  when is_number(MaxTime), MaxTime > 0 ->
    runs_in(to_integer(MaxTime), Fun, Arguments).

-spec spawn(fun(() -> any())) -> pid().
-spec spawn(fun(), list()) -> pid().
spawn(Fun, Arguments) ->
    CallId = get_callid(),
    erlang:spawn(fun() ->
                         _ = put_callid(CallId),
                         erlang:apply(Fun, Arguments)
                 end).
spawn(Fun) ->
    CallId = get_callid(),
    erlang:spawn(fun() ->
                         _ = put_callid(CallId),
                         Fun()
                 end).

-spec spawn_link(fun(() -> any())) -> pid().
-spec spawn_link(fun(), list()) -> pid().
spawn_link(Fun, Arguments) ->
    CallId = get_callid(),
    erlang:spawn_link(fun () ->
                              _ = put_callid(CallId),
                              erlang:apply(Fun, Arguments)
                      end).
spawn_link(Fun) ->
    CallId = get_callid(),
    erlang:spawn_link(fun() ->
                              _ = put_callid(CallId),
                              Fun()
                      end).

-spec spawn_monitor(fun(), list()) -> pid_ref().
spawn_monitor(Fun, Arguments) ->
    CallId = get_callid(),
    erlang:spawn_monitor(fun () ->
                                 _ = put_callid(CallId),
                                 erlang:apply(Fun, Arguments)
                         end).


-spec set_startup() -> api_seconds().
set_startup() ->
    put('$startup', current_tstamp()).

-spec startup() -> api_seconds().
startup() ->
    get('$startup').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an object, extract the category and name into a tuple
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(api_terms()) -> {api_binary(), api_binary()}.
get_event_type(Props) when is_list(Props) ->
    {props:get_value(<<"Event-Category">>, Props)
    ,props:get_value(<<"Event-Name">>, Props)
    };
get_event_type(JObj) ->
    {kz_json:get_value(<<"Event-Category">>, JObj)
    ,kz_json:get_value(<<"Event-Name">>, JObj)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generic helper to get the text value of a XML path
%% @end
%%--------------------------------------------------------------------
-spec get_xml_value(kz_deeplist(), xml_el() | string()) -> api_binary().
get_xml_value(Paths, Xml) ->
    Path = lists:flatten(Paths),
    try xmerl_xpath:string(Path, Xml) of
        Elements when is_list(Elements) -> extract_xml_values(Elements);
        _Else -> 'undefined'
    catch
        _E:_R ->
            lager:debug("~s getting value of '~s': ~p", [_E, Path, _R]),
            'undefined'
    end.

%% @private
-spec extract_xml_values(xml_els()) -> api_ne_binary().
extract_xml_values([]) -> 'undefined';
extract_xml_values(Elements) ->
    Values = [case Element of
                  #xmlText{value = Value} -> Value;
                  #xmlAttribute{value = Value} -> Value;
                  _ -> <<>> %% Important as xmerl only handles strings
              end
              || Element <- Elements
             ],
    case iolist_to_binary(Values) of
        <<>> ->
            %% Note: here we make sure that Values were all either xmlText
            %%  or xmlAttribute. Thus, if Values is a list of only empty binaries
            %%  it means that no value field was extracted.
            %% On the flip side, a "" present in Values means
            %%  that at least one extracted value was ""
            %%  and we should return <<>> instead of 'undefined'.
            IsEmptyBinary = fun (<<>>) -> 'true'; (_) -> 'false' end,
            case lists:all(IsEmptyBinary, Values) of
                'true' -> 'undefined';
                'false' -> <<>>
            end;
        Bin -> Bin
    end.

%% must be a term that can be changed to a list
-spec to_hex(binary() | string()) -> string().
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

-spec to_hex_binary(binary() | string()) -> binary().
to_hex_binary(S) ->
    Bin = to_binary(S),
    << <<(binary_to_hex_char(B div 16)), (binary_to_hex_char(B rem 16))>> || <<B>> <= Bin>>.

-spec hexencode_binary(binary()) -> binary().
hexencode_binary(<<_/binary>> = Bin) ->
    hexencode_binary(Bin, <<>>);
hexencode_binary(S) ->
    hexencode_binary(to_binary(S)).

hexencode_binary(<<>>, Acc) -> Acc;
hexencode_binary(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    hexencode_binary(Rest, <<Acc/binary
                             ,(binary_to_hex_char(Hi))
                             ,(binary_to_hex_char(Lo))
                           >>).

-spec from_hex_binary(binary()) -> binary().
from_hex_binary(Bin) ->
    to_binary(from_hex_string(to_list(Bin))).

-spec from_hex_string(list()) -> list().
-spec from_hex_string(list(), list()) -> list().
from_hex_string(Str) ->
    from_hex_string(Str, []).

from_hex_string([], Acc) -> lists:reverse(Acc);
from_hex_string([Div, Rem | T], Acc) ->
    Lo = hex_char_to_binary(Rem),
    Hi = hex_char_to_binary(Div),

    Sum = (Hi * 16) + Lo,

    from_hex_string(T, [Sum|Acc]).

-spec hex_char_to_binary(pos_integer()) -> pos_integer().
hex_char_to_binary(B) when B < 58 ->
    (to_lower_char(B) - $0);
hex_char_to_binary(B) ->
    to_lower_char(B) - ($a - 10).

-spec rand_hex_binary(pos_integer() | ne_binary()) -> ne_binary().
rand_hex_binary(Size) when not is_integer(Size) ->
    rand_hex_binary(to_integer(Size));
rand_hex_binary(Size) when is_integer(Size)
                           andalso Size > 0 ->
    to_hex_binary(rand_hex(Size)).

-spec rand_hex(pos_integer()) -> ne_binary().
rand_hex(Size) ->
    crypto:strong_rand_bytes(Size).

-spec binary_to_hex_char(pos_integer()) -> pos_integer().
binary_to_hex_char(N) when N < 10 -> $0 + N;
binary_to_hex_char(N) when N < 16 -> $a - 10 + N.

-spec uri_decode(text()) -> text().
uri_decode(Binary) when is_binary(Binary) ->
    to_binary(http_uri:decode(to_list(Binary)));
uri_decode(String) when is_list(String) ->
    http_uri:decode(String);
uri_decode(Atom) when is_atom(Atom) ->
    to_atom(http_uri:decode(to_list(Atom)), 'true').

-spec uri_encode(text()) -> text().
uri_encode(Binary) when is_binary(Binary) ->
    to_binary(http_uri:encode(to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    to_atom(http_uri:encode(to_list(Atom)), 'true').

-spec resolve_uri(nonempty_string() | api_binary(), nonempty_string() | ne_binary()) -> ne_binary().
resolve_uri(Raw, 'undefined') -> to_binary(Raw);
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(<<_/binary>> = RawPath, <<_/binary>> = Relative) ->
    join_binary(
      resolve_uri_path(RawPath, Relative)
               ,<<"/">>
     );
resolve_uri(RawPath, Relative) ->
    resolve_uri(to_binary(RawPath), to_binary(Relative)).

-spec resolve_uri_path(ne_binary(), ne_binary()) -> ne_binaries().
resolve_uri_path(RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(RawPath, <<"/">>, ['global'])),
    UrlTokens = binary:split(Relative, <<"/">>, ['global']),

    lists:reverse(
      lists:foldl(fun resolve_uri_fold/2, PathTokensRev, UrlTokens)
     ).

-spec resolve_uri_fold(ne_binary(), ne_binaries()) -> ne_binaries().
resolve_uri_fold(<<"..">>, []) -> [];
resolve_uri_fold(<<"..">>, [_ | PathTokens]) -> PathTokens;
resolve_uri_fold(<<".">>, PathTokens) -> PathTokens;
resolve_uri_fold(<<>>, PathTokens) -> PathTokens;
resolve_uri_fold(Segment, [<<>>|DirTokens]) ->
    [Segment|DirTokens];
resolve_uri_fold(Segment, [LastToken|DirTokens]=PathTokens) ->
    case filename:extension(LastToken) of
        <<>> ->
            %% no extension, append Segment to Tokens
            [Segment | PathTokens];
        _Ext ->
            %% Extension found, append Segment to DirTokens
            [Segment|DirTokens]
    end.

-spec uri(ne_binary(), ne_binaries()) -> ne_binary().
uri(BaseUrl, Tokens) ->
    [Pro, Url] = binary:split(BaseUrl, <<"://">>),
    Uri = filename:join([Url | Tokens]),
    <<Pro/binary, "://", Uri/binary>>.


-spec safe_urlencode(binary() | number()) -> binary().
safe_urlencode(V) when is_binary(V)
                       orelse is_number(V) ->
    kz_http_util:urlencode(to_binary(V)).

-spec to_integer(string() | binary() | integer() | float()) -> integer().
-spec to_integer(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X) -> to_integer(X, 'notstrict').

to_integer(X, 'strict') when is_float(X) -> erlang:error('badarg');
to_integer(X, 'notstrict') when is_float(X) -> round(X);
to_integer(X, S) when is_binary(X) -> to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
    try list_to_integer(X)
    catch
        'error':'badarg' when S =:= 'notstrict' ->
            round(list_to_float(X))
    end;
to_integer(X, _) when is_integer(X) ->
    X.

-spec to_float(string() | binary() | integer() | float()) -> float().
-spec to_float(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> float().
to_float(X) -> to_float(X, 'notstrict').

to_float(X, S) when is_binary(X) -> to_float(binary_to_list(X), S);
to_float(X, S) when is_list(X) ->
    try list_to_float(X)
    catch
        'error':'badarg' when S =:= 'notstrict' -> list_to_integer(X)*1.0 %% "500" -> 500.0
    end;
to_float(X, 'strict') when is_integer(X) -> erlang:error('badarg');
to_float(X, 'notstrict') when is_integer(X) -> X * 1.0;
to_float(X, _) when is_float(X) -> X.

-spec to_number(binary() | string() | number()) -> number().
to_number(X) when is_number(X) -> X;
to_number(X) when is_binary(X) -> to_number(to_list(X));
to_number(X) when is_list(X) ->
    try list_to_integer(X)
    catch
        'error':'badarg' -> list_to_float(X)
    end.

-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_float(X) -> kz_mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> binary().
to_binary(X) when is_float(X) -> to_binary(kz_mochinum:digits(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_pid(X) -> to_binary(pid_to_list(X));
to_binary(X) when is_binary(X) -> X.

%% the safer version, won't let you leak atoms
-spec to_atom(atom() | list() | binary() | integer() | float()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_existing_atom(X);
to_atom(X) -> to_atom(to_list(X)).

%% only if you're really sure you want this
%% to protect yourself a bit from overrunning the atom table,
%% pass a list of safe values for X
%% so if X is a binary, the SafeList would be [ne_binary(),...]
%% if X is a list, the SafeList would be [nonempty_string(),...]
%% etc. So to_atom will not coerce the type of X to match the types in SafeList
%% when doing the lists:member/2
-spec to_atom(atom() | list() | binary() | integer() | float(), 'true' | list()) -> atom().
to_atom(X, _) when is_atom(X) -> X;
to_atom(X, 'true') when is_list(X) -> list_to_atom(X);
to_atom(X, 'true') -> to_atom(to_list(X), 'true');
to_atom(X, 'false') -> to_atom(X);
to_atom(X, SafeList) when is_list(SafeList) ->
    to_atom(to_list(X), lists:member(X, SafeList)).

-spec to_boolean(binary() | string() | atom()) -> boolean().
to_boolean(<<"true">>) -> 'true';
to_boolean("true") -> 'true';
to_boolean('true') -> 'true';
to_boolean(<<"false">>) -> 'false';
to_boolean("false") -> 'false';
to_boolean('false') -> 'false'.

-spec to_date(binary() | string() | integer()) -> kz_date().
to_date(X) ->
    {Date, _ } = to_datetime(X),
    Date.

-spec to_datetime(binary() | string() | integer()) -> kz_datetime().
to_datetime(X) when is_integer(X) -> calendar:gregorian_seconds_to_datetime(X);
to_datetime(X) when is_binary(X) -> to_datetime(to_integer(X));
to_datetime(X) when is_list(X) -> to_datetime(to_integer(X)).

-spec error_to_binary({'error', binary()} | binary()) -> binary().
error_to_binary({'error', Reason}) ->
    error_to_binary(Reason);
error_to_binary(Reason) ->
    try to_binary(Reason)
    catch
        _:_ -> <<"Unknown Error">>
    end.

-spec is_true(binary() | string() | atom()) -> boolean().
is_true(<<"true">>) -> 'true';
is_true("true") -> 'true';
is_true('true') -> 'true';
is_true(_) -> 'false'.

-spec always_true(any()) -> 'true'.
always_true(_) -> 'true'.

-spec is_false(binary() | string() | atom()) -> boolean().
is_false(<<"false">>) -> 'true';
is_false("false") -> 'true';
is_false('false') -> 'true';
is_false(_) -> 'false'.

-spec always_false(any()) -> 'false'.
always_false(_) -> 'false'.

-spec is_ne_binary(binary()) -> boolean().
is_ne_binary(V) ->
    is_binary(V)
        andalso is_not_empty(V).

-spec is_boolean(binary() | string() | atom()) -> boolean().
is_boolean(<<"true">>) -> 'true';
is_boolean("true") -> 'true';
is_boolean('true') -> 'true';
is_boolean(<<"false">>) -> 'true';
is_boolean("false") -> 'true';
is_boolean('false') -> 'true';
is_boolean(_) -> 'false'.

-spec is_empty(any()) -> boolean().
is_empty(0) -> 'true';
is_empty([]) -> 'true';
is_empty("0") -> 'true';
is_empty("false") -> 'true';
is_empty("NULL") -> 'true';
is_empty("undefined") -> 'true';
is_empty(<<>>) -> 'true';
is_empty(<<"0">>) -> 'true';
is_empty(<<"false">>) -> 'true';
is_empty(<<"NULL">>) -> 'true';
is_empty(<<"undefined">>) -> 'true';
is_empty('null') -> 'true';
is_empty('false') -> 'true';
is_empty('undefined') -> 'true';
is_empty(Float) when is_float(Float), Float =:= 0.0 -> 'true';
is_empty(MaybeJObj) ->
    case kz_json:is_json_object(MaybeJObj) of
        'false' -> 'false'; %% if not a json object, it's not empty
        'true' -> kz_json:is_empty(MaybeJObj)
    end.

-spec is_not_empty(any()) -> boolean().
is_not_empty(Term) -> (not is_empty(Term)).

-spec is_proplist(any()) -> boolean().
is_proplist(Term) when is_list(Term) ->
    lists:all(fun({_,_}) -> 'true'; (A) -> is_atom(A) end, Term);
is_proplist(_) -> 'false'.

-spec identity(X) -> X.
identity(X) -> X.

-spec to_lower_binary(any()) -> api_binary().
to_lower_binary('undefined') -> 'undefined';
to_lower_binary(Bin) when is_binary(Bin) -> << <<(to_lower_char(B))>> || <<B>> <= Bin>>;
to_lower_binary(Else) -> to_lower_binary(to_binary(Else)).

-spec to_lower_string(any()) -> 'undefined' | list().
to_lower_string('undefined') -> 'undefined';
to_lower_string(L) when is_list(L) ->
    [to_lower_char(C) || C <- L];
to_lower_string(Else) ->
    to_lower_string(to_list(Else)).

-spec ucfirst_binary(ne_binary()) -> ne_binary().
ucfirst_binary(<<F:8, Bin/binary>>) -> <<(to_upper_char(F)):8, Bin/binary>>.

-spec lcfirst_binary(ne_binary()) -> ne_binary().
lcfirst_binary(<<F:8, Bin/binary>>) -> <<(to_lower_char(F)):8, Bin/binary>>.

-spec to_lower_char(char()) -> char().
to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
to_lower_char(C) -> C.

-spec to_upper_binary(any()) -> api_binary().
to_upper_binary('undefined') -> 'undefined';
to_upper_binary(Bin) when is_binary(Bin) -> << <<(to_upper_char(B))>> || <<B>> <= Bin>>;
to_upper_binary(Else) -> to_upper_binary(to_binary(Else)).

-spec to_upper_string(any()) -> 'undefined' | list().
to_upper_string('undefined') -> 'undefined';
to_upper_string(L) when is_list(L) -> [to_upper_char(C) || C <- L];
to_upper_string(Else) -> to_upper_string(to_list(Else)).

-spec to_upper_char(char()) -> char().
to_upper_char(C) when is_integer(C), $a =< C, C =< $z -> C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> C - 32;
to_upper_char(C) -> C.

-spec strip_binary(binary()) -> binary().
-spec strip_binary(binary(), 'both' | 'left' | 'right' | char() | [char()]) -> binary().
-spec strip_left_binary(binary(), char() | binary()) -> binary().
-spec strip_right_binary(binary(), char() | binary()) -> binary().
strip_binary(B) -> strip_binary(B, 'both').

strip_binary(B, 'left') -> strip_left_binary(B, $\s);
strip_binary(B, 'right') -> strip_right_binary(B, $\s);
strip_binary(B, 'both') -> strip_right_binary(strip_left_binary(B, $\s), $\s);
strip_binary(B, C) when is_integer(C) -> strip_right_binary(strip_left_binary(B, C), C);
strip_binary(B, Cs) when is_list(Cs) ->
    lists:foldl(fun(C, Acc) -> strip_binary(Acc, C) end
               ,B
               ,Cs
               ).

strip_left_binary(<<C, B/binary>>, C) -> strip_left_binary(B, C);
strip_left_binary(B, _) -> B.

strip_right_binary(C, C) -> <<>>;
strip_right_binary(<<C, B/binary>>, C) ->
    case strip_right_binary(B, C) of
        <<>> -> <<>>;
        T -> <<C, T/binary>>
    end;
strip_right_binary(<<A, B/binary>>, C) ->
    <<A, (strip_right_binary(B, C))/binary>>;
strip_right_binary(<<>>, _) -> <<>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure a binary is a maximum given size, truncating it if not.
%% @end
%%--------------------------------------------------------------------
-spec truncate_binary(binary(), non_neg_integer()) -> binary().
-spec truncate_binary(binary(), non_neg_integer(), 'left' | 'right') -> binary().
truncate_binary(Bin, Size) ->
    truncate_binary(Bin, Size, 'right').

truncate_binary(Bin, Size, 'left') ->
    truncate_left_binary(Bin, Size);
truncate_binary(Bin, Size, 'right') ->
    truncate_right_binary(Bin, Size).

-spec truncate_left_binary(binary(), non_neg_integer()) -> binary().
truncate_left_binary(Bin, Size) when byte_size(Bin) > Size ->
    binary:part(Bin, {byte_size(Bin), -Size});
truncate_left_binary(Bin, _) ->
    Bin.

-spec truncate_right_binary(binary(), non_neg_integer()) -> binary().
truncate_right_binary(Bin, Size) when byte_size(Bin) > Size ->
    binary:part(Bin, {0, Size});
truncate_right_binary(Bin, _) ->
    Bin.

-spec suffix_binary(binary(), binary()) -> boolean().
suffix_binary(<<>>, _Bin) -> 'false';
suffix_binary(<<_/binary>> = Suffix, <<_/binary>> = Bin) ->
    try truncate_left_binary(Bin, byte_size(Suffix)) =:= Suffix
    catch
        _:_ -> 'false'
    end.

-spec clean_binary(binary()) -> binary().
-spec clean_binary(binary(), kz_proplist()) -> binary().
clean_binary(Bin) ->
    clean_binary(Bin, []).

clean_binary(Bin, Opts) ->
    Routines = [fun remove_white_spaces/2],
    lists:foldl(fun(F, B) -> F(B, Opts) end, Bin, Routines).

-spec remove_white_spaces(binary(), kz_proplist()) -> binary().
remove_white_spaces(Bin, Opts) ->
    case props:get_value(<<"remove_white_spaces">>, Opts, 'true') of
        'false' -> Bin;
        'true' -> remove_white_spaces(Bin)
    end.

-spec remove_white_spaces(binary()) -> binary().
remove_white_spaces(Bin) ->
    << <<X>> || <<X>> <= Bin, X =/= $\s >>.

-spec binary_md5(text()) -> ne_binary().
binary_md5(Text) -> to_hex_binary(erlang:md5(to_binary(Text))).

-spec a1hash(ne_binary(), ne_binary(), ne_binary()) -> nonempty_string().
a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

%% found via trapexit
-spec floor(integer() | float()) -> integer().
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        'true' -> T;
        'false' -> T - 1
    end;
floor(X) -> trunc(X).

%% found via trapexit
-spec ceiling(integer() | float()) -> integer().
ceiling(X) when X < 0 -> trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        'true' -> T;
        'false' -> T + 1
    end.

%% returns current seconds
-spec current_tstamp() -> gregorian_seconds().
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-spec current_unix_tstamp() -> unix_seconds().
current_unix_tstamp() ->
    gregorian_seconds_to_unix_seconds(current_tstamp()).

%% fetch and cache the kazoo version from the VERSION file in kazoo's root folder
-spec kazoo_version() -> ne_binary().
kazoo_version() ->
    {_, _, Version} = get_app('kazoo'),
    to_binary(Version).

-spec write_pid(file:filename_all()) -> 'ok' | {'error', atom()}.
write_pid(FileName) ->
    file:write_file(FileName, io_lib:format("~s", [os:getpid()]), ['write', 'binary']).

-spec gregorian_seconds_to_unix_seconds(integer() | string() | binary()) -> integer().
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    to_integer(GregorianSeconds) - ?UNIX_EPOCH_IN_GREGORIAN.

-spec unix_seconds_to_gregorian_seconds(integer() | string() | binary()) -> integer().
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    to_integer(UnixSeconds) + ?UNIX_EPOCH_IN_GREGORIAN.

-spec unix_timestamp_to_gregorian_seconds(integer() | string() | binary()) -> integer().
unix_timestamp_to_gregorian_seconds(UnixTimestamp) ->
    ?UNIX_EPOCH_IN_GREGORIAN + (to_integer(UnixTimestamp) div 1000).

-spec pretty_print_datetime(kz_datetime() | integer()) -> ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w-~2..0w-~2..0w"
                                  ,[Y, Mo, D, H, Mi, S]
                                  )).

-spec rfc1036(calendar:datetime() | gregorian_seconds()) -> ne_binary().
-spec rfc1036(calendar:datetime() | gregorian_seconds(), ne_binary()) -> ne_binary().
rfc1036(DateTime) ->
    rfc1036(DateTime, <<"GMT">>).

rfc1036({Date = {Y, Mo, D}, {H, Mi, S}}, TZ) ->
    Wday = calendar:day_of_the_week(Date),
    <<(weekday(Wday))/binary, ", ",
      (pad_binary_left(to_binary(D), 2, <<"0">>))/binary, " ",
      (month(Mo))/binary, " ",
      (to_binary(Y))/binary, " ",
      (pad_binary_left(to_binary(H), 2, <<"0">>))/binary, ":",
      (pad_binary_left(to_binary(Mi), 2, <<"0">>))/binary, ":",
      (pad_binary_left(to_binary(S), 2, <<"0">>))/binary,
      " ", TZ/binary
    >>;
rfc1036(Timestamp, TZ) when is_integer(Timestamp) ->
    rfc1036(calendar:gregorian_seconds_to_datetime(Timestamp), TZ).

-spec iso8601(calendar:datetime() | gregorian_seconds()) -> ne_binary().
iso8601({{Y,M,D},_}) ->
    <<(to_binary(Y))/binary, "-"
      ,(pad_binary_left(to_binary(M), 2, <<"0">>))/binary, "-"
      ,(pad_binary_left(to_binary(D), 2, <<"0">>))/binary
    >>;
iso8601(Timestamp) when is_integer(Timestamp) ->
    iso8601(calendar:gregorian_seconds_to_datetime(Timestamp)).

%% borrowed from cow_date.erl
-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

-spec pretty_print_elapsed_s(non_neg_integer()) -> ne_binary().
pretty_print_elapsed_s(0) -> <<"0s">>;
pretty_print_elapsed_s(Seconds) ->
    iolist_to_binary(unitfy_seconds(Seconds)).

-spec unitfy_seconds(non_neg_integer()) -> iolist().
unitfy_seconds(0) -> "";
unitfy_seconds(Seconds) when Seconds < ?SECONDS_IN_MINUTE ->
    [to_binary(Seconds), "s"];
unitfy_seconds(Seconds) when Seconds < ?SECONDS_IN_HOUR ->
    M = Seconds div ?SECONDS_IN_MINUTE,
    [to_binary(M), "m", unitfy_seconds(Seconds - (M * ?SECONDS_IN_MINUTE))];
unitfy_seconds(Seconds) when Seconds < ?SECONDS_IN_DAY ->
    H = Seconds div ?SECONDS_IN_HOUR,
    [to_binary(H), "h", unitfy_seconds(Seconds - (H * ?SECONDS_IN_HOUR))];
unitfy_seconds(Seconds) ->
    D = Seconds div ?SECONDS_IN_DAY,
    [to_binary(D), "d", unitfy_seconds(Seconds - (D * ?SECONDS_IN_DAY))].

-spec pretty_print_bytes(non_neg_integer()) -> ne_binary().
-spec pretty_print_bytes(non_neg_integer(), 'full' | 'truncated') -> ne_binary().
pretty_print_bytes(Bytes) ->
    pretty_print_bytes(Bytes, 'full').

pretty_print_bytes(0, _) -> <<"0B">>;
pretty_print_bytes(Bytes, Type) ->
    iolist_to_binary(unitfy_bytes(Bytes, Type)).

-spec unitfy_bytes(non_neg_integer(), 'full' | 'truncated') -> iolist().
unitfy_bytes(0, _Type) -> "";
unitfy_bytes(Bytes, _Type) when Bytes < ?BYTES_K  ->
    [to_binary(Bytes), "B"];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_M ->
    K = Bytes div ?BYTES_K,
    [to_binary(K), "K", maybe_unitfy_bytes(Bytes rem ?BYTES_K, Type)];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_G ->
    M = Bytes div ?BYTES_M,
    [to_binary(M), "M", maybe_unitfy_bytes(Bytes rem ?BYTES_M, Type)];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_T ->
    G = Bytes div ?BYTES_G,
    [to_binary(G), "G", maybe_unitfy_bytes(Bytes rem ?BYTES_G, Type)];
unitfy_bytes(Bytes, Type) ->
    T = Bytes div ?BYTES_T,
    [to_binary(T), "T", maybe_unitfy_bytes(Bytes rem ?BYTES_T, Type)].

-spec maybe_unitfy_bytes(non_neg_integer(), 'full' | 'truncated') -> iolist().
maybe_unitfy_bytes(Bytes, 'full'=Type) ->
    unitfy_bytes(Bytes, Type);
maybe_unitfy_bytes(_Bytes, 'truncated') ->
    <<>>.

-spec decr_timeout(kz_timeout(), non_neg_integer() | kz_now()) -> kz_timeout().
decr_timeout('infinity', _) -> 'infinity';
decr_timeout(Timeout, Elapsed) when is_integer(Elapsed) ->
    Diff = Timeout - Elapsed,
    case Diff < 0 of
        'true' -> 0;
        'false' -> Diff
    end;
decr_timeout(Timeout, Start) ->
    decr_timeout(Timeout, elapsed_ms(Start)).

-spec microseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
-spec milliseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
microseconds_to_seconds(Microseconds) -> to_integer(Microseconds) div 1000000.
milliseconds_to_seconds(Milliseconds) -> to_integer(Milliseconds) div ?MILLISECONDS_IN_SECOND.

-spec elapsed_s(kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_ms(kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_us(kz_now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start) -> elapsed_s(Start, os:timestamp());
elapsed_s(Start) when is_integer(Start) -> elapsed_s(Start, current_tstamp()).

elapsed_ms({_,_,_}=Start) -> elapsed_ms(Start, os:timestamp());
elapsed_ms(Start) when is_integer(Start) -> elapsed_ms(Start, current_tstamp()).

elapsed_us({_,_,_}=Start) -> elapsed_us(Start, os:timestamp());
elapsed_us(Start) when is_integer(Start) -> elapsed_us(Start, current_tstamp()).

-spec elapsed_s(kz_now() | pos_integer(), kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_ms(kz_now() | pos_integer(), kz_now() | pos_integer()) -> pos_integer().
-spec elapsed_us(kz_now() | pos_integer(), kz_now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start, {_,_,_}=Now) -> timer:now_diff(Now, Start) div 1000000;
elapsed_s({_,_,_}=Start, Now) -> elapsed_s(now_s(Start), Now);
elapsed_s(Start, {_,_,_}=Now) -> elapsed_s(Start, now_s(Now));
elapsed_s(Start, Now) when is_integer(Start), is_integer(Now) -> Now - Start.

elapsed_ms({_,_,_}=Start, {_,_,_}=Now) -> timer:now_diff(Now, Start) div ?MILLISECONDS_IN_SECOND;
elapsed_ms({_,_,_}=Start, Now) -> elapsed_ms(now_s(Start), Now);
elapsed_ms(Start, {_,_,_}=Now) -> elapsed_ms(Start, now_s(Now));
elapsed_ms(Start, Now)
  when is_integer(Start),
       is_integer(Now) ->
    (Now - Start) * ?MILLISECONDS_IN_SECOND.

elapsed_us({_,_,_}=Start, {_,_,_}=Now) -> timer:now_diff(Now, Start);
elapsed_us({_,_,_}=Start, Now) -> elapsed_us(now_s(Start), Now);
elapsed_us(Start, {_,_,_}=Now) -> elapsed_us(Start, now_s(Now));
elapsed_us(Start, Now) when is_integer(Start), is_integer(Now) -> (Now - Start) * 1000000.

-spec now() -> kz_now().
now() -> erlang:timestamp().

-spec now_s() -> gregorian_seconds().
-spec now_ms() -> pos_integer().
-spec now_us() -> pos_integer().

now_s() ->  erlang:system_time('seconds').
now_ms() -> erlang:system_time('milli_seconds').
now_us() -> erlang:system_time('micro_seconds').

-spec now_s(kz_now()) -> gregorian_seconds().
-spec now_ms(kz_now()) -> pos_integer().
-spec now_us(kz_now()) -> pos_integer().
now_us({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.
now_ms({_,_,_}=Now) -> now_us(Now) div ?MILLISECONDS_IN_SECOND.
now_s({_,_,_}=Now) -> unix_seconds_to_gregorian_seconds(now_us(Now) div 1000000).

-spec format_date() -> binary().
-spec format_date(gregorian_seconds()) -> binary().
-spec format_time() -> binary().
-spec format_time(gregorian_seconds()) -> binary().
-spec format_datetime() -> binary().
-spec format_datetime(gregorian_seconds()) -> binary().

format_date() ->
    format_date(current_tstamp()).

format_date(Timestamp) ->
    {{Y,M,D}, _ } = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([to_binary(Y), "-", to_binary(M), "-", to_binary(D)]).

format_time() ->
    format_time(current_tstamp()).

format_time(Timestamp) ->
    { _, {H,I,S}} = calendar:gregorian_seconds_to_datetime(Timestamp),
    list_to_binary([to_binary(H), ":", to_binary(I), ":", to_binary(S)]).

format_datetime() ->
    format_datetime(current_tstamp()).

format_datetime(Timestamp) ->
    list_to_binary([format_date(Timestamp), " ", format_time(Timestamp)]).

-spec bin_usage() -> integer().
bin_usage() ->
    {'ok', {_, Usage, _}} = recon_lib:proc_attrs(binary_memory, self()),
    Usage.

-spec mem_usage() -> integer().
mem_usage() ->
    {'memory', Memory} = erlang:process_info(self(), 'memory'),
    Memory.

-spec node_name() -> binary().
-spec node_hostname() -> binary().
node_name() ->
    [Name, _Host] = binary:split(to_binary(node()), <<"@">>),
    Name.
node_hostname() ->
    [_Name, Host] = binary:split(to_binary(node()), <<"@">>),
    Host.


%% @public
-spec write_file(file:filename_all(), iodata()) -> 'ok'.
write_file(Filename, Bytes) ->
    write_file(Filename, Bytes, []).

%% @public
-spec write_file(file:filename_all(), iodata(), [file:mode()]) -> 'ok'.
write_file(Filename, Bytes, Modes) ->
    case file:write_file(Filename, Bytes, Modes) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("writing file ~s (~p) failed : ~p", [Filename, Modes, _E])
    end.

-spec rename_file(file:filename_all(), file:filename_all()) -> 'ok'.
rename_file(FromFilename, ToFilename) ->
    case file:rename(FromFilename, ToFilename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("moving file ~s into ~s failed : ~p", [FromFilename, ToFilename, _E])
    end.

%% @public
-spec delete_file(file:filename_all()) -> 'ok'.
delete_file(Filename) ->
    case file:delete(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("deleting file ~s failed : ~p", [Filename, _E])
    end.

%% @public
-spec make_dir(file:filename_all()) -> 'ok'.
make_dir(Filename) ->
    case file:make_dir(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("creating directory ~s failed : ~p", [Filename, _E])
    end.

-spec anonymous_caller_id_name() -> ne_binary().
anonymous_caller_id_name() ->
    <<"anonymous">>.

-spec anonymous_caller_id_number() -> ne_binary().
anonymous_caller_id_number() ->
    <<"0000000000">>.

%% for core apps that want to know which app is calling

-spec process_fold([tuple()], atom()) -> tuple() | atom().
process_fold([], App) -> App;
process_fold([{M, _, _, _}=Mod | Others], App) ->
    ModApp = case application:get_application(M) of
                 {'ok', KModApp} -> KModApp;
                 'undefined' -> M
             end,
    process_fold(ModApp, App, Mod, Others).

-spec process_fold(atom(), atom(), tuple(), [tuple()]) -> tuple() | atom().
process_fold(App, App, _, Others) ->
    process_fold(Others, App);
process_fold(App, _, M, _) -> {App, M}.

-spec calling_app() -> ne_binary().
calling_app() ->
    Modules = erlang:process_info(self(),current_stacktrace),
    {'current_stacktrace', [_Me, {Module, _, _, _} | Start]} = Modules,
    {'ok', App} = application:get_application(Module),
    case process_fold(Start, App) of
        App -> to_binary(App);
        {Parent, _MFA} -> to_binary(Parent)
    end.

-spec calling_app_version() -> {ne_binary(), ne_binary()}.
calling_app_version() ->
    Modules = erlang:process_info(self(),current_stacktrace),
    {'current_stacktrace', [_Me, {Module, _, _, _} | Start]} = Modules,
    {'ok', App} = application:get_application(Module),
    NewApp = case process_fold(Start, App) of
                 App -> App;
                 {Parent, _MFA} -> Parent
             end,
    {NewApp, _, Version} = get_app(NewApp),
    {to_binary(NewApp), to_binary(Version)}.

-spec calling_process() -> map().
calling_process() ->
    Modules = erlang:process_info(self(),current_stacktrace),
    {'current_stacktrace', [_Me, {Module, _, _, _}=M | Start]} = Modules,
    App = case application:get_application(Module) of
              {'ok', KApp} -> KApp;
              'undefined' -> Module
          end,
    {NewApp, {Mod, Function, Arity, [{file, Filename}, {line, Line}]}} =
        case process_fold(Start, App) of
            App -> {App, M};
            {Parent, MFA } -> {Parent, MFA}
        end,
    #{app => NewApp
     ,module => Mod
     ,function => Function
     ,arity => Arity
     ,file => Filename
     ,line => Line
     }.

-spec get_app(atom() | ne_binary()) -> {atom(), string(), string()} | 'undefined'.
get_app(<<_/binary>> = AppName) ->
    get_app(to_atom(AppName));
get_app(AppName) ->
    case [App || {Name, _, _}=App <- application:loaded_applications(), Name =:= AppName] of
        [] -> 'undefined';
        [Ret | _] -> Ret
    end.

-spec application_version(atom()) -> ne_binary().
application_version(Application) ->
    {'ok', Vsn} = application:get_key(Application, 'vsn'),
    to_binary(Vsn).


%% @public
-spec iolist_join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: iodata() | char().
iolist_join(_, []) -> [];
iolist_join(Sep, [H|T]) ->
    [H | iolist_join_prepend(Sep, T)].

%% @private
-spec iolist_join_prepend(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: iolist().
iolist_join_prepend(_, []) -> [];
iolist_join_prepend(Sep, [H|T]) ->
    [Sep, H | iolist_join_prepend(Sep, T)].


-spec binary_reverse(binary()) -> binary().
binary_reverse(Bin) ->
    to_binary(lists:reverse(to_list(Bin))).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

resolve_uri_test_() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    RawPathList = [<<"http:">>, <<>>, <<"pivot">>, <<"script2.php">>],

    [?_assertEqual(RawPathList, resolve_uri_path(RawPath, Relative))
    ,?_assertEqual(RawPathList, resolve_uri_path(RawPath, <<"/", Relative/binary>>))
    ].

-endif.
