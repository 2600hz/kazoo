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
         ,normalize_account_name/1
         ,account_update/1, account_update/2
        ]).
-export([is_in_account_hierarchy/2, is_in_account_hierarchy/3]).
-export([is_system_admin/1
         ,is_system_db/1
        ]).
-export([get_account_realm/1, get_account_realm/2]).
-export([is_account_enabled/1, is_account_expired/1]).
-export([maybe_disable_account/1
         ,disable_account/1
         ,enable_account/1
         ,set_superduper_admin/2
         ,set_allow_number_additions/2
        ]).

-export([try_load_module/1]).

-export([uri_encode/1
         ,uri_decode/1
         ,resolve_uri/2
         ,safe_urlencode/1
         ,normalize_amqp_uri/1
        ]).

-export([uri/2]).

-export([error_to_binary/1]).
-export([a1hash/3]).

-export([pretty_print_bytes/1
         ,bin_usage/0, mem_usage/0
        ]).

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

-export([node_name/0, node_hostname/0]).

-export([anonymous_caller_id_name/0
         ,anonymous_caller_id_number/0
        ]).

-export([write_file/2, write_file/3
         ,delete_file/1
         ,make_dir/1
        ]).

-export([calling_app/0]).
-export([calling_app_version/0]).
-export([calling_process/0]).
-export([get_app/1]).

-include_lib("kernel/include/inet.hrl").

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_api.hrl").

-define(KAZOO_VERSION_CACHE_KEY, {?MODULE, 'kazoo_version'}).

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
    change_console_log_level(kz_term:to_atom(L)).

-spec change_error_log_level(log_level()) -> 'ok'.
change_error_log_level(L) when is_atom(L) ->
    lager:info("updated error_log to level ~s", [L]),
    lager:set_loglevel({'lager_file_backend', "log/error.log"}, L);
change_error_log_level(L) ->
    change_error_log_level(kz_term:to_atom(L)).

-spec change_syslog_log_level(log_level()) -> 'ok'.
change_syslog_log_level(L) when is_atom(L) ->
    lager:info("updated syslog_log to level ~s", [L]),
    lager:set_loglevel({'lager_syslog_backend',{"2600hz",'local0'}}, L);
change_syslog_log_level(L) ->
    change_syslog_log_level(kz_term:to_atom(L)).

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
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_account_id(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

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
%% Given a representation of an account, build an MODb in an 'encoded' format.
%% Note: accepts MODbs as well as account IDs/DBs
%% @end
%%--------------------------------------------------------------------
-spec format_account_id(api_binary(), kz_year() | ne_binary(), kz_month() | ne_binary()) ->
                               api_binary().
format_account_id('undefined', _Year, _Month) -> 'undefined';
format_account_id(AccountId, Year, Month) when not is_integer(Year) ->
    format_account_id(AccountId, kz_term:to_integer(Year), Month);
format_account_id(AccountId, Year, Month) when not is_integer(Month) ->
    format_account_id(AccountId, Year, kz_term:to_integer(Month));
format_account_id(Account, Year, Month) when is_integer(Year),
                                             is_integer(Month) ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(Account),
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, kz_term:to_binary(Year), kz_time:pad_month(Month)).

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
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_account_modb(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

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
       || <<Char>> <= kz_term:to_lower_binary(AccountName),
          (Char >= $a andalso Char =< $z)
              or (Char >= $0 andalso Char =< $9)
    >>.

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
    case (IncludeSelf andalso AccountId =:= CheckId)
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

-spec is_system_db(ne_binary()) -> boolean().
is_system_db(Db) ->
    lists:member(Db, ?KZ_SYSTEM_DBS).

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
    Module = kz_term:to_atom(Name, 'true'),
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
%% Given an JSON Object extracts the Call-ID into the processes
%% dictionary, failing that the Msg-ID and finally a generic
%% @end
%%--------------------------------------------------------------------
-spec put_callid(kz_json:object() | kz_proplist() | ne_binary() | atom()) ->
                        api_binary().
put_callid(?NE_BINARY = CallId) ->
    lager:md([{'callid', CallId}]), erlang:put('callid', CallId);
put_callid(Atom) when is_atom(Atom) ->
    lager:md([{'callid', Atom}]), erlang:put('callid', Atom);
put_callid(Prop) when is_list(Prop) ->
    lager:md([{'callid', callid(Prop)}]), erlang:put('callid', callid(Prop));
put_callid(JObj) ->
    lager:md([{'callid', callid(JObj)}]), erlang:put('callid', callid(JObj)).

-spec get_callid() -> ne_binary().
get_callid() -> erlang:get('callid').

callid(Prop) when is_list(Prop) ->
    props:get_first_defined([?KEY_LOG_ID, <<"Call-ID">>, ?KEY_MSG_ID], Prop, ?LOG_SYSTEM_ID);
callid(JObj) ->
    kz_json:get_first_defined([?KEY_LOG_ID, <<"Call-ID">>, ?KEY_MSG_ID], JObj, ?LOG_SYSTEM_ID).

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
    put('$startup', kz_time:current_tstamp()).

-spec startup() -> api_seconds().
startup() ->
    get('$startup').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an object, extract the category and name into a tuple
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(kz_json:object() | kz_proplist()) -> {api_binary(), api_binary()}.
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
-spec extract_xml_values(xml_els()) -> api_binary().
extract_xml_values([]) -> 'undefined';
extract_xml_values(Elements) ->
    Values = [case Element of
                  #xmlText{value = Value} -> Value;
                  #xmlAttribute{value = Value} -> Value;
                  _ -> <<>> %% Important as xmerl only handles strings
              end
              || Element <- Elements],
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

-spec uri_decode(text()) -> text().
uri_decode(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:decode(kz_term:to_list(Binary)));
uri_decode(String) when is_list(String) ->
    http_uri:decode(String);
uri_decode(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:decode(kz_term:to_list(Atom)), 'true').

-spec uri_encode(text()) -> text().
uri_encode(Binary) when is_binary(Binary) ->
    kz_term:to_binary(http_uri:encode(kz_term:to_list(Binary)));
uri_encode(String) when is_list(String) ->
    http_uri:encode(String);
uri_encode(Atom) when is_atom(Atom) ->
    kz_term:to_atom(http_uri:encode(kz_term:to_list(Atom)), 'true').

-spec resolve_uri(nonempty_string() | api_binary(), nonempty_string() | ne_binary()) -> ne_binary().
resolve_uri(Raw, 'undefined') -> kz_term:to_binary(Raw);
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(<<_/binary>> = RawPath, <<_/binary>> = Relative) ->
    kz_term:join_binary(
      resolve_uri_path(RawPath, Relative)
      ,<<"/">>
     );
resolve_uri(RawPath, Relative) ->
    resolve_uri(kz_term:to_binary(RawPath), kz_term:to_binary(Relative)).

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


-spec safe_urlencode(binary() | number()) -> iolist().
safe_urlencode(V) when is_binary(V)
                       orelse is_number(V) ->
    kz_http_util:urlencode(kz_term:to_binary(V)).


-spec error_to_binary({'error', binary()} | binary()) -> binary().
error_to_binary({'error', Reason}) ->
    error_to_binary(Reason);
error_to_binary(Reason) ->
    try kz_term:to_binary(Reason) of
        Message -> Message
    catch
        _:_ -> <<"Unknown Error">>
    end.

-spec a1hash(ne_binary(), ne_binary(), ne_binary()) -> nonempty_string().
a1hash(User, Realm, Password) ->
    kz_term:to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

%% fetch and cache the kazoo version from the VERSION file in kazoo's root folder
-spec kazoo_version() -> ne_binary().
kazoo_version() ->
    {_, _, Version} = get_app('kazoo'),
    kz_term:to_binary(Version).

-spec write_pid(ne_binary() | nonempty_string() | iolist()) -> 'ok' | {'error', atom()}.
write_pid(FileName) ->
    file:write_file(FileName, io_lib:format("~s", [os:getpid()]), ['write', 'binary']).

-spec pretty_print_bytes(non_neg_integer()) -> ne_binary().
pretty_print_bytes(0) -> <<"0B">>;
pretty_print_bytes(Bytes) ->
    iolist_to_binary(unitfy_bytes(Bytes)).

-spec unitfy_bytes(non_neg_integer()) -> iolist().
unitfy_bytes(0) -> "";
unitfy_bytes(Bytes) when Bytes < ?BYTES_K  ->
    [kz_term:to_binary(Bytes), "B"];
unitfy_bytes(Bytes) when Bytes < ?BYTES_M ->
    K = Bytes div ?BYTES_K,
    [kz_term:to_binary(K), "K", unitfy_bytes(Bytes rem ?BYTES_K)];
unitfy_bytes(Bytes) when Bytes < ?BYTES_G ->
    M = Bytes div ?BYTES_M,
    [kz_term:to_binary(M), "M", unitfy_bytes(Bytes rem ?BYTES_M)];
unitfy_bytes(Bytes) when Bytes < ?BYTES_T ->
    G = Bytes div ?BYTES_G,
    [kz_term:to_binary(G), "G", unitfy_bytes(Bytes rem ?BYTES_G)];
unitfy_bytes(Bytes) ->
    T = Bytes div ?BYTES_T,
    [kz_term:to_binary(T), "T", unitfy_bytes(Bytes rem ?BYTES_T)].

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
    [Name, _Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
    Name.
node_hostname() ->
    [_Name, Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
    Host.


%% @public
-spec write_file(file:name(), iodata()) -> 'ok'.
write_file(Filename, Bytes) ->
    write_file(Filename, Bytes, []).

%% @public
-spec write_file(file:name(), iodata(), [file:mode()]) -> 'ok'.
write_file(Filename, Bytes, Modes) ->
    case file:write_file(Filename, Bytes, Modes) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:debug("writing file ~s (~p) failed : ~p", [Filename, Modes, _E])
    end.

%% @public
-spec delete_file(file:name_all()) -> 'ok'.
delete_file(Filename) ->
    case file:delete(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:debug("deleting file ~s failed : ~p", [Filename, _E])
    end.

%% @public
-spec make_dir(file:name()) -> 'ok'.
make_dir(Filename) ->
    case file:make_dir(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:debug("creating directory ~s failed : ~p", [Filename, _E])
    end.

normalize_amqp_uri(URI) ->
    kz_term:to_binary(amqp_uri:remove_credentials(kz_term:to_list(URI))).

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
        App -> kz_term:to_binary(App);
        {Parent, _MFA} -> kz_term:to_binary(Parent)
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
    {kz_term:to_binary(NewApp), kz_term:to_binary(Version)}.

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
    get_app(kz_term:to_atom(AppName));
get_app(AppName) ->
    case [App || {Name, _, _}=App <- application:loaded_applications(), Name =:= AppName] of
        [] -> 'undefined';
        [Ret | _] -> Ret
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec resolve_uri_test() -> any().
resolve_uri_test() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    RawPathList = [<<"http:">>, <<>>, <<"pivot">>, <<"script2.php">>],

    ?assertEqual(RawPathList, resolve_uri_path(RawPath, Relative)),
    ?assertEqual(RawPathList, resolve_uri_path(RawPath, <<"/", Relative/binary>>)).

-endif.
