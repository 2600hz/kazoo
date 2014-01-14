%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_util).

-export([log_stacktrace/0, log_stacktrace/1
         ,format_account_id/1, format_account_id/2, format_account_id/3
         ,format_account_mod_id/2, format_account_mod_id/3
        ]).
-export([is_in_account_hierarchy/2, is_in_account_hierarchy/3]).
-export([is_system_admin/1
         ,is_system_db/1
        ]).
-export([get_account_realm/1, get_account_realm/2]).
-export([is_account_enabled/1]).

-export([try_load_module/1]).
-export([shuffle_list/1]).

-export([to_integer/1, to_integer/2
         ,to_float/1, to_float/2
         ,to_number/1
         ,to_hex/1, to_hex_binary/1, rand_hex_binary/1
         ,to_list/1, to_binary/1
         ,to_atom/1, to_atom/2
        ]).
-export([to_boolean/1, is_boolean/1
         ,is_true/1, is_false/1
         ,is_empty/1, is_not_empty/1
         ,is_proplist/1
        ]).
-export([to_lower_binary/1, to_upper_binary/1
         ,to_lower_string/1, to_upper_string/1
         ,ucfirst_binary/1, lcfirst_binary/1
         ,strip_binary/1, strip_binary/2
         ,strip_left_binary/2, strip_right_binary/2
        ]).

-export([uri_encode/1
         ,uri_decode/1
        ]).

-export([pad_month/1]).

-export([binary_md5/1]).
-export([pad_binary/3, join_binary/1, join_binary/2]).
-export([a1hash/3, floor/1, ceiling/1]).

-export([current_tstamp/0, ensure_started/1]).
-export([gregorian_seconds_to_unix_seconds/1, unix_seconds_to_gregorian_seconds/1
         ,pretty_print_datetime/1
         ,decr_timeout/2
        ]).
-export([microseconds_to_seconds/1
         ,milliseconds_to_seconds/1
         ,elapsed_s/1, elapsed_ms/1, elapsed_us/1
         ,now_s/1, now_ms/1, now_us/1
        ]).

-export([put_callid/1]).
-export([get_event_type/1]).
-export([get_xml_value/2]).

-export([whistle_version/0, write_pid/1]).

-export([change_console_log_level/1
         ,change_error_log_level/1
         ,change_syslog_log_level/1
        ]).

-include_lib("kernel/include/inet.hrl").
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("../include/wh_types.hrl").
-include("../include/wh_log.hrl").
-include("../include/wh_databases.hrl").

-define(WHISTLE_VERSION_CACHE_KEY, {?MODULE, 'whistle_version'}).

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
    lager:debug("stacktrace:"),
    _ = [lager:debug("st: ~p", [Line]) || Line <- ST],
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
%% @end
%%--------------------------------------------------------------------
-type account_format() :: 'unencoded' | 'encoded' | 'raw'.
-spec format_account_id(ne_binaries() | api_binary() | wh_json:object()) -> ne_binary().
-spec format_account_id(ne_binaries() | api_binary() | wh_json:object(), account_format()) -> ne_binary().
-spec format_account_id(ne_binaries() | api_binary(), wh_year(), wh_month()) -> ne_binary().

format_account_id(Doc) -> format_account_id(Doc, 'unencoded').

format_account_id(DbName, Timestamp) when is_integer(Timestamp) andalso Timestamp > 0 ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(DbName, Year, Month);
format_account_id(<<"accounts">>, _) -> <<"accounts">>;
%% unencode the account db name
format_account_id(<<"account/", _/binary>> = DbName, 'unencoded') ->
    DbName;
format_account_id(<<"account%2F", _/binary>> = DbName, 'unencoded') ->
    binary:replace(DbName, <<"%2F">>, <<"/">>, ['global']);

%% encode the account db name
format_account_id(<<"account%2F", _/binary>>=DbName, 'encoded') ->
    DbName;
format_account_id(<<"account/", _/binary>>=DbName, 'encoded') ->
    binary:replace(DbName, <<"/">>, <<"%2F">>, ['global']);

%% get just the account ID from the account db name
format_account_id(<<"account%2F", AccountId/binary>>, 'raw') ->
    binary:replace(AccountId, <<"%2F">>, <<>>, ['global']);
format_account_id(<<"account/", AccountId/binary>>, 'raw') ->
    binary:replace(AccountId, <<"/">>, <<>>, ['global']);

format_account_id([AccountId], Encoding) when is_binary(AccountId) ->
    format_account_id(AccountId, Encoding);
format_account_id(Account, Encoding) when not is_binary(Account) ->
    case wh_json:is_json_object(Account) of
        'true' -> format_account_id([wh_json:get_value([<<"_id">>], Account)], Encoding);
        'false' -> format_account_id(wh_util:to_binary(Account), Encoding)
    end;

format_account_id(AccountId, 'unencoded') ->
    [Id1, Id2, Id3, Id4 | IdRest] = to_list(AccountId),
    to_binary(["account/", Id1, Id2, $/, Id3, Id4, $/, IdRest]);
format_account_id(AccountId, 'encoded') when is_binary(AccountId) ->
    [Id1, Id2, Id3, Id4 | IdRest] = to_list(AccountId),
    to_binary(["account%2F", Id1, Id2, "%2F", Id3, Id4, "%2F", IdRest]);
format_account_id(AccountId, 'raw') -> AccountId.

format_account_id(AccountId, Year, Month) when is_integer(Year), is_integer(Month) ->
    <<(format_account_id(AccountId, 'encoded'))/binary
      ,"-"
      ,(to_binary(Year))/binary
      ,(pad_month(Month))/binary
    >>.

-spec format_account_mod_id(ne_binary(), pos_integer()) -> ne_binary().
-spec format_account_mod_id(ne_binary(), wh_year(), wh_month()) -> ne_binary().
format_account_mod_id(AccountId, Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(AccountId, Year, Month).
format_account_mod_id(AccountId, Year, Month) ->
    format_account_id(AccountId, Year, Month).

-spec pad_month(pos_integer()) -> ne_binary().
pad_month(Month) when Month < 10 ->
    <<"0", (to_binary(Month))/binary>>;
pad_month(Month) ->
    to_binary(Month).

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
    CheckId = wh_util:format_account_id(CheckFor, 'raw'),
    AccountId = wh_util:format_account_id(InAccount, 'raw'),
    AccountDb = wh_util:format_account_id(InAccount, 'encoded'),
    case (IncludeSelf andalso AccountId =:= CheckId) orelse couch_mgr:open_cache_doc(AccountDb, AccountId) of
        'true' ->
            lager:debug("account ~s is the same as the account to fetch the hierarchy from", [CheckId]),
            'true';
        {'ok', JObj} ->
            Tree = wh_json:get_value(<<"pvt_tree">>, JObj, []),
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
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:is_true(<<"pvt_superduper_admin">>, JObj);
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
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'error', _E} ->
            lager:error("could not open account ~p in ~p", [AccountId, AccountDb]),
            'false';
        {'ok', JObj} ->
            wh_json:is_true(<<"pvt_enabled">>, JObj, 'true')
                andalso wh_json:is_true(<<"enabled">>, JObj, 'true')

    end.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Retrieves the account realm
%% @end
%%--------------------------------------------------------------------
-spec get_account_realm(api_binary()) -> api_binary().
-spec get_account_realm(api_binary(), ne_binary()) -> api_binary().
get_account_realm(AccountId) ->
    get_account_realm(
      wh_util:format_account_id(AccountId, 'encoded')
      ,wh_util:format_account_id(AccountId, 'raw')
     ).

get_account_realm('undefined', _) -> 'undefined';
get_account_realm(Db, AccountId) ->
    case couch_mgr:open_cache_doc(Db, AccountId) of
        {'ok', JObj} -> wh_json:get_ne_value(<<"realm">>, JObj);
        {'error', R} ->
            lager:debug("error while looking up account realm in ~s: ~p", [AccountId, R]),
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
    Module = wh_util:to_atom(Name, 'true'),
    try Module:module_info('imports') of
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
pad_binary(Bin, Size, Value) when size(Bin) < Size ->
    pad_binary(<<Bin/binary, Value/binary>>, Size, Value);
pad_binary(Bin, _, _) -> Bin.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Join a binary together with a seperator.
%% Changed to Accumulator from the binary-contruction for speed reasons:
%%
%% Bins = [to_binary(N) || N <- lists:seq(1,10000)]
%% Old join_binary(Bins): 171.1ms fastest, 221.9ms slowest
%% New join_binary(Bins):   1.1ms fastest,   2.6ms slowest
%% Obvious winner
%%
%% @end
%%--------------------------------------------------------------------
-spec join_binary([text() | atom(),...]) -> binary().
-spec join_binary([text() | atom(),...], binary()) -> binary().

join_binary(Bins) -> join_binary(Bins, <<", ">>, []).
join_binary(Bins, Sep) -> join_binary(Bins, Sep, []).

join_binary([], _, Acc) -> iolist_to_binary(lists:reverse(Acc));
join_binary([Bin], _, Acc) ->
    iolist_to_binary(lists:reverse([to_binary(Bin) | Acc]));
join_binary([Bin|Bins], Sep, Acc) ->
    join_binary(Bins, Sep, [Sep, to_binary(Bin) |Acc]).

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
    D = lists:keysort(1, [{random:uniform(), A} || A <- List]),
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
-spec put_callid(wh_json:object() | wh_proplist() | ne_binary() | atom()) ->
                        api_binary().
put_callid(?NE_BINARY = CallId) -> erlang:put('callid', CallId);
put_callid(Atom) when is_atom(Atom) -> erlang:put('callid', Atom);
put_callid(Prop) when is_list(Prop) -> erlang:put('callid', callid(Prop));
put_callid(JObj) -> erlang:put('callid', callid(JObj)).

callid(Prop) when is_list(Prop) ->
    props:get_first_defined([<<"Call-ID">>, <<"Msg-ID">>], Prop, ?LOG_SYSTEM_ID);
callid(JObj) ->
    wh_json:get_first_defined([<<"Call-ID">>, <<"Msg-ID">>], JObj, ?LOG_SYSTEM_ID).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an API JSON object extract the category and name into a
%% tuple for easy processing
%% @end
%%--------------------------------------------------------------------
-spec get_event_type(wh_json:object() | wh_proplist()) -> {api_binary(), api_binary()}.
get_event_type(Props) when is_list(Props) ->
    {props:get_value(<<"Event-Category">>, Props)
     ,props:get_value(<<"Event-Name">>, Props)
    };
get_event_type(JObj) ->
    {wh_json:get_value(<<"Event-Category">>, JObj)
     ,wh_json:get_value(<<"Event-Name">>, JObj)
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generic helper to get the text value of a XML path
%% @end
%%--------------------------------------------------------------------
-spec get_xml_value(wh_deeplist(), xml_el()) -> api_binary().
get_xml_value(Paths, Xml) ->
    Path = lists:flatten(Paths),
    try xmerl_xpath:string(Path, Xml) of
        [#xmlText{value=Value}] ->
            wh_util:to_binary(Value);
        [#xmlText{}|_]=Values ->
            iolist_to_binary([wh_util:to_binary(Value)
                              || #xmlText{value=Value} <- Values
                             ]);
        [#xmlAttribute{value=Value}] ->
            wh_util:to_binary(Value);
        [#xmlAttribute{}|_]=Values ->
            iolist_to_binary([wh_util:to_binary(Value)
                              || #xmlText{value=Value} <- Values
                             ]);
        _Else -> 'undefined'
    catch
        _E:_R ->
            lager:debug("~s getting value of '~s': ~p", [_E, Path, _R]),
            'undefined'
    end.

%% must be a term that can be changed to a list
-spec to_hex(binary() | string()) -> string().
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

-spec to_hex_binary(binary() | string()) -> binary().
to_hex_binary(S) ->
    Bin = to_binary(S),
    << <<(binary_to_hex_char(B div 16)), (binary_to_hex_char(B rem 16))>> || <<B>> <= Bin>>.

-spec rand_hex_binary(pos_integer()) -> ne_binary().
rand_hex_binary(Size) when is_integer(Size) andalso Size > 0 ->
    to_hex_binary(crypto:rand_bytes(Size)).

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

-spec to_integer(string() | binary() | integer() | float()) -> integer().
-spec to_integer(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X) -> to_integer(X, 'notstrict').

to_integer(X, 'strict') when is_float(X) -> erlang:error('badarg');
to_integer(X, 'notstrict') when is_float(X) -> round(X);
to_integer(X, S) when is_binary(X) -> to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
    try list_to_integer(X) of
        I -> I
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
    try list_to_float(X) of
        F -> F
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
    try list_to_integer(X) of
        Int -> Int
    catch
        'error':'badarg' -> list_to_float(X)
    end.

-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_float(X) -> mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float()) -> binary().
to_binary(X) when is_float(X) -> to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> iolist_to_binary(X);
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

-spec is_true(binary() | string() | atom()) -> boolean().
is_true(<<"true">>) -> 'true';
is_true("true") -> 'true';
is_true('true') -> 'true';
is_true(_) -> 'false'.

-spec is_false(binary() | string() | atom()) -> boolean().
is_false(<<"false">>) -> 'true';
is_false("false") -> 'true';
is_false('false') -> 'true';
is_false(_) -> 'false'.

-spec is_boolean(binary() | string() | atom()) -> boolean().
is_boolean(<<"true">>) -> 'true';
is_boolean("true") -> 'true';
is_boolean('true') -> 'true';
is_boolean(<<"false">>) -> 'true';
is_boolean("false") -> 'true';
is_boolean('false') -> 'true';
is_boolean(_) -> 'false'.

-spec is_empty(term()) -> boolean().
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
is_empty(Float) when is_float(Float), Float == 0.0 -> 'true';
is_empty(MaybeJObj) ->
    case wh_json:is_json_object(MaybeJObj) of
        'false' -> 'false'; %% if not a json object, its not empty
        'true' -> wh_json:is_empty(MaybeJObj)
    end.

-spec is_not_empty(term()) -> boolean().
is_not_empty(Term) -> (not is_empty(Term)).

-spec is_proplist(any()) -> boolean().
is_proplist(Term) when is_list(Term) ->
    lists:all(fun({_,_}) -> 'true'; (A) -> is_atom(A) end, Term);
is_proplist(_) -> 'false'.

-spec to_lower_binary(term()) -> api_binary().
to_lower_binary('undefined') -> 'undefined';
to_lower_binary(Bin) when is_binary(Bin) -> << <<(to_lower_char(B))>> || <<B>> <= Bin>>;
to_lower_binary(Else) -> to_lower_binary(to_binary(Else)).

-spec to_lower_string(term()) -> 'undefined' | list().
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

-spec to_upper_binary(term()) -> api_binary().
to_upper_binary('undefined') -> 'undefined';
to_upper_binary(Bin) when is_binary(Bin) -> << <<(to_upper_char(B))>> || <<B>> <= Bin>>;
to_upper_binary(Else) -> to_upper_binary(to_binary(Else)).

-spec to_upper_string(term()) -> 'undefined' | list().
to_upper_string('undefined') -> 'undefined';
to_upper_string(L) when is_list(L) -> [to_upper_char(C) || C <- L];
to_upper_string(Else) -> to_upper_string(to_list(Else)).

-spec to_upper_char(char()) -> char().
to_upper_char(C) when is_integer(C), $a =< C, C =< $z -> C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> C - 32;
to_upper_char(C) -> C.

-spec strip_binary(binary()) -> binary().
-spec strip_binary(binary(), 'both' | 'left' | 'right') -> binary().
-spec strip_left_binary(binary(), char()) -> binary().
-spec strip_right_binary(binary(), char()) -> binary().
strip_binary(B) -> strip_binary(B, 'both').

strip_binary(B, 'left') -> strip_left_binary(B, $\s);
strip_binary(B, 'right') -> strip_right_binary(B, $\s);
strip_binary(B, 'both') -> strip_right_binary(strip_left_binary(B, $\s), $\s);
strip_binary(B, C) when is_integer(C) -> strip_right_binary(strip_left_binary(B, C), C).

strip_left_binary(<<C, B/binary>>, C) -> strip_left_binary(B, C);
strip_left_binary(B, _) -> B.

strip_right_binary(C, C) -> <<>>;
strip_right_binary(<<C, B/binary>>, C) ->
    case strip_right_binary(B, C) of
        <<>> -> <<>>;
        T -> <<C, T/binary>>
    end;
strip_right_binary(<<A, B/binary>>, C) -> <<A, (strip_right_binary(B, C))/binary>>;
strip_right_binary(<<>>, _) -> <<>>.

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
-spec current_tstamp() -> non_neg_integer().
current_tstamp() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%% fetch and cache the whistle version from the VERSION file in whistle's root folder
-spec whistle_version() -> ne_binary().
whistle_version() ->
    case wh_cache:fetch(?WHISTLE_VERSION_CACHE_KEY) of
        {'ok', Version} ->  Version;
        {'error', _} ->
            VersionFile = filename:join([code:lib_dir('whistle'), "..", "..", "VERSION"]),
            whistle_version(VersionFile)
    end.

-spec whistle_version(ne_binary() | nonempty_string()) -> ne_binary().
whistle_version(FileName) ->
    case file:read_file(FileName) of
        {'ok', Version} ->
            wh_cache:store(?WHISTLE_VERSION_CACHE_KEY, Version),
            list_to_binary(string:strip(binary_to_list(Version), 'right', $\n));
        _ ->
            Version = <<"not available">>,
            wh_cache:store(?WHISTLE_VERSION_CACHE_KEY, Version),
            Version
    end.

-spec write_pid(ne_binary() | nonempty_string() | iolist()) -> 'ok' | {'error', atom()}.
write_pid(FileName) ->
    file:write_file(FileName, io_lib:format("~s", [os:getpid()]), ['write', 'binary']).

-spec ensure_started(atom()) -> 'ok' | {'error', term()}.
ensure_started(App) when is_atom(App) ->
    case application:start(App) of
        'ok' -> 'ok';
        {'error', {'already_started', App}} -> 'ok';
        E -> E
    end.

%% there are 86400 seconds in a day
%% there are 62167219200 seconds between Jan 1, 0000 and Jan 1, 1970
-define(UNIX_EPOCH_AS_GREG_SECONDS, 62167219200).

-spec gregorian_seconds_to_unix_seconds(integer() | string() | binary()) -> non_neg_integer().
gregorian_seconds_to_unix_seconds(GregorianSeconds) ->
    to_integer(GregorianSeconds) - ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec unix_seconds_to_gregorian_seconds(integer() | string() | binary()) -> non_neg_integer().
unix_seconds_to_gregorian_seconds(UnixSeconds) ->
    to_integer(UnixSeconds) + ?UNIX_EPOCH_AS_GREG_SECONDS.

-spec pretty_print_datetime(wh_datetime() | wh_now()) -> ne_binary().
pretty_print_datetime(Timestamp) when is_integer(Timestamp) ->
    pretty_print_datetime(calendar:gregorian_seconds_to_datetime(Timestamp));
pretty_print_datetime({{Y,Mo,D},{H,Mi,S}}) ->
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0w_~2..0w-~2..0w-~2..0w",
                                   [Y, Mo, D, H, Mi, S])).

-spec decr_timeout(wh_timeout(), non_neg_integer() | wh_now()) -> wh_timeout().
decr_timeout('infinity', _) -> 'infinity';
decr_timeout(Timeout, Elapsed) when is_integer(Elapsed) ->
    Diff = Timeout - Elapsed,
    case Diff < 0 of
        'true' -> 0;
        'false' -> Diff
    end;
decr_timeout(Timeout, Start) -> decr_timeout(Timeout, wh_util:elapsed_ms(Start)).

-spec microseconds_to_seconds(float() | integer() | string() | binary()) -> non_neg_integer().
microseconds_to_seconds(Microseconds) -> to_integer(Microseconds) div 1000000.
milliseconds_to_seconds(Milliseconds) -> to_integer(Milliseconds) div 1000.

-spec elapsed_s(wh_now() | pos_integer()) -> pos_integer().
-spec elapsed_ms(wh_now() | pos_integer()) -> pos_integer().
-spec elapsed_us(wh_now() | pos_integer()) -> pos_integer().
elapsed_s({_,_,_}=Start) -> timer:now_diff(erlang:now(), Start) div 1000000;
elapsed_s(Start) when is_integer(Start) -> current_tstamp() - Start.

elapsed_ms({_,_,_}=Start) -> timer:now_diff(erlang:now(), Start) div 1000;
elapsed_ms(Start) when is_integer(Start) -> current_tstamp() - Start * 1000.

elapsed_us({_,_,_}=Start) -> timer:now_diff(erlang:now(), Start);
elapsed_us(Start) when is_integer(Start) -> current_tstamp() - Start * 1000000.

-spec now_s(wh_now()) -> integer().
-spec now_ms(wh_now()) -> integer().
-spec now_us(wh_now()) -> integer().
now_us({MegaSecs,Secs,MicroSecs}) -> (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.
now_ms({_,_,_}=Now) -> now_us(Now) div 1000.
now_s({_,_,_}=Now) -> now_us(Now) div 1000000.

-ifdef(TEST).

%% PROPER TESTING
prop_to_integer() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Is = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_integer(to_integer(N)) andalso erlang:is_integer(to_integer(FN)) end, Is)
            end).

prop_to_number() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Is = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_number(to_number(N)) andalso erlang:is_number(to_number(FN)) end, Is)
            end).

prop_to_float() ->
    ?FORALL({F, I}, {float(), integer()},
            begin
                Fs = [ [Fun(N), N] || Fun <- [ fun to_list/1, fun to_binary/1], N <- [F, I] ],
                lists:all(fun([FN, N]) -> erlang:is_float(to_float(N)) andalso erlang:is_float(to_float(FN)) end, Fs)
            end).

prop_to_list() ->
    ?FORALL({A, L, B, I, F}, {atom(), list(), binary(), integer(), float()},
            lists:all(fun(X) -> is_list(to_list(X)) end, [A, L, B, I, F])).

                                                %-type iolist() :: maybe_improper_list(char() | binary() | iolist(), binary() | []).
prop_to_binary() ->
    ?FORALL({A, L, B, I, F, IO}, {atom(), list(range(0,255)), binary(), integer(), float(), iolist()},
            lists:all(fun(X) -> is_binary(to_binary(X)) end, [A, L, B, I, F, IO])).

prop_iolist_t() ->
    ?FORALL(IO, iolist(), is_binary(to_binary(IO))).

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {timeout, 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{max_shrinks, 0}]))
      ]}}.

pad_binary_test() ->
    ?assertEqual(<<"1234500000">>, pad_binary(<<"12345">>, 10, <<"0">>)).

greg_secs_to_unix_secs_test() ->
    GregSecs = current_tstamp(),
    ?assertEqual(GregSecs - ?UNIX_EPOCH_AS_GREG_SECONDS, gregorian_seconds_to_unix_seconds(GregSecs)).

unix_secs_to_greg_secs_test() ->
    UnixSecs = 1000000000,
    ?assertEqual(UnixSecs + ?UNIX_EPOCH_AS_GREG_SECONDS, unix_seconds_to_gregorian_seconds(UnixSecs)).

microsecs_to_secs_test() ->
    Microsecs = 1310157838405890,
    Secs = 1310157838,
    ?assertEqual(Secs, microseconds_to_seconds(Microsecs)).

no_whistle_version_test() ->
    ?assertEqual(<<"not available">>, whistle_version(<<"/path/to/nonexistent/file">>)).

join_binary_test() ->
    ?assertEqual(<<"foo">>, join_binary([<<"foo">>], <<", ">>)),
    ?assertEqual(<<"foo, bar">>, join_binary([<<"foo">>, <<"bar">>], <<", ">>)),
    ?assertEqual(<<"foo, bar, baz">>, join_binary([<<"foo">>, <<"bar">>, <<"baz">>], <<", ">>)).

ucfirst_binary_test() ->
    ?assertEqual(<<"Foo">>, ucfirst_binary(<<"foo">>)),
    ?assertEqual(<<"Foo">>, ucfirst_binary(<<"Foo">>)),
    ?assertEqual(<<"FOO">>, ucfirst_binary(<<"FOO">>)),
    ?assertEqual(<<"1oo">>, ucfirst_binary(<<"1oo">>)),
    ?assertEqual(<<"100">>, ucfirst_binary(<<"100">>)),
    ?assertEqual(<<"1FF">>, ucfirst_binary(<<"1FF">>)).

lcfirst_binary_test() ->
    ?assertEqual(<<"foo">>, lcfirst_binary(<<"foo">>)),
    ?assertEqual(<<"foo">>, lcfirst_binary(<<"Foo">>)),
    ?assertEqual(<<"fOO">>, lcfirst_binary(<<"FOO">>)),
    ?assertEqual(<<"1oo">>, lcfirst_binary(<<"1oo">>)),
    ?assertEqual(<<"100">>, lcfirst_binary(<<"100">>)),
    ?assertEqual(<<"1FF">>, lcfirst_binary(<<"1FF">>)).

to_lower_binary_test() ->
    ?assertEqual(<<"foo">>, to_lower_binary(<<"foo">>)),
    ?assertEqual(<<"foo">>, to_lower_binary(<<"Foo">>)),
    ?assertEqual(<<"foo">>, to_lower_binary(<<"FoO">>)),
    ?assertEqual(<<"f00">>, to_lower_binary(<<"f00">>)),
    ?assertEqual(<<"f00">>, to_lower_binary(<<"F00">>)).

to_upper_binary_test() ->
    ?assertEqual(<<"FOO">>, to_upper_binary(<<"foo">>)),
    ?assertEqual(<<"FOO">>, to_upper_binary(<<"Foo">>)),
    ?assertEqual(<<"FOO">>, to_upper_binary(<<"FoO">>)),
    ?assertEqual(<<"F00">>, to_upper_binary(<<"f00">>)),
    ?assertEqual(<<"F00">>, to_upper_binary(<<"F00">>)).

to_lower_string_test() ->
    ?assertEqual("foo", to_lower_string("foo")),
    ?assertEqual("foo", to_lower_string("Foo")),
    ?assertEqual("foo", to_lower_string("FoO")),
    ?assertEqual("f00", to_lower_string("f00")),
    ?assertEqual("f00", to_lower_string("F00")).

to_upper_string_test() ->
    ?assertEqual("FOO", to_upper_string("foo")),
    ?assertEqual("FOO", to_upper_string("Foo")),
    ?assertEqual("FOO", to_upper_string("FoO")),
    ?assertEqual("F00", to_upper_string("f00")),
    ?assertEqual("F00", to_upper_string("F00")).

strip_binary_test() ->
    ?assertEqual(<<"foo">>, strip_binary(<<"foo">>)),
    ?assertEqual(<<"foo">>, strip_binary(<<"foo ">>)),
    ?assertEqual(<<"foo">>, strip_binary(<<" foo ">>)),
    ?assertEqual(<<"foo">>, strip_binary(<<"  foo  ">>)),
    ?assertEqual(<<"foo">>, strip_binary(<<"     foo">>)),

    ?assertEqual(<<"foo">>, strip_left_binary(<<"foo">>, $\s)),
    ?assertEqual(<<"foo">>, strip_left_binary(<<" foo">>, $\s)),
    ?assertEqual(<<"foo ">>, strip_left_binary(<<" foo ">>, $\s)),
    ?assertEqual(<<"foo ">>, strip_left_binary(<<"foo ">>, $\s)),

    ?assertEqual(<<"foo">>, strip_right_binary(<<"foo">>, $\s)),
    ?assertEqual(<<" foo">>, strip_right_binary(<<" foo">>, $\s)),
    ?assertEqual(<<" foo">>, strip_right_binary(<<" foo ">>, $\s)),
    ?assertEqual(<<"foo">>, strip_right_binary(<<"foo ">>, $\s)).

to_boolean_test() ->
    All = [<<"true">>, "true", 'true', <<"false">>, "false", 'false'],
    NotAll = [0, 123, 1.23, "123", "abc", abc, <<"abc">>, <<"123">>, {what, is, this, doing, here}],
    ?assertEqual('true', lists:all(fun(X) ->
                                         try to_boolean(X) of
                                             _ -> 'true'
                                         catch _:_ -> 'false'
                                         end
                                 end, All)),
    ?assertEqual('true', lists:all(fun(X) ->
                                         try to_boolean(X) of
                                             _ -> 'false'
                                         catch _:_ -> 'true'
                                         end
                                 end, NotAll)).

strip_test() ->
    ?assertEqual(strip_binary(<<"...Hello.....">>, $.), <<"Hello">>).

-endif.
