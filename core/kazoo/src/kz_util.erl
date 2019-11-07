%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Various utilities - a veritable cornucopia.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_util).

-export([format_account_id/1, format_account_id/2, format_account_id/3
        ,format_account_mod_id/1, format_account_mod_id/2, format_account_mod_id/3
        ,format_account_db/1
        ,format_account_modb/1, format_account_modb/2
        ,format_resource_selectors_id/1, format_resource_selectors_id/2
        ,format_resource_selectors_db/1
        ]).

-export([pretty_print_bytes/1, pretty_print_bytes/2
        ,bin_usage/0, mem_usage/0
        ,set_startup/0, startup/0
        ]).
-export([get_event_type/1]).

-export([kazoo_version/0, write_pid/1]).

-export([node_name/0, node_hostname/0]).

-export([write_file/2, write_file/3
        ,rename_file/2
        ,delete_file/1
        ,delete_dir/1
        ,make_dir/1
        ]).

-export([calling_app/0]).
-export([calling_app_version/0]).
-export([calling_process/0]).
-export([get_app/1]).

-export([application_version/1]).

-include_lib("kernel/include/inet.hrl").

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").

-define(KAZOO_VERSION_CACHE_KEY, {?MODULE, 'kazoo_version'}).

-export_type([account_format/0]).

-type account_format() :: 'unencoded' | 'encoded' | 'raw'.

%% @equiv format_account_id(Account, raw)

-spec format_account_id(kz_term:api_binary()) -> kz_term:api_binary().
format_account_id(Account) ->
    format_account_id(Account, 'raw').

%%------------------------------------------------------------------------------
%% @doc Given a representation of an account return it in a `encoded',
%% `unencoded' or `raw' format.
%%
%% <div class="notice">Accepts MODbs as well as account IDs/DBs</div>
%% <div class="notice">If given `(Account, GregorianSeconds)', it will return
%% an MODB in the `encoded' format.</div>
%% @end
%%------------------------------------------------------------------------------

-spec format_account_id(kz_term:api_binary(), account_format()) -> kz_term:api_ne_binary();
                       (kz_term:api_binary(), kz_time:gregorian_seconds()) -> kz_term:api_ne_binary(). %% for MODb!
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

%%------------------------------------------------------------------------------
%% @doc Returns `raw' account ID if it's account ID/DB/MODB/ResourceSelector,
%% otherwise returns same passing binary.
%% Passes input along if not `account_id() | account_db() | account_db_unencoded()'.
%% @end
%%------------------------------------------------------------------------------

-spec raw_account_id(kz_term:ne_binary()) -> kz_term:ne_binary().
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

%%------------------------------------------------------------------------------
%% `(modb()) -> modb_id() when modb() :: modb_id() | modb_db() | modb_db_unencoded()'
%% Crashes if given anything else.
%%------------------------------------------------------------------------------
-spec raw_account_modb(kz_term:ne_binary()) -> kz_term:ne_binary().
raw_account_modb(?MATCH_MODB_SUFFIX_RAW(_, _, _) = AccountId) ->
    AccountId;
raw_account_modb(?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month)) ->
    ?MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month);
raw_account_modb(?MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month)) ->
    ?MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month).

%% @equiv format_resource_selectors_id(Account, raw)

-spec format_resource_selectors_id(kz_term:api_binary()) -> kz_term:api_binary().
format_resource_selectors_id(Account) ->
    format_resource_selectors_id(Account, 'raw').

%%------------------------------------------------------------------------------
%% @doc Given a representation of an account `resource_selectors'.
%% Returns it in a `encoded', `unencoded' or `raw' format.
%% @end
%%------------------------------------------------------------------------------

-spec format_resource_selectors_id(kz_term:api_binary(), account_format()) -> kz_term:api_binary();
                                  (kz_term:api_binary(), kz_time:gregorian_seconds()) -> kz_term:api_binary(). %% MODb!
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
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_resource_selectors_id(AccountId, 'encoded') ->
    ?MATCH_RESOURCE_SELECTORS_RAW(A,B,Rest) = raw_resource_selectors_id(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

%%------------------------------------------------------------------------------
%% Returns `account_id() | any()'.
%% Passes input along if not `account_id() | account_db() | account_db_unencoded().'
%%------------------------------------------------------------------------------
-spec raw_resource_selectors_id(kz_term:ne_binary()) -> kz_term:ne_binary().
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

%% @equiv format_resource_selectors_id(Account, encoded)

-spec format_resource_selectors_db(kz_term:api_binary()) -> kz_term:api_binary().
format_resource_selectors_db(AccountId) ->
    format_resource_selectors_id(AccountId, 'encoded').

%%------------------------------------------------------------------------------
%% @doc Given a representation of an account, build an MODb in an `encoded' format.
%%
%% <div class="notice">Accepts MODbs as well as account IDs/DBs</div>
%% @end
%%------------------------------------------------------------------------------
-spec format_account_id(kz_term:api_binary(), kz_time:year() | kz_term:ne_binary(), kz_time:month() | kz_term:ne_binary()) ->
                               kz_term:api_binary().
format_account_id('undefined', _Year, _Month) -> 'undefined';
format_account_id(AccountId, Year, Month) when not is_integer(Year) ->
    format_account_id(AccountId, kz_term:to_integer(Year), Month);
format_account_id(AccountId, Year, Month) when not is_integer(Month) ->
    format_account_id(AccountId, Year, kz_term:to_integer(Month));
format_account_id(Account, Year, Month) when is_integer(Year),
                                             is_integer(Month) ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_id(Account),
    ?MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, kz_term:to_binary(Year), kz_date:pad_month(Month)).

%% @equiv format_account_mod_id(Account, os:timestamp())

-spec format_account_mod_id(kz_term:api_binary()) -> kz_term:api_binary().
format_account_mod_id(Account) ->
    format_account_mod_id(Account, os:timestamp()).

%% @equiv format_account_id(AccountId, Year, Month)

-spec format_account_mod_id(kz_term:api_binary(), kz_time:gregorian_seconds() | kz_time:now()) -> kz_term:api_binary().
format_account_mod_id(AccountId, {_,_,_}=Timestamp) ->
    {{Year, Month, _}, _} = calendar:now_to_universal_time(Timestamp),
    format_account_id(AccountId, Year, Month);
format_account_mod_id(AccountId, Timestamp) when is_integer(Timestamp) ->
    {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Timestamp),
    format_account_id(AccountId, Year, Month).

%%------------------------------------------------------------------------------
%% @doc Given a representation of an account, build an MODb in an `encoded' format.
%%
%% <div class="notice">Accepts MODbs as well as account IDs/DBs</div>
%% @end
%%------------------------------------------------------------------------------

-spec format_account_mod_id(kz_term:api_binary(), kz_time:year() | kz_term:ne_binary(), kz_time:month() | kz_term:ne_binary()) ->
                                   kz_term:api_binary().
format_account_mod_id(AccountId, Year, Month) ->
    format_account_id(AccountId, Year, Month).

%% @equiv format_account_id(AccountId, encoded)

-spec format_account_db(kz_term:api_binary()) -> kz_term:api_binary().
format_account_db(AccountId) ->
    format_account_id(AccountId, 'encoded').

%% @equiv format_account_modb(AccountId, raw)

-spec format_account_modb(kz_term:ne_binary()) -> kz_term:ne_binary().
format_account_modb(AccountId) ->
    format_account_modb(AccountId, 'raw').

%%------------------------------------------------------------------------------
%% @doc Given a representation of an MODb, returns the MODb in the specified format.
%%
%% <div class="notice">crashes if given anything but an MODb (in any format).</div>
%% @end
%%------------------------------------------------------------------------------

-spec format_account_modb(kz_term:ne_binary(), account_format()) -> kz_term:ne_binary().
format_account_modb(AccountId, 'raw') ->
    raw_account_modb(AccountId);
format_account_modb(AccountId, 'unencoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    kz_term:to_binary(["account/", A, "/", B, "/", Rest]);
format_account_modb(AccountId, 'encoded') ->
    ?MATCH_ACCOUNT_RAW(A,B,Rest) = raw_account_modb(AccountId),
    kz_term:to_binary(["account%2F", A, "%2F", B, "%2F", Rest]).

-spec set_startup() -> kz_time:api_seconds().
set_startup() ->
    put('$startup', kz_time:now_s()).

-spec startup() -> kz_time:api_seconds().
startup() ->
    get('$startup').

%%------------------------------------------------------------------------------
%% @doc Given an object, extract the category and name into a tuple.
%% @end
%%------------------------------------------------------------------------------
-spec get_event_type(kz_term:api_terms()) -> {kz_term:api_binary(), kz_term:api_binary()}.
get_event_type(Props) when is_list(Props) ->
    {props:get_value(<<"Event-Category">>, Props)
    ,props:get_value(<<"Event-Name">>, Props)
    };
get_event_type(JObj) ->
    {kz_json:get_value(<<"Event-Category">>, JObj)
    ,kz_json:get_value(<<"Event-Name">>, JObj)
    }.

%%------------------------------------------------------------------------------
%% @doc Fetch and cache the kazoo version from the VERSION file in kazoo's root folder/
%% @end
%%------------------------------------------------------------------------------
-spec kazoo_version() -> kz_term:ne_binary().
kazoo_version() ->
    {_, _, Version} = get_app('kazoo'),
    kz_term:to_binary(Version).

-spec write_pid(file:filename_all()) -> 'ok' | {'error', atom()}.
write_pid(FileName) ->
    file:write_file(FileName, io_lib:format("~s", [os:getpid()]), ['write', 'binary']).


-spec pretty_print_bytes(non_neg_integer()) -> kz_term:ne_binary().
pretty_print_bytes(Bytes) ->
    pretty_print_bytes(Bytes, 'full').

-spec pretty_print_bytes(non_neg_integer(), 'full' | 'truncated') -> kz_term:ne_binary().
pretty_print_bytes(0, _) -> <<"0B">>;
pretty_print_bytes(Bytes, Type) ->
    iolist_to_binary(unitfy_bytes(Bytes, Type)).

-spec unitfy_bytes(non_neg_integer(), 'full' | 'truncated') -> iolist().
unitfy_bytes(0, _Type) -> "";
unitfy_bytes(Bytes, _Type) when Bytes < ?BYTES_K  ->
    [kz_term:to_binary(Bytes), "B"];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_M ->
    K = Bytes div ?BYTES_K,
    [kz_term:to_binary(K), "K", maybe_unitfy_bytes(Bytes rem ?BYTES_K, Type)];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_G ->
    M = Bytes div ?BYTES_M,
    [kz_term:to_binary(M), "M", maybe_unitfy_bytes(Bytes rem ?BYTES_M, Type)];
unitfy_bytes(Bytes, Type) when Bytes < ?BYTES_T ->
    G = Bytes div ?BYTES_G,
    [kz_term:to_binary(G), "G", maybe_unitfy_bytes(Bytes rem ?BYTES_G, Type)];
unitfy_bytes(Bytes, Type) ->
    T = Bytes div ?BYTES_T,
    [kz_term:to_binary(T), "T", maybe_unitfy_bytes(Bytes rem ?BYTES_T, Type)].

-spec maybe_unitfy_bytes(non_neg_integer(), 'full' | 'truncated') -> iolist().
maybe_unitfy_bytes(Bytes, 'full'=Type) ->
    unitfy_bytes(Bytes, Type);
maybe_unitfy_bytes(_Bytes, 'truncated') ->
    <<>>.

-spec bin_usage() -> integer().
bin_usage() ->
    {'ok', {_, Usage, _}} = recon_lib:proc_attrs(binary_memory, self()),
    Usage.

-spec mem_usage() -> integer().
mem_usage() ->
    {'memory', Memory} = erlang:process_info(self(), 'memory'),
    Memory.

-spec node_name() -> binary().
node_name() ->
    [Name, _Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
    Name.

-spec node_hostname() -> binary().
node_hostname() ->
    [_Name, Host] = binary:split(kz_term:to_binary(node()), <<"@">>),
    Host.

-spec write_file(file:filename_all(), iodata()) -> 'ok'.
write_file(Filename, Bytes) ->
    write_file(Filename, Bytes, []).

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

-spec delete_file(file:filename_all()) -> 'ok'.
delete_file(Filename) ->
    case file:delete(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("deleting file ~s failed : ~p", [Filename, _E])
    end.

-spec delete_dir(string()) -> 'ok'.
delete_dir(Dir) ->
    F = fun(D) -> 'ok' = file:del_dir(D) end,
    lists:foreach(F, del_all_files([Dir], [])).

-spec del_all_files(kz_term:strings(), kz_term:strings()) -> kz_term:strings().
del_all_files([], EmptyDirs) -> EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
    {'ok', FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                        Path = Dir ++ "/" ++ F,
                                        case filelib:is_dir(Path) of
                                            'true' ->
                                                {Fs, [Path | Ds]};
                                            'false' ->
                                                {[Path | Fs], Ds}
                                        end
                                end, {[],[]}, FilesInDir),
    lists:foreach(fun delete_file/1, Files),
    del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

-spec make_dir(file:filename_all()) -> 'ok'.
make_dir(Filename) ->
    case file:make_dir(Filename) of
        'ok' -> 'ok';
        {'error', _}=_E ->
            lager:error("creating directory ~s failed : ~p", [Filename, _E])
    end.

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

-spec calling_app_stacktrace() -> any().
calling_app_stacktrace() ->
    try throw('get_stacktrace')
    catch
        _E:_R:ST ->
            [_Me | Others] = ST,
            Others
    end.

%%------------------------------------------------------------------------------
%% @doc For core applications that want to know which app is calling.
%% @end
%%------------------------------------------------------------------------------
-spec calling_app() -> kz_term:ne_binary().
calling_app() ->
    Modules = calling_app_stacktrace(),
    [_Me, {Module, _, _, _} | Start] = Modules,
    {'ok', App} = application:get_application(Module),
    case process_fold(Start, App) of
        App -> kz_term:to_binary(App);
        {Parent, _MFA} -> kz_term:to_binary(Parent)
    end.

-spec calling_app_version() -> {kz_term:ne_binary(), kz_term:ne_binary()}.
calling_app_version() ->
    {'ok', App} = application:get_application(self()),
    {NewApp, _, Version} = get_app(App),
    {kz_term:to_binary(NewApp), kz_term:to_binary(Version)}.

-spec calling_process() -> map().
calling_process() ->
    Modules = calling_app_stacktrace(),
    [_Me, {Module, _, _, _}=M | Start] = Modules,
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

-spec get_app(atom() | kz_term:ne_binary()) -> {atom(), string(), string()} | 'undefined'.
get_app(<<_/binary>> = AppName) ->
    get_app(kz_term:to_atom(AppName));
get_app(AppName) ->
    case [App || {Name, _, _}=App <- application:loaded_applications(), Name =:= AppName] of
        [] -> 'undefined';
        [Ret | _] -> Ret
    end.

-spec application_version(atom()) -> kz_term:ne_binary().
application_version(Application) ->
    case application:get_key(Application, 'vsn') of
        {'ok', Vsn} -> kz_term:to_binary(Vsn);
        'undefined' -> <<"unknown">>
    end.
