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

-export([log_stacktrace/0, log_stacktrace/1]).

-export([try_load_module/1]).

-export([normalize_amqp_uri/1]).

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
