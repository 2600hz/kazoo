-module(freeswitch).

-export([version/1
        ,version/2
        ]).
-export([noevents/1]).
-export([close/1]).
-export([getpid/1
        ,getpid/2
        ]).
-export([bind/2
        ,bind/3
        ]).
-export([fetch_reply/4
        ,fetch_reply/5
        ]).
-export([api/2
        ,api/3
        ,api/4
        ]).
-export([bgapi/3
        ,bgapi/4
        ,bgapi/5
        ,bgapi/6
        ]).
-export([event/2
        ,event/3
        ]).
-export([nixevent/2]).
-export([sendevent/3
        ,sendevent_custom/3
        ]).
-export([sendmsg/3]).
-export([cmd/3, cmds/3]).
-export([cast_cmd/3, cast_cmds/3]).

-export([config/1, config/2
        ,bgapi4/5
        ]).

-include("ecallmgr.hrl").

-define(TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-define(FS_MODULE, mod_kazoo).

-type fs_api_ok() :: {'ok', binary()}.
-type fs_api_error():: {'error', 'timeout' | 'exception' | binary()}.
-type fs_api_return() :: fs_api_ok() | fs_api_error() | 'ok'.
-export_type([fs_api_ok/0
             ,fs_api_error/0
             ,fs_api_return/0
             ]).

-spec version(atom()) -> fs_api_return().
-spec version(atom(), pos_integer()) -> fs_api_return().
version(Node) -> ?FS_MODULE:version(Node).
version(Node, Timeout) -> ?FS_MODULE:version(Node, Timeout).

-spec noevents(atom()) -> fs_api_return().
noevents(Node) -> ?FS_MODULE:noevents(Node).

-spec close(atom()) -> 'ok'.
close(Node) -> ?FS_MODULE:close(Node).

-spec getpid(atom()) -> fs_api_return().
-spec getpid(atom(), pos_integer()) -> fs_api_return().
getpid(Node) -> ?FS_MODULE:getpid(Node).
getpid(Node, Timeout) -> ?FS_MODULE:getpid(Node, Timeout).

-spec bind(atom(), atom()) -> fs_api_return().
-spec bind(atom(), atom(), pos_integer()) -> fs_api_return().
bind(Node, Type) -> ?FS_MODULE:bind(Node, Type).
bind(Node, Type, Timeout) -> ?FS_MODULE:bind(Node, Type, Timeout).

-spec fetch_reply(atom(), binary(), atom() | binary(), binary() | string()) -> 'ok'.
-spec fetch_reply(atom(), binary(), atom() | binary(), binary() | string(), pos_integer() | 'infinity') ->
                         'ok' | {'error', 'baduuid'}.
fetch_reply(Node, FetchID, Section, Reply) -> ?FS_MODULE:fetch_reply(Node, FetchID, Section, Reply).
fetch_reply(Node, FetchID, Section, Reply, Timeout) -> ?FS_MODULE:fetch_reply(Node, FetchID, Section, Reply, Timeout).

-spec api(atom(), text()) -> fs_api_return().
-spec api(atom(), text(), text()) -> fs_api_return().
-spec api(atom(), text(), text(), kz_timeout()) -> fs_api_return().
api(Node, Cmd) -> ?FS_MODULE:api(Node, Cmd).
api(Node, Cmd, Args) -> ?FS_MODULE:api(Node, Cmd, Args).
api(Node, Cmd, Args, Timeout) -> ?FS_MODULE:api(Node, Cmd, Args, Timeout).

%% @doc Make a backgrounded API call to FreeSWITCH. The asynchronous reply is
%% sent to calling process after it is received. This function
%% returns the result of the initial bgapi call or `timeout' if FreeSWITCH fails
%% to respond.
-spec bgapi(atom(), atom(), string() | binary()) -> fs_api_return().
bgapi(Node, Cmd, Args) -> ?FS_MODULE:bgapi(Node, Cmd, Args).

-spec bgapi(atom(), atom(), string() | binary(), fun()) -> fs_api_return().
bgapi(Node, Cmd, Args, Fun) -> ?FS_MODULE:bgapi(Node, Cmd, Args, Fun).

-spec bgapi(atom(), atom(), string() | binary(), fun(), list()) -> fs_api_return().
bgapi(Node, Cmd, Args, Fun, CallBackParams) -> ?FS_MODULE:bgapi(Node, Cmd, Args, Fun, CallBackParams).

-spec bgapi(atom(), ne_binary(), list(), atom(), string() | binary(), fun()) -> fs_api_return().
bgapi(Node, UUID, CallBackParams, Cmd, Args, Fun) -> ?FS_MODULE:bgapi(Node, UUID, CallBackParams, Cmd, Args, Fun).

-type event() :: atom() | kz_json:object().
-spec event(atom(), event() | [event()]) -> 'ok' | {'error', 'timeout' | 'exception'}.
-spec event(atom(), event() | [event()], pos_integer()) -> 'ok' | {'error', 'timeout' | 'exception'}.
event(Node, Events) -> ?FS_MODULE:event(Node, Events).
event(Node, Events, Timeout) -> ?FS_MODULE:event(Node, Events, Timeout).

-spec nixevent(atom(), event() | [event()]) -> 'ok'.
nixevent(Node, Event) -> ?FS_MODULE:nixevent(Node, Event).

-spec sendevent(atom(), atom(), list()) -> 'ok'.
sendevent(Node, EventName, Headers) -> ?FS_MODULE:sendevent(Node, EventName, Headers).

-spec sendevent_custom(atom(), atom(), list()) -> 'ok'.
sendevent_custom(Node, SubClassName, Headers) -> ?FS_MODULE:sendevent_custom(Node, SubClassName, Headers).

-spec sendmsg(atom(), ne_binary(), list()) -> fs_api_return().
sendmsg(Node, UUID, Headers) -> ?FS_MODULE:sendmsg(Node, UUID, Headers).

-spec cmd(atom(), ne_binary(), list()) -> fs_api_return().
cmd(Node, UUID, Headers) -> ?FS_MODULE:cmd(Node, UUID, Headers).

-spec cmds(atom(), ne_binary(), list()) -> fs_api_return().
cmds(Node, UUID, Headers) -> ?FS_MODULE:cmds(Node, UUID, Headers).

-spec cast_cmd(atom(), ne_binary(), list()) -> fs_api_return().
cast_cmd(Node, UUID, Headers) -> ?FS_MODULE:cast_cmd(Node, UUID, Headers).

-spec cast_cmds(atom(), ne_binary(), list()) -> fs_api_return().
cast_cmds(Node, UUID, Headers) -> ?FS_MODULE:cast_cmds(Node, UUID, Headers).

-spec config(atom()) -> 'ok'.
config(Node) -> ?FS_MODULE:config(Node).

-spec config(atom(), atom()) -> 'ok'.
config(Node, Section) ->
    ?FS_MODULE:config(Node, Section).

-spec bgapi4(atom(), atom(), string() | binary(), fun(), list()) ->
                    {'ok', binary()} |
                    {'error', 'timeout' | 'exception' | binary()}.
bgapi4(Node, Cmd, Args, Fun, CallBackParams) -> ?FS_MODULE:bgapi4(Node, Cmd, Args, Fun, CallBackParams).
