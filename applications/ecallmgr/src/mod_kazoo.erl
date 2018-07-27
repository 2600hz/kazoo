%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(mod_kazoo).

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

-export([config/1
        ,bgapi4/5
        ]).

-include("ecallmgr.hrl").

-define(TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-type fs_api_ok() :: {'ok', binary()}.
-type fs_api_error():: {'error', 'baduuid' | 'timeout' | 'exception' | binary()}.
-type fs_api_return() :: fs_api_ok() | fs_api_error() | 'ok'.
-export_type([fs_api_ok/0
             ,fs_api_error/0
             ,fs_api_return/0
             ]).

-spec version(atom()) -> fs_api_return().
version(Node) ->
    version(Node, ?TIMEOUT).

-spec version(atom(), pos_integer()) -> fs_api_return().
version(Node, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, 'version', Timeout) of
        'timeout' -> {'error', 'timeout'};
        Result -> Result
    catch
        _E:_R ->
            lager:info("failed to get mod_kazoo version from ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec noevents(atom()) -> fs_api_return().
noevents(Node) ->
    try gen_server:cast({'mod_kazoo', Node}, 'noevents') of
        'ok' -> 'ok'
    catch
        _E:_R ->
            lager:info("failed to send noevents to ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec close(atom()) -> 'ok'.
close(Node) ->
    gen_server:cast({'mod_kazoo', Node}, 'exit').

-spec getpid(atom()) -> fs_api_return().
getpid(Node) ->
    getpid(Node, ?TIMEOUT).

-spec getpid(atom(), pos_integer()) -> fs_api_return().
getpid(Node, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, 'getpid', Timeout) of
        'timeout' -> {'error', 'timeout'};
        Result -> Result
    catch
        _E:_R ->
            lager:info("failed to get mod_kazoo pid from ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec bind(atom(), atom()) -> fs_api_return().
bind(Node, Type) ->
    bind(Node, Type, ?TIMEOUT).

-spec bind(atom(), atom(), pos_integer()) -> fs_api_return().
bind(Node, Type, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, {'bind', Type}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        Result -> Result
    catch
        _E:_R ->
            lager:info("failed to get bind to ~p on ~s: ~p ~p", [Type, Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec fetch_reply(atom(), binary(), atom() | binary(), binary() | string()) -> 'ok'.
fetch_reply(Node, FetchID, Section, Reply) ->
    gen_server:cast({'mod_kazoo', Node}, {'fetch_reply', Section, FetchID, Reply}).

-spec fetch_reply(atom(), binary(), atom() | binary(), binary() | string(), pos_integer() | 'infinity') ->
                         'ok' | {'error', 'baduuid'}.
fetch_reply(Node, FetchID, Section, Reply, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, {'fetch_reply', Section, FetchID, Reply}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        Result -> Result
    catch
        _E:_R ->
            lager:info("failed to send fetch reply to ~s: ~p ~p", [Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec api(atom(), kz_term:text()) -> fs_api_return().
api(Node, Cmd) ->
    api(Node, Cmd, "").

-spec api(atom(), kz_term:text(), kz_term:text()) -> fs_api_return().
api(Node, Cmd, Args) ->
    api(Node, Cmd, Args, ?TIMEOUT).

-spec api(atom(), kz_term:text(), kz_term:text(), timeout()) -> fs_api_return().
api(Node, Cmd, Args, Timeout) when is_atom(Node) ->
    try gen_server:call({'mod_kazoo', Node}, {'api', Cmd, Args}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        Result -> Result
    catch
        _E:_R ->
            lager:info("failed to execute api command ~s on ~s: ~p ~p", [Cmd, Node, _E, _R]),
            {'error', 'exception'}
    end.

%%------------------------------------------------------------------------------
%% @doc Make a background API call to FreeSWITCH. The asynchronous reply is
%% sent to calling process after it is received. This function
%% returns the result of the initial `bgapi' call or `timeout' if FreeSWITCH fails
%% to respond.
%% @end
%%------------------------------------------------------------------------------
-spec bgapi(atom(), atom(), string() | binary()) -> fs_api_return().
bgapi(Node, Cmd, Args) ->
    Self = self(),
    _ = kz_util:spawn(
          fun() ->
                  try gen_server:call({'mod_kazoo', Node}, {'bgapi', Cmd, Args}, ?TIMEOUT) of
                      {'ok', <<"-ERR ", Reason/binary>>} ->
                          Self ! {'api', internal_fs_error(Reason)};
                      {'ok', JobId}=JobOk ->
                          Self ! {'api', JobOk},
                          receive
                              {'bgok', JobId, _}=BgOk -> Self ! BgOk;
                              {'bgerror', JobId, _}=BgError -> Self ! BgError
                          after 360 * ?MILLISECONDS_IN_SECOND ->
                                  Self ! {'bgerror', JobId, 'timeout'}
                          end;
                      {'error', Reason} ->
                          Self ! {'api', {'error', Reason}};
                      'timeout' ->
                          Self ! {'api', {'error', 'timeout'}}
                  catch
                      _E:_R ->
                          lager:info("failed to execute bgapi command ~s on ~s: ~p ~p"
                                    ,[Cmd, Node, _E, _R]),
                          Self ! {'api', {'error', 'exception'}}
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

-spec bgapi(atom(), atom(), string() | binary(), fun()) -> fs_api_return().
bgapi(Node, Cmd, Args, Fun) when is_function(Fun, 2) ->
    Self = self(),
    _ = kz_util:spawn(
          fun() ->
                  try gen_server:call({'mod_kazoo', Node}, {'bgapi', Cmd, Args}, ?TIMEOUT) of
                      {'ok', <<"-ERR ", Reason/binary>>} ->
                          Self ! {'api', internal_fs_error(Reason)};
                      {'ok', JobId}=JobOk ->
                          Self ! {'api', JobOk},
                          receive
                              {'bgok', JobId, Reply} ->
                                  Fun('ok', Reply);
                              {'bgerror', JobId, Reply} ->
                                  Fun('error', Reply)
                          end;
                      {'error', Reason} ->
                          Self ! {'api', {'error', Reason}};
                      'timeout' ->
                          Self ! {'api', {'error', 'timeout'}}
                  catch
                      _E:_R ->
                          lager:info("failed to execute bgapi command ~s on ~s: ~p ~p"
                                    ,[Cmd, Node, _E, _R]),
                          Self ! {'api', {'error', 'exception'}}
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

-spec bgapi(atom(), atom(), string() | binary(), fun(), list()) -> fs_api_return().
bgapi(Node, Cmd, Args, Fun, CallBackParams) when is_function(Fun, 3) ->
    Self = self(),
    _ = kz_util:spawn(
          fun() ->
                  try gen_server:call({'mod_kazoo', Node}, {'bgapi', Cmd, Args}, ?TIMEOUT) of
                      {'ok', <<"-ERR ", Reason/binary>>} ->
                          Self ! {'api', internal_fs_error(Reason)};
                      {'ok', JobId}=JobOk ->
                          Self ! {'api', JobOk},
                          receive
                              {'bgok', JobId, Reply} ->
                                  Fun('ok', Reply, [JobId | CallBackParams]);
                              {'bgerror', JobId, Reply} ->
                                  Fun('error', Reply, [JobId | CallBackParams])
                          end;
                      {'error', Reason} ->
                          Self ! {'api', {'error', Reason}};
                      'timeout' ->
                          Self ! {'api', {'error', 'timeout'}}
                  catch
                      _E:_R ->
                          lager:info("failed to execute bgapi command ~s on ~s: ~p ~p"
                                    ,[Cmd, Node, _E, _R]),
                          Self ! {'api', {'error', 'exception'}}
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

-spec bgapi(atom(), kz_term:ne_binary(), list(), atom(), string() | binary(), fun()) -> fs_api_return().
bgapi(Node, UUID, CallBackParams, Cmd, Args, Fun) when is_function(Fun, 6) ->
    Self = self(),
    _ = kz_util:spawn(
          fun() ->
                  try gen_server:call({'mod_kazoo', Node}, {'bgapi', Cmd, Args}, ?TIMEOUT) of
                      {'ok', <<"-ERR ", Reason/binary>>} ->
                          Self ! {'api', internal_fs_error(Reason)};
                      {'ok', JobId}=JobOk ->
                          Self ! {'api', JobOk},
                          receive
                              {'bgok', JobId, Reply} ->
                                  Fun('ok', Node, UUID, CallBackParams, JobId, Reply);
                              {'bgerror', JobId, Reply} ->
                                  Fun('error', Node, UUID, CallBackParams, JobId, Reply)
                          end;
                      {'error', Reason} ->
                          Self ! {'api', {'error', Reason}};
                      'timeout' ->
                          Self ! {'api', {'error', 'timeout'}}
                  catch
                      _E:_R ->
                          lager:info("failed to execute bgapi command ~s on ~s: ~p ~p"
                                    ,[Cmd, Node, _E, _R]),
                          Self ! {'api', {'error', 'exception'}}
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

-type event() :: atom() | kz_json:object().

-spec event(atom(), event() | [event()]) -> 'ok' | {'error', 'timeout' | 'exception'}.
event(Node, Events) ->
    event(Node, Events, ?TIMEOUT).

-spec event(atom(), event() | [event()], pos_integer()) -> 'ok' | {'error', 'timeout' | 'exception'}.
event(Node, [_|_]=Events, Timeout) ->
    PortOpen = get('port_open'),
    try gen_server:call({'mod_kazoo', Node}, {'event', Events}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        {'ok', {IP, Port}} when PortOpen =:= 'undefined' ->
            put('port_open', 'true'),
            {'ok', IPAddress} = inet_parse:address(IP),
            {'ok', _} = gen_tcp:connect(IPAddress, Port, [{'mode', 'binary'}, {'packet', 2}]),
            'ok';
        {'ok', _} -> 'ok';
        Else ->
            Else
    catch
        _E:_R ->
            lager:info("failed to bind to events on ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end;
event(Node, Event, Timeout) ->
    event(Node, [Event], Timeout).

-spec nixevent(atom(), event() | [event()]) -> 'ok'.
nixevent(Node, [_|_]=Events) ->
    gen_server:cast({'mod_kazoo', Node}, {'nixevent', Events});
nixevent(Node, Event) ->
    nixevent(Node, [Event]).

-spec sendevent(atom(), atom(), list()) -> 'ok'.
sendevent(Node, EventName, Headers) ->
    gen_server:cast({'mod_kazoo', Node}, {'sendevent', EventName, Headers}).

-spec sendevent_custom(atom(), atom(), list()) -> 'ok'.
sendevent_custom(Node, SubClassName, Headers) ->
    gen_server:cast({'mod_kazoo', Node}, {'sendevent', 'CUSTOM',  SubClassName, Headers}).

-spec sendmsg(atom(), kz_term:ne_binary(), list()) -> fs_api_return().
sendmsg(Node, UUID, Headers) ->
    gen_server:call({'mod_kazoo', Node}, {'sendmsg', UUID, Headers}).

-spec config(atom()) -> 'ok'.
config(Node) ->
    gen_server:cast({'mod_kazoo', Node}, {'config', []}).

-spec bgapi4(atom(), atom(), string() | binary(), fun(), list()) ->
                    {'ok', binary()} |
                    {'error', 'timeout' | 'exception' | binary()}.
bgapi4(Node, Cmd, Args, Fun, CallBackParams) ->
    Self = self(),
    _ = kz_util:spawn(
          fun() ->
                  try gen_server:call({'mod_kazoo', Node}, {'bgapi4', Cmd, Args}, ?TIMEOUT) of
                      {'ok', <<"-ERR ", Reason/binary>>} ->
                          Self ! {'api', internal_fs_error(Reason)};
                      {'ok', JobId}=JobOk ->
                          Self ! {'api', JobOk},
                          receive
                              {'bgok', JobId, Reply}
                                when is_function(Fun, 3) -> Fun('ok', Reply, [JobId | CallBackParams]);
                              {'bgerror', JobId, Reply}
                                when is_function(Fun, 3) -> Fun('error', Reply, [JobId | CallBackParams]);
                              {'bgok', JobId, Reply, Data}
                                when is_function(Fun, 4) -> Fun('ok', Reply, Data, [JobId | CallBackParams]);
                              {'bgerror', JobId, Reply, Data}
                                when is_function(Fun, 4) -> Fun('error', Reply, Data, [JobId | CallBackParams]);
                              _Other -> lager:debug("unexpected message from freeswitch : ~p", [_Other])
                          end;
                      {'error', Reason} ->
                          Self ! {'api', {'error', Reason}};
                      'timeout' ->
                          Self ! {'api', {'error', 'timeout'}}
                  catch
                      _E:_R ->
                          lager:info("failed to execute bgapi command ~s on ~s: ~p ~p"
                                    ,[Cmd, Node, _E, _R]),
                          Self ! {'api', {'error', 'exception'}}
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

-spec internal_fs_error(binary()) -> {'error', binary()}.
internal_fs_error(Reason) ->
    Error = kz_binary:strip(binary:replace(Reason, <<"\n">>, <<>>)),
    {'error', Error}.
