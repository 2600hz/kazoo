%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
-export([fetch_reply/1]).
-export([api/2
        ,api/3
        ,api/4
        ]).
-export([bgapi/3
        ,bgapi/4
        ,bgapi/5
        ,bgapi/6
        ]).
-export([json_api/2
        ,json_api/3
        ,json_api/4
        ,json_api/5
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

-export([sync_channel/2]).
-export([no_legacy/1]).

-include("ecallmgr.hrl").

-define(TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).

-spec version(atom()) -> freeswitch:fs_api_return().
version(Node) ->
    version(Node, ?TIMEOUT).

-spec version(atom(), pos_integer()) -> freeswitch:fs_api_return().
version(Node, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, 'version', Timeout) of
        'timeout' -> {'error', 'timeout'};
        Result -> Result
    catch
        exit:{{nodedown, _Node}, _}:_ -> {error, nodedown};
        _E:_R:_ ->
            lager:info("failed to get mod_kazoo version from ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec noevents(atom()) -> freeswitch:fs_api_return().
noevents(Node) ->
    try gen_server:cast({'mod_kazoo', Node}, 'noevents') of
        'ok' -> 'ok'
    catch
        _E:_R:_ ->
            lager:info("failed to send noevents to ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec close(atom()) -> 'ok'.
close(Node) ->
    gen_server:cast({'mod_kazoo', Node}, 'exit').

-spec getpid(atom()) -> freeswitch:fs_api_return().
getpid(Node) ->
    getpid(Node, ?TIMEOUT).

-spec getpid(atom(), pos_integer()) -> freeswitch:fs_api_return().
getpid(Node, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, 'getpid', Timeout) of
        'timeout' -> {'error', 'timeout'};
        Result -> Result
    catch
        _E:_R:_ ->
            lager:info("failed to get mod_kazoo pid from ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec bind(atom(), atom()) -> freeswitch:fs_api_return().
bind(Node, Type) ->
    bind(Node, Type, ?TIMEOUT).

-spec bind(atom(), atom(), pos_integer()) -> freeswitch:fs_api_return().
bind(Node, Type, Timeout) ->
    try gen_server:call({'mod_kazoo', Node}, {'bind', Type}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        Result -> Result
    catch
        _E:_R:_ ->
            lager:info("failed to get bind to ~p on ~s: ~p ~p", [Type, Node, _E, _R]),
            {'error', 'exception'}
    end.

-spec fetch_reply(map()) -> 'ok' | {'ok', any()} | {'error', any()}.
fetch_reply(#{node := Node, section := Section, fetch_id := FetchID, reply := Reply, timeout := Timeout})
  when Timeout >= 0 ->
    try gen_server:call({'mod_kazoo', Node}, {'fetch_reply', Section, FetchID, Reply}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        {'ok', <<"+OK ", Result/binary>>} when Result =/= <<>> -> {'ok', Result};
        {'ok', <<"+OK", _/binary>>} -> 'ok';
        Result -> Result
    catch
        _E:_R:_ ->
            lager:info("failed to send fetch reply to ~s: ~p ~p", [Node, _E, _R]),
            {'error', 'exception'}
    end;
fetch_reply(#{node := Node, section := Section, fetch_id := FetchID, reply := Reply}) ->
    gen_server:cast({'mod_kazoo', Node}, {'fetch_reply', Section, FetchID, Reply}).

api_result(Result, 'undefined') -> Result;
api_result(Result, Bin) ->
    case kz_binary:strip_left(kz_binary:strip_right(Bin, <<"\n">>), $\s) of
        <<>> when Result =:= 'error' -> {error, 'failed'};
        <<"true">> -> {Result, true};
        <<"false">> -> {Result, false};
        <<>> -> ok;
        Msg -> {Result, maybe_number(Msg, byte_size(Msg))}
    end.

maybe_number(Msg, Size)
  when Size < 10 ->
    Float = (catch erlang:binary_to_float(Msg)),
    Int = (catch erlang:binary_to_integer(Msg)),
    case {is_number(Float), is_number(Int)} of
        {'true', _} -> Float;
        {_, 'true'} -> Int;
        _ -> Msg
    end;
maybe_number(Msg, _Size) -> Msg.

-spec api(atom(), kz_term:text()) -> freeswitch:fs_api_return().
api(Node, Cmd) ->
    api(Node, Cmd, "").

-spec api(atom(), kz_term:text(), kz_term:text()) -> freeswitch:fs_api_return().
api(Node, Cmd, Args) ->
    api(Node, Cmd, Args, ?TIMEOUT).

-spec api(atom(), kz_term:text(), kz_term:text(), timeout()) -> freeswitch:fs_api_return().
api(Node, Cmd, Args, Timeout) when is_atom(Node) ->
    try gen_server:call({'mod_kazoo', Node}, {'api', Cmd, Args}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR", Reason/binary>>} -> api_result('error', Reason);
        {'ok', <<"+OK", Result/binary>>} -> api_result('ok', Result);
        {'ok', Result} -> api_result('ok', Result);
        'error' -> {'error', 'failed'};
        {'error', _} = Err -> Err;
        Result -> api_result('ok', Result)
    catch
        _E:_R:_ ->
            lager:info("failed to execute api command ~s on ~s: ~p ~p", [Cmd, Node, _E, _R]),
            {'error', 'exception'}
    end.


-spec json_api(atom(), kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:api_object()}) ->
                      freeswitch:fs_json_api_return().
json_api(Node, {Cmd, Args}) ->
    json_api(Node, 'undefined', Cmd, Args, ?TIMEOUT);
json_api(Node, Cmd) ->
    json_api(Node, 'undefined', Cmd, 'undefined', ?TIMEOUT).

-spec json_api(atom(), kz_term:api_ne_binary(), kz_term:text()) ->
                      freeswitch:fs_json_api_return().
json_api(Node, UUID, Cmd) ->
    json_api(Node, UUID, Cmd, 'undefined', ?TIMEOUT).

-spec json_api(atom(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:api_object()) ->
                      freeswitch:fs_json_api_return().
json_api(Node, UUID, Cmd, Args) ->
    json_api(Node, UUID, Cmd, Args, ?TIMEOUT).

-spec json_api(atom(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:api_object() | binary(), timeout()) ->
                      freeswitch:fs_json_api_return().
json_api(Node, UUID, Cmd, 'undefined', Timeout) ->
    json_api(Node, UUID, Cmd, <<>>, Timeout);
json_api(Node, UUID, Cmd, Data, Timeout) when is_atom(Node) ->
    Params = [{<<"command">>, Cmd}
             ,{<<"uuid">>, UUID}
             ,{<<"data">>, Data}
             ],
    JObj = kz_json:from_list(Params),
    try gen_server:call({'mod_kazoo', Node}, {'json_api', kz_json:encode(JObj)}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'error', {'parse_error', _Where}} = Err -> Err;
        {'ok', Result} -> json_api_result('ok', Result);
        {'error', Result} -> json_api_result('error', Result)
    catch
        _E:_R:_ ->
            lager:info("failed to execute api command ~s on ~s: ~p ~p", [Cmd, Node, _E, _R]),
            {'error', 'exception'}
    end.

json_api_result(Result, 'undefined') -> Result;
json_api_result('error', Bin) ->
    JObj = kz_json:decode(Bin),
    {'error', kz_json:get_first_defined([<<"error">>, <<"message">>], JObj)};
json_api_result('ok', Bin) ->
    JObj = kz_json:decode(Bin),
    case kz_json:get_atom_value(<<"status">>, JObj) of
        'success' -> {'ok', kz_json:get_json_value(<<"response">>, JObj)};
        'error' -> {'error', kz_json:get_first_defined([<<"error">>, <<"message">>], JObj)}
    end.

%%------------------------------------------------------------------------------
%% @doc Make a background API call to FreeSWITCH. The asynchronous reply is
%% sent to calling process after it is received. This function
%% returns the result of the initial `bgapi' call or `timeout' if FreeSWITCH fails
%% to respond.
%% @end
%%------------------------------------------------------------------------------
-spec bgapi(atom(), atom(), string() | binary()) -> freeswitch:fs_api_return().
bgapi(Node, Cmd, Args) ->
    Self = self(),
    _ = kz_process:spawn(
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
                      _E:_R:_ ->
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

-spec bgapi(atom(), atom(), string() | binary(), fun()) -> freeswitch:fs_api_return().
bgapi(Node, Cmd, Args, Fun) when is_function(Fun, 2) ->
    Self = self(),
    _ = kz_process:spawn(
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
                      _E:_R:_ ->
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

-spec bgapi(atom(), atom(), string() | binary(), fun(), list()) -> freeswitch:fs_api_return().
bgapi(Node, Cmd, Args, Fun, CallBackParams) when is_function(Fun, 3) ->
    Self = self(),
    _ = kz_process:spawn(
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
                      _E:_R:_ ->
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

-spec bgapi(atom(), kz_term:ne_binary(), list(), atom(), string() | binary(), fun()) -> freeswitch:fs_api_return().
bgapi(Node, UUID, CallBackParams, Cmd, Args, Fun) when is_function(Fun, 6) ->
    Self = self(),
    _ = kz_process:spawn(
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
                      _E:_R:_ ->
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
    PackeSize = freeswitch:event_stream_framing(Node),
    try gen_server:call({'mod_kazoo', Node}, {'event', Events}, Timeout) of
        'timeout' -> {'error', 'timeout'};
        {'ok', <<"-ERR ", Reason/binary>>} -> internal_fs_error(Reason);
        {'ok', {IP, Port}} when PortOpen =:= 'undefined' ->
            put('port_open', 'true'),
            {'ok', IPAddress} = inet_parse:address(IP),
            {'ok', _} = gen_tcp:connect(IPAddress, Port, [{'mode', 'binary'}, {'packet', PackeSize}]),
            'ok';
        {'ok', _} -> 'ok';
        Else ->
            Else
    catch
        _E:_R:_ ->
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

-spec sendmsg(atom(), kz_term:ne_binary(), list()) -> freeswitch:fs_api_return().
sendmsg(Node, UUID, Headers) ->
    gen_server:call({'mod_kazoo', Node}, {'sendmsg', UUID, Headers}).

-spec config(atom()) -> 'ok'.
config(Node) ->
    gen_server:cast({'mod_kazoo', Node}, {'config', []}).

-spec config(atom(), atom()) -> 'ok'.
config(Node, Section) ->
    gen_server:cast({'mod_kazoo', Node}, {'config', [Section]}).

-spec bgapi4(atom(), atom(), string() | binary(), fun(), list()) ->
                    {'ok', binary()} |
                    {'error', 'timeout' | 'exception' | binary()}.
bgapi4(Node, Cmd, Args, Fun, CallBackParams) ->
    Self = self(),
    _ = kz_process:spawn(fun bgapi4/6, [Node, Cmd, Args, Fun, CallBackParams, Self]),
    receive
        {'api', Result} -> Result
    end.

bgapi4(Node, Cmd, Args, Fun, CallBackParams, Self) ->
    try gen_server:call({'mod_kazoo', Node}, {'bgapi4', Cmd, Args}, ?TIMEOUT) of
        {'ok', <<"-ERR ", Reason/binary>>} ->
            Self ! {'api', internal_fs_error(Reason)};
        {'ok', <<"+OK ", JobId/binary>>} ->
            Self ! {'api', {ok, JobId}},
            bgapi4_result(JobId, Fun, CallBackParams);
        {'ok', JobId}=JobOk ->
            Self ! {'api', JobOk},
            bgapi4_result(JobId, Fun, CallBackParams);
        {'error', Reason} ->
            Self ! {'api', {'error', Reason}};
        'timeout' ->
            Self ! {'api', {'error', 'timeout'}}
    catch
        _E:_R:_ ->
            lager:info("failed to execute bgapi command ~s on ~s: ~p ~p"
                      ,[Cmd, Node, _E, _R]),
            Self ! {'api', {'error', 'exception'}}
    end.

bgapi4_result(JobId, Fun, CallBackParams) ->
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
    end.


-spec internal_fs_error(binary()) -> {'error', binary()}.
internal_fs_error(Reason) ->
    Error = kz_binary:strip(binary:replace(Reason, <<"\n">>, <<>>)),
    {'error', Error}.

-spec cmd(atom(), kz_term:ne_binary(), list()) -> freeswitch:fs_api_return().
cmd(Node, UUID, Command) ->
    gen_server:call({'mod_kazoo', Node}, {'command', UUID, Command}).

-spec cmds(atom(), kz_term:ne_binary(), list()) -> freeswitch:fs_api_return().
cmds(Node, UUID, Commands) ->
    gen_server:call({'mod_kazoo', Node}, {'commands', UUID, Commands}).

-spec cast_cmd(atom(), kz_term:ne_binary(), list()) -> freeswitch:fs_api_return().
cast_cmd(Node, UUID, Command) ->
    gen_server:cast({'mod_kazoo', Node}, {'command', UUID, Command}).

-spec cast_cmds(atom(), kz_term:ne_binary(), list()) -> freeswitch:fs_api_return().
cast_cmds(Node, UUID, Commands) ->
    gen_server:cast({'mod_kazoo', Node}, {'commands', UUID, Commands}).

-spec sync_channel(atom(), kz_term:ne_binary()) -> 'ok'.
sync_channel(Node, UUID) ->
    Headers = [{<<"Call-ID">>, UUID}
              ,{<<"Event-PID">>, kz_term:to_binary(self())}
              ,{<<"Event-Node">>, kz_term:to_binary(node())}
              ],
    gen_server:cast({'mod_kazoo', Node}, {'sendevent', 'CUSTOM',  'CHANNEL_SYNC', Headers}).

-spec no_legacy(atom()) -> 'ok' | {'error', 'timeout' | 'exception'}.
no_legacy(Node) ->
    try gen_server:call({'mod_kazoo', Node}, 'no_legacy') of
        'timeout' -> {'error', 'timeout'};
        Result -> Result
    catch
        exit:{{nodedown, _Node}, _}:_ -> {error, nodedown};
        _E:_R:_ ->
            lager:info("failed to set mod_kazoo no_legacy on ~s: ~p ~p"
                      ,[Node, _E, _R]),
            {'error', 'exception'}
    end.
