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
        ]).
-export([event/2
         ,event/3
        ]).
-export([nixevent/2]).
-export([sendevent/3
         ,sendevent_custom/3
        ]).
-export([sendmsg/3]).

-define(TIMEOUT, 5000).

version(Node) ->
    version(Node, ?TIMEOUT).
version(Node, Timeout) ->
    gen_server:call({'mod_kazoo', Node}, 'version', Timeout).

noevents(Node) ->
    gen_server:cast({'mod_kazoo', Node}, 'noevents').

close(Node) ->
    gen_server:cast({'mod_kazoo', Node}, 'exit').

getpid(Node) ->
    getpid(Node, ?TIMEOUT).
getpid(Node, Timeout) ->
    gen_server:call({'mod_kazoo', Node}, 'getpid', Timeout).

bind(Node, Type) ->
    bind(Node, Type, ?TIMEOUT).
bind(Node, Type, Timeout) ->
    gen_server:call({'mod_kazoo', Node}, {'bind', Type}, Timeout).

fetch_reply(Node, FetchID, Section, Reply) ->
    gen_server:cast({'mod_kazoo', Node}, {'fetch_reply', Section, FetchID, Reply}).

fetch_reply(Node, FetchID, Section, Reply, Timeout) ->
    gen_server:call({'mod_kazoo', Node}, {'fetch_reply', Section, FetchID, Reply}, Timeout).

api(Node, Cmd) ->
    api(Node, Cmd, "").
api(Node, Cmd, Args) ->
    api(Node, Cmd, Args, ?TIMEOUT).
api(Node, Cmd, Args, Timeout) ->
    gen_server:call({'mod_kazoo', Node}, {'api', Cmd, Args}, Timeout).

%% @doc Make a backgrounded API call to FreeSWITCH. The asynchronous reply is
%% sent to calling process after it is received. This function
%% returns the result of the initial bgapi call or `timeout' if FreeSWITCH fails
%% to respond.
-spec(bgapi(Node :: atom(), Cmd :: atom(), Args :: string() | binary()) -> {'ok', string()} | {'error', any()} | 'timeout').
bgapi(Node, Cmd, Args) ->
    Self = self(),
    spawn(fun() ->
                  case gen_server:call({'mod_kazoo', Node}, {'bgapi', Cmd, Args}, ?TIMEOUT) of
                      {'ok', JobId}=Ok ->
                          Self ! {'api', Ok},
                          receive
                              {'bgok', JobId, _}=Ok -> Ok;
                              {'bgerror', JobId, _}=Error -> Error
                          end;
                      {'error', Reason} ->
                          Self ! {'api', {'error', Reason}};
                      'timeout' ->
                          Self ! {'api', {'error', 'timeout'}}
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

bgapi(Node, Cmd, Args, Fun) ->
    Self = self(),
    spawn(fun() ->
                  case gen_server:call({'mod_kazoo', Node}, {'bgapi', Cmd, Args}, ?TIMEOUT) of
                      {'ok', JobId} ->
                          Self ! {'api', 'ok'},
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
                  end
          end),
    %% get the initial result of the command, NOT the asynchronous response, and
    %% return it
    receive
        {'api', Result} -> Result
    end.

event(Node, Events) ->
    event(Node, Events, ?TIMEOUT).
event(Node, [_|_]=Events, Timeout) ->
    PortOpen = get('port_open'),
    case gen_server:call({'mod_kazoo', Node}, {'event', Events}, Timeout) of
        {'ok', {IP, Port}} when PortOpen =:= 'undefined' ->
            put('port_open', 'true'),
            {'ok', IPAddress} = inet_parse:address(IP),

            io:format("connect to ~p(~p): ~p", [IPAddress, Port, IP]),

            {'ok', Socket} = gen_tcp:connect(IPAddress, Port, [{'mode', 'binary'}, {'packet', 2}]),
            io:format("Opened event socket ~p to port ~p~n", [Socket, Port]),
            'ok';
        {'ok', _} -> 'ok';
        Else ->
            Else
    end;
event(Node, Event, Timeout) ->
    event(Node, [Event], Timeout).

nixevent(Node, [_|_]=Events) ->
    gen_server:cast({'mod_kazoo', Node}, {'nixevent', Events});
nixevent(Node, Event) ->
    nixevent(Node, [Event]).

sendevent(Node, EventName, Headers) ->
    gen_server:cast({'mod_kazoo', Node}, {'sendevent', EventName, Headers}).

sendevent_custom(Node, SubClassName, Headers) ->
    gen_server:cast({'mod_kazoo', Node}, {'sendevent', 'CUSTOM',  SubClassName, Headers}).

sendmsg(Node, UUID, Headers) ->
    gen_server:cast({'mod_kazoo', Node}, {'sendmsg', UUID, Headers}).
