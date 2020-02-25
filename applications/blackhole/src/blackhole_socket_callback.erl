%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(blackhole_socket_callback).

-include("blackhole.hrl").

-export([open/3
        ,recv/2
        ,close/1
        ]).

-define(SESSION_KEY(SessionId), {'session', ?MODULE, SessionId}).

-type bh_return() :: {'ok', bh_context:context()}.

-spec open(pid(), binary(), inet:ip_address()) -> bh_return().
open(Pid, SessionId, IPAddr) ->
    IPBin = kz_network_utils:iptuple_to_binary(IPAddr),
    lager:debug("opening socket (~p) ~p, peer: ~p", [Pid, SessionId, IPBin]),

    Context = bh_context:set_source(bh_context:new(Pid, SessionId), IPBin),

    Routing = <<"blackhole.session.open">>,
    Ctx = blackhole_bindings:fold(Routing, Context),
    {'ok', Ctx}.

-spec recv({binary(), kz_json:object()}, bh_context:context()) -> bh_return() | 'error'.
recv({Action, Payload}, Context) ->
    lager:debug("received action ~s with payload ~s"
               ,[Action
                ,kz_json:encode(kz_json:delete_key(<<"auth_token">>, Payload))
                ]
               ),
    Routines = [fun rate/3
               ,fun authenticate/3
               ,fun validate/3
               ,fun authorize/3
               ,fun limits/3
               ,fun command/3
               ,fun finish/3
               ],
    Ctx = bh_context:from_json(Context, Payload),
    exec(Ctx, Action, Payload, Routines).

exec(Context, _Action, _Payload, []) ->
    {'ok', Context};
exec(Context, Action, Payload, [Fun | Funs]) ->
    lager:debug("executing ~p for ~s with payload ~s"
               ,[Fun, Action, kz_json:encode(kz_json:delete_key(<<"auth_token">>, Payload))]
               ),
    Ctx = Fun(Context, Action, Payload),
    case bh_context:success(Ctx) of
        'true' -> exec(Ctx, Action, Payload, Funs);
        'false' -> send_error(Ctx)
    end.

-spec send_error(bh_context:context()) -> 'error'.
send_error(Context) ->
    SessionPid = bh_context:websocket_pid(Context),
    RequestId = bh_context:req_id(Context),
    Errors = bh_context:errors(Context),

    Data = kz_json:from_list([{<<"errors">>, Errors}]),
    blackhole_data_emitter:reply(SessionPid, RequestId, <<"error">>, Data),
    'error'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec close(bh_context:context()) -> bh_context:context().
close(HandlerOpts) when is_list(HandlerOpts) ->
    lager:info("closing connection early");
close(Context) ->
    lager:info("closing session ~s", [bh_context:websocket_session_id(Context)]),
    Routing = <<"blackhole.session.close">>,
    blackhole_bindings:fold(Routing, Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
rate(Context, _Action, _Payload) ->
    Bucket = bh_context:websocket_session_id(Context),
    case kz_buckets:consume_token(?APP_NAME, Bucket) of
        'true' -> Context;
        'false' ->
            Msg = io_lib:format("rate limiting threshold hit for ~s!", [Bucket]),
            lager:warning(Msg),
            bh_context:add_error(Context, Msg)
    end.

authenticate(Context, Action, Payload) ->
    case bh_context:is_authenticated(Context) of
        'true' -> Context;
        'false' ->
            Routing = <<"blackhole.authenticate.", Action/binary>>,
            handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload]))
    end.

validate(Context, Action, Payload) ->
    Routing = <<"blackhole.validate.", Action/binary>>,
    handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])).

authorize(Context, Action, Payload) ->
    Routing = <<"blackhole.authorize.", Action/binary>>,
    handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])).

limits(Context, Action, Payload) ->
    Routing = <<"blackhole.limits.", Action/binary>>,
    handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])).

command(Context, Action, Payload) ->
    Routing = <<"blackhole.command.", Action/binary>>,
    Ctx = handle_result(Context, blackhole_bindings:map(Routing, [Context, Payload])),
    case bh_context:success(Ctx) of
        'true' ->
            SessionPid = bh_context:websocket_pid(Ctx),
            RequestId = bh_context:req_id(Ctx),
            Data = bh_context:resp_data(Ctx),
            blackhole_data_emitter:reply(SessionPid, RequestId, <<"success">>, Data),
            Ctx;
        'false' -> Ctx
    end.

finish(Context, Action, _Payload) ->
    Routing = <<"blackhole.finish.", Action/binary>>,
    blackhole_bindings:fold(Routing, Context).

handle_result(Context, []) -> Context;
handle_result(Context, Res) ->
    case blackhole_bindings:failed(Res) of
        [Ctx | _] -> Ctx;
        _ -> handle_success(Context, Res)
    end.

handle_success(Context, Res) ->
    case blackhole_bindings:succeeded(Res) of
        [Ctx | _] -> Ctx;
        _ -> Context
    end.
