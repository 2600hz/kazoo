%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------

-module(blackhole_socket_callback).

-include("blackhole.hrl").

-export([open/3
        ,recv/4
        ,close/3
        ]).

open(SessionPid, SessionId, _Opts) ->
    lager:debug("opening socket ~p", [SessionId]),
    {'ok', bh_context:new(SessionPid, SessionId)}.

recv(_SessionPid, SessionId, {'message', <<>>, Message}, State) ->
    lager:debug("received message ~p on socket ~p", [Message, SessionId]),
    {'ok', State};

recv(_SessionPid, _SessionId, {'event', _Ignore, <<"subscribe">>, SubscriptionJObj}, Context) ->
    Context1 = bh_context:from_subscription(Context, SubscriptionJObj),
    case blackhole_util:is_authorized(Context1) of
        'true' ->
            Binding = bh_context:binding(Context1),
            case blackhole_util:get_callback_module(Binding) of
                'undefined' -> blackhole_util:respond_with_error(Context1);
                Module ->
                    blackhole_util:maybe_add_binding_to_listener(Module, Binding, Context1),
                    blackhole_bindings:bind(Binding, Module, 'handle_event', Context1)
            end;
        'false' ->
            blackhole_util:respond_with_authn_failure(Context1)
    end,
    {'ok', Context1};

recv(_SessionPid, _SessionId, {'event', _Ignore, _Event, _Data}, Context) ->
    lager:debug("received event: ~p on socket ~p with data payload", [_Event, _SessionId]),
    {'ok', Context};
recv(_SessionPid, SessionId, Message, Context) ->
    lager:info("receive unknown message ~p on socket ~p", [Message, SessionId]),
    {'ok', Context}.

close(SessionPid, SessionId, _Context) ->
    lager:debug("closing socket ~p", [SessionId]),
    Filter = fun (_1, _2, _3, _4) -> filter_bindings(SessionPid, _1, _2, _3, _4) end,
    blackhole_bindings:filter(Filter),
    'ok'.

filter_bindings(SessionPid, Binding, _Module, _Function, BindingContext) ->
    case bh_context:is_context(BindingContext) of
        'false' -> 'true';
        'true' ->
            case bh_context:websocket_pid(BindingContext) =:= SessionPid of
                'false' -> 'true';
                'true' ->
                    _ = wh_util:spawn('blackhole_util', 'remove_binding', [Binding, BindingContext]),
                    'false'
            end
    end.
