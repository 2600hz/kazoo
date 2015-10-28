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
    Context = bh_context:new(SessionPid, SessionId),
    _ = blackhole_tracking:add_socket(Context),
    {'ok', Context}.

recv(_SessionPid, SessionId, {'message', <<>>, Message}, State) ->
    lager:debug("received message ~p on socket ~p", [Message, SessionId]),
    {'ok', State};

recv(_SessionPid, _SessionId, {'event', _Ignore, <<"subscribe">>, SubscriptionJObj}, Context) ->
    Context1 = bh_context:subscribe(Context, SubscriptionJObj),
    case blackhole_util:is_authorized(Context1) of
        'true' ->
            Binding = wh_json:get_value(<<"binding">>, SubscriptionJObj),
            case blackhole_util:get_callback_module(Binding) of
                'undefined' -> blackhole_util:respond_with_error(Context1);
                Module ->
                    _ = blackhole_tracking:update_socket(Context1),
                    blackhole_util:maybe_add_binding_to_listener(Module, Binding, Context1),
                    blackhole_bindings:bind(Binding, Module, 'handle_event', Context1)
            end;
        'false' ->
            blackhole_util:respond_with_authn_failure(Context1)
    end,
    {'ok', Context1};

recv(SessionPid, SessionId, {'event', _Ignore, <<"unsubscribe">>, SubscriptionJObj}, Context) ->
    lager:debug("maybe remove binding for session: ~p. Data: ~p", [SessionId, SubscriptionJObj]),
    Context1 = bh_context:from_json(Context, SubscriptionJObj),
    case blackhole_util:is_authorized(Context1) of
        'true' ->
            Context2 = bh_context:unsubscribe(Context1, SubscriptionJObj),
            case wh_json:get_value(<<"account_id">>, SubscriptionJObj) of
                'undefined' ->
                    lager:debug("remove all bindings for session: ~p", [SessionId]),
                    _ = blackhole_tracking:update_socket(Context2),
                    Filter = fun (A, B, C, D) -> filter_bindings(SessionPid, A, B, C, D) end,
                    blackhole_bindings:filter(Filter);
                AccountId ->
                    Binding = wh_json:get_value(<<"binding">>, SubscriptionJObj),
                    case blackhole_util:get_callback_module(Binding) of
                        'undefined' -> blackhole_util:respond_with_error(Context1);
                        Module ->
                            lager:debug("remove binding for account_id: ~p", [AccountId]),
                            _ = blackhole_tracking:update_socket(Context2),
                            blackhole_bindings:unbind(Binding, Module, 'handle_event', Context1),
                            blackhole_util:maybe_rm_binding_from_listener(Module, Binding, Context1)
                    end
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

close(SessionPid, SessionId, Context) ->
    lager:debug("closing socket ~p", [SessionId]),
    _ = blackhole_tracking:remove_socket(Context),
    Filter = fun (A, B, C, D) -> filter_bindings(SessionPid, A, B, C, D) end,
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
