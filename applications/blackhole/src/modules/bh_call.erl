%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(bh_call).
-include("blackhole.hrl").

-export([handle_event/2, validate_ws_message/3, authorize_ws_message/2, handle_ws_message/2]).

-define(LISTEN_TO, [
                    <<"CHANNEL_CREATE">>, <<"CHANNEL_ANSWER">>, <<"CHANNEL_DESTROY">>, <<"CHANNEL_BRIDGE">>
                   ,<<"PARK_PARKED">>, <<"PARK_RETRIEVED">>, <<"PARK_ABANDONED">>
                   ]).

-record(state, {account_id :: ne_binary()
                ,call_event
                ,call_id
                ,context :: bh_context:context()
        }).

-spec validate_ws_message(ne_binary(), bh_context:context(), kz_json:object()) -> #state{}.
validate_ws_message(_Event, #bh_context{} = Context, JObj) ->
    Binding = kz_json:get_value(<<"binding">>, JObj),
    [Event, CallId] = binary:split(Binding, <<".">>, ['global']),
    validate_binding(Event, CallId),
    AccountId = blackhole_util:get_account(Context, JObj),
    #state{account_id=AccountId, call_event=Event, call_id=CallId, context=Context}.

-spec authorize_ws_message(ne_binary(), #state{}) -> #state{}.
authorize_ws_message(_Event, #state{account_id=AccountId, context=#bh_context{auth_account_id=AuthAccountId}} = State) ->
    % XXX: check actual tree here
    case AccountId of
        AuthAccountId -> 'ok';
        _ -> erlang:error('not_authorized')
    end,
    State.

-spec validate_binding(ne_binary(), ne_binary()) -> 'ok'.
validate_binding(<<"*">>, <<"*">>) -> 'ok';
validate_binding(Event, <<"*">>) -> 
    case lists:member(Event, ?LISTEN_TO) of
        'true' -> 'ok';
        'false' -> erlang:error('invalid_call_event')
    end;
validate_binding(_Event, _CallId) ->
    erlang:error('invalid_call_binding').

-spec handle_event(#state{}, kz_json:object()) -> 'ok'.
handle_event(#state{context=Context}, EventJObj) ->
    'true' = kapi_call:event_v(EventJObj),
    blackhole_util:handle_event(Context, EventJObj, event_name(EventJObj)).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

-spec handle_ws_message(ne_binary(), #state{}) -> #state{}.
handle_ws_message(<<"subscribe">>, #state{account_id=AccountId, call_event = <<"*">>, call_id = <<"*">>} = State) ->
    add_call_binding(AccountId, State, ?LISTEN_TO);
handle_ws_message(<<"subscribe">>, #state{account_id=AccountId, call_event=Event, call_id = <<"*">>} = State) ->
    add_call_binding(AccountId, State, [Event]);
handle_ws_message(<<"unsubscribe">>, #state{account_id=AccountId, call_event = <<"*">>, call_id = <<"*">>} = State) ->
    rm_call_binding(AccountId, State, ?LISTEN_TO);
handle_ws_message(<<"unsubscribe">>, #state{account_id=AccountId, call_event=Event, call_id = <<"*">>} = State) ->
    rm_call_binding(AccountId, State, [Event]).

-spec add_call_binding(ne_binary(), bh_context:context(), [ne_binary()]) -> #state{}.
add_call_binding(_AccountId, State, []) -> State;
add_call_binding(AccountId, State, [Event | Events]) ->
    blackhole_bindings:bind(<<"call.", AccountId/binary, ".", Event/binary, ".*">>, ?MODULE, 'handle_event', State),
    blackhole_listener:add_call_binding(AccountId, Event),
    add_call_binding(AccountId, State, Events).

-spec rm_call_binding(ne_binary(), bh_context:context(), [ne_binary()]) -> #state{}.
rm_call_binding(_AccountId, State, []) -> State;
rm_call_binding(AccountId, State, [Event | Events]) ->
    blackhole_bindings:unbind(<<"call.", AccountId/binary, ".", Event/binary, ".*">>, ?MODULE, 'handle_event', State),
    blackhole_listener:remove_call_binding(AccountId, Event),
    rm_call_binding(AccountId, State, Events).
