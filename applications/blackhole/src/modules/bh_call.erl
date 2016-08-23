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

-export([handle_amqp_event/2, init/0]).
-export([validate/4, execute/4]).

-define(LISTEN_TO, [
                    <<"CHANNEL_CREATE">>, <<"CHANNEL_ANSWER">>, <<"CHANNEL_DESTROY">>, <<"CHANNEL_BRIDGE">>
                   ,<<"PARK_PARKED">>, <<"PARK_RETRIEVED">>, <<"PARK_ABANDONED">>
                   ]).

init() ->
    blackhole_bindings:bind(<<"command.subscribe.call.validate">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"command.subscribe.call.execute">>, ?MODULE, 'execute').

validate(Context=#bh_context{}, JMsg, <<"*">>, <<"*">>) ->
    validate_msg(Context, JMsg);
validate(Context=#bh_context{}, JMsg, Event, <<"*">>) ->
    case lists:member(Event, ?LISTEN_TO) of
        'true' -> validate_msg(Context, JMsg);
        'false' -> {'error', 'invalid_call_event'}
    end;
validate(_Context, _JMsg, _Event, _CallId) -> {'error', 'invalid_binding'}.

execute(#bh_call{account_id=AccountId}=State, <<"subscribe">>, <<"*">>, <<"*">>) ->
    add_call_binding(AccountId, State, ?LISTEN_TO);
execute(#bh_call{account_id=AccountId}=State, <<"subscribe">>, Event, <<"*">>) ->
    add_call_binding(AccountId, State, [Event]);
execute(#bh_call{account_id=AccountId}=State, <<"unsubscribe">>, <<"*">>, <<"*">>) ->
    rm_call_binding(AccountId, State, ?LISTEN_TO);
execute(#bh_call{account_id=AccountId}=State, <<"unsubscribe">>, Event, <<"*">>) ->
    rm_call_binding(AccountId, State, [Event]).

validate_msg(#bh_context{websocket_pid=WsPid}, JMsg) ->
    AccountId = blackhole_util:ensure_value(kz_json:get_value(<<"account_id">>, JMsg), 'no_account_id'),
    #bh_call{account_id=AccountId, ws_pid=WsPid}.

-spec handle_amqp_event(#bh_call{}, kz_json:object()) -> 'ok'.
handle_amqp_event(#bh_call{ws_pid=WsPid}, EventJObj) ->
    'true' = kapi_call:event_v(EventJObj),
    blackhole_util:handle_event(WsPid, EventJObj, event_name(EventJObj)).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

-spec add_call_binding(ne_binary(), #bh_call{}, [ne_binary()]) -> #bh_call{}.
add_call_binding(_AccountId, State=#bh_call{}, []) -> State;
add_call_binding(AccountId, State=#bh_call{}, [Event | Events]) ->
    blackhole_bindings:bind(<<"call.", AccountId/binary, ".", Event/binary, ".*">>, ?MODULE, 'handle_amqp_event', State),
    blackhole_listener:add_call_binding(AccountId, Event),
    add_call_binding(AccountId, State, Events).

-spec rm_call_binding(ne_binary(), #bh_call{}, [ne_binary()]) -> #bh_call{}.
rm_call_binding(_AccountId, State=#bh_call{}, []) -> State;
rm_call_binding(AccountId, State, [Event | Events]) ->
    blackhole_bindings:unbind(<<"call.", AccountId/binary, ".", Event/binary, ".*">>, ?MODULE, 'handle_amqp_event', State),
    blackhole_listener:remove_call_binding(AccountId, Event),
    rm_call_binding(AccountId, State, Events).
