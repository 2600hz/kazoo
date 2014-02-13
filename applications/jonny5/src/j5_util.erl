%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_util).

-export([send_system_alert/3]).
-export([remove_call_charges/2]).

-include_lib("jonny5.hrl").

-spec remove_call_charges(api_binary(), api_binary()) -> 'ok'.
remove_call_charges('undefined', _) -> 'ok';
remove_call_charges(_, 'undefined') -> 'ok';
remove_call_charges(AccountId, CallId) ->
    case wh_transactions:call_charges(AccountId, CallId, 'false') of
        [] -> 'ok';
        Transactions ->
            _ = wh_transactions:remove(Transactions),
            'ok'
    end.

send_system_alert(_Reason, _JObj, _Limits) ->
    %% TODO: FIX ME!!
    spawn(fun() -> ok end).
