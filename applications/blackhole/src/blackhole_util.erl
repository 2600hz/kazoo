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
-module(blackhole_util).

-include("blackhole.hrl").

-export([get_callback_module/1]).
-export([handle_event/3]).
-export([send_error/3, send_success/3, send_success/2]).
-export([get_account/2]).

-spec special_bindings(ne_binary()) -> ne_binary().
special_bindings(<<"doc_edited">>) -> <<"object">>;
special_bindings(<<"doc_created">>) -> <<"object">>;
special_bindings(<<"doc_deleted">>) -> <<"object">>;
special_bindings(M) -> M.

-spec get_callback_module(ne_binary()) -> atom().
get_callback_module(Binding) ->
    case binary:split(Binding, <<".">>) of
        [M|_] ->
            Mod = special_bindings(M),
            try
                kz_util:to_atom(<<"bh_", Mod/binary>>, 'true')
            catch
                'error':'badarg' -> erlang:error('unknown_handler_module')
            end;
        _ ->
            erlang:error('bad_binding_format')
    end.

-spec handle_event(bh_context:context(), kz_json:object(), ne_binary()) -> 'ok'.
handle_event(#bh_context{binding=Binding} = Context, EventJObj, EventName) ->
    kz_util:put_callid(EventJObj),
    NormJObj = kz_json:normalize_jobj(kz_json:set_value(<<"Binding">>, Binding, EventJObj)),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), EventName, NormJObj).

send_error(Context, Message, Detail) ->
    JObj = kz_json:from_list([{<<"status">>, <<"error">>}
                             ,{<<"error">>, Message}
                             ,{<<"detail">>, Detail}]),
    blackhole_data_emitter:msg(bh_context:websocket_pid(Context), JObj).

send_success(Context, Message, Detail) ->
    JObj = kz_json:from_list([{<<"status">>, <<"success">>}
                             ,{<<"message">>, Message}
                             ,{<<"detail">>, Detail}]),
    blackhole_data_emitter:msg(bh_context:websocket_pid(Context), JObj).

send_success(Context, Message) ->
    JObj = kz_json:from_list([{<<"status">>, <<"success">>}
                             ,{<<"message">>, Message}]),
    blackhole_data_emitter:msg(bh_context:websocket_pid(Context), JObj).

get_account(#bh_context{auth_account_id=_AuthAccountId}, JObj) ->
    ensure_value(kz_json:get_value(<<"account_id">>, JObj), 'no_account_id').

ensure_value('undefined', Error) -> erlang:error(Error);
ensure_value(V, _) -> V.
