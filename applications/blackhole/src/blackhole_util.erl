%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%% Roman Galeev
%%%-------------------------------------------------------------------
-module(blackhole_util).
-include("blackhole.hrl").

-export([handle_event/3]).
-export([send_error/3, send_success/3, send_success/2]).
-export([ensure_value/2, ensure_true/2]).
-export([get_descendants/1]).

-spec handle_event(bh_context:context(), kz_json:object(), ne_binary()) -> 'ok'.
handle_event(#bh_context{binding=Binding} = Context, EventJObj, EventName) ->
    kz_util:put_callid(EventJObj),
    NormJObj = kz_json:normalize_jobj(kz_json:set_value(<<"Binding">>, Binding, EventJObj)),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), EventName, NormJObj).

send_error(WsPid, Message, Detail) ->
    JObj = kz_json:from_list([{<<"status">>, <<"error">>}
                             ,{<<"error">>, Message}
                             ,{<<"detail">>, Detail}]),
    blackhole_data_emitter:msg(WsPid, JObj).

send_success(WsPid, Message, Detail) ->
    JObj = kz_json:from_list([{<<"status">>, <<"success">>}
                             ,{<<"message">>, Message}
                             ,{<<"detail">>, Detail}]),
    blackhole_data_emitter:msg(WsPid, JObj).

send_success(WsPid, Message) ->
    JObj = kz_json:from_list([{<<"status">>, <<"success">>}
                             ,{<<"message">>, Message}]),
    blackhole_data_emitter:msg(WsPid, JObj).

ensure_value('undefined', Error) -> erlang:error(Error);
ensure_value(V, _) -> V.

ensure_true('true', _) -> 'true';
ensure_true(_, Error) -> erlang:error(Error).

-spec get_descendants(ne_binary()) -> ne_binaries().
get_descendants(?MATCH_ACCOUNT_RAW(AccountId)) ->
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_descendants">>, ViewOptions) of
        {'ok', JObjs} ->
            [Id || JObj <- JObjs,
                   (Id = kz_doc:id(JObj)) =/= AccountId
            ];
        {'error', _R} ->
            lager:debug("unable to get descendants of ~s: ~p", [AccountId, _R]),
            []
    end.
