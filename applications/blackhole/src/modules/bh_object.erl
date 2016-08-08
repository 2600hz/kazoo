%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%% Roman Galeev
%%%-------------------------------------------------------------------
-module(bh_object).

-export([handle_event/2
        ,subscribe/2, unsubscribe/2
        ]).

-include("blackhole.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

-spec doc_binding(ne_binary(), ne_binary()) -> ne_binary().
doc_binding(Type, Action) -> <<Action/binary, ".*.", Type/binary, ".*">>.

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(#bh_context{binding=Binding} = Context, EventJObj) ->
    kz_util:put_callid(EventJObj),
    NormJObj = kz_json:normalize_jobj(kz_json:set_value(<<"Binding">>, Binding, EventJObj)),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), NormJObj).

%% example event: fax.doc_update
-spec event_name(kz_json:object()) -> ne_binary().
event_name(EventJObj) ->
    EventName = kz_api:event_name(EventJObj),
    DocType = kz_json:get_value(<<"Type">>, EventJObj),
    <<DocType/binary, ".", EventName/binary>>.

%% example binding: object.fax.doc_update
-spec subscribe(bh_context:context(), ne_binary()) -> bh_subscribe_result().
subscribe(Context, <<"object.", Args/binary>> = _Binding) ->
    case binary:split(Args, <<".">>, ['global']) of
        [Type, <<"*">>] ->
            case lists:member(Type, ?DOC_TYPES) of
                'false' ->
                    {'error', <<"Unmatched binding">>};
                'true' ->
                    [ subscribe(Context, doc_binding(Type, Action), Action, Type) || Action <- ?DOC_ACTIONS ],
                    {'ok', Context}
            end;
        [Type, Action] ->
            case lists:member(Action, ?DOC_ACTIONS)
                andalso lists:member(Type, ?DOC_TYPES) of
                'false' ->
                    {'error', <<"Unmatched binding">>};
                'true' ->
                    subscribe(Context, doc_binding(Action, Type), Action, Type),
                    {'ok', Context}
            end;
        _Else ->
            {'error', <<"Unmatched binding">>}
    end.

-spec unsubscribe(bh_context:context(), ne_binary()) -> bh_subscribe_result().
unsubscribe(Context, <<"object.", Args/binary>> = _Binding) ->
    case binary:split(Args, <<".">>, ['global']) of
        [Type, <<"*">>] ->
            case lists:member(Type, ?DOC_TYPES) of
                'false' ->
                    {'error', <<"Unmatched binding">>};
                'true' ->
                    [ unsubscribe(Context, doc_binding(Type, Action), Action, Type) || Action <- ?DOC_ACTIONS],
                    {'ok', Context}
            end;
        [Type, Action] ->
            case lists:member(Action, ?DOC_ACTIONS)
                andalso lists:member(Type, ?DOC_TYPES) of
                'false' ->
                    {'error', <<"Unmatched binding">>};
                'true' ->
                    unsubscribe(Context, doc_binding(Action, Type), Action, Type),
                    {'ok', Context}
            end;
        _Else ->
            {'error', <<"Unmatched binding">>}
    end.

-spec bind_options(ne_binary(), list()) -> kz_json:object().
bind_options(AccountId, Keys) ->
    [{'restrict_to', ['doc_updates']}
    ,{'account_id', AccountId}
    ,{'keys', Keys}
    ,'federate'
    ].

-spec subscribe(bh_context:context(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
subscribe(Context, Binding, Action, Type) ->
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    blackhole_bindings:bind(Binding, ?MODULE, 'handle_event', Context),
    blackhole_listener:add_binding('conf', bind_options(AccountId, Keys)).

-spec unsubscribe(bh_context:context(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
unsubscribe(Context, Binding, Type, Action) ->
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    blackhole_bindings:unbind(Binding, ?MODULE, 'handle_event', Context),
    blackhole_listener:remove_binding('conf', bind_options(AccountId, Keys)).
