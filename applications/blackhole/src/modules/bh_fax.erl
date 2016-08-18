%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(bh_fax).

-export([handle_event/2
        ,handle_object_event/2
        ,subscribe/2
        ,unsubscribe/2
        ]).

-include("blackhole.hrl").

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    'true' = kapi_fax:status_v(EventJObj),
    blackhole_util:handle_event(Context, EventJObj, <<"fax.status">>).

-spec handle_object_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_object_event(Context, EventJObj) ->
    blackhole_util:handle_event(Context, EventJObj, <<"fax.object">>).

-spec subscribe(bh_context:context(), ne_binary()) -> bh_subscribe_result().
subscribe(#bh_context{account_id=AccountId}=Context, <<"fax.status.", FaxId/binary>> = Binding) ->
    blackhole_listener:add_binding('fax', fax_status_bind_options(AccountId, FaxId)),
    blackhole_bindings:bind(Binding, ?MODULE, 'handle_event', Context),
    {'ok', Context};
%% listen_to: doc_edited.$modb.fax.$fax_id
subscribe(#bh_context{account_id=AccountId}=Context, <<"fax.object.", Action/binary>>) ->
    blackhole_listener:add_binding('conf', fax_object_bind_options(AccountId, Action)),
    blackhole_bindings:bind(fax_object_bind_key(AccountId, Action), ?MODULE, 'handle_object_event', Context),
    {'ok', Context};
subscribe(_Context, _Binding) ->
    {'error', <<"Unmatched binding">>}.

-spec unsubscribe(bh_context:context(), ne_binary()) -> bh_subscribe_result().
unsubscribe(#bh_context{account_id=AccountId}=Context, <<"fax.status.", FaxId/binary>> = Binding) ->
    blackhole_listener:remove_binding('fax', fax_status_bind_options(AccountId, FaxId)),
    blackhole_bindings:unbind(Binding, ?MODULE, 'handle_event', Context),
    {'ok', Context};
unsubscribe(#bh_context{account_id=AccountId}=Context, <<"fax.object.", Action/binary>>) ->
    blackhole_listener:remove_binding('conf', fax_object_bind_options(AccountId, Action)),
    blackhole_bindings:unbind(fax_object_bind_key(AccountId, Action), ?MODULE, 'handle_object_event', Context),
    {'ok', Context};
unsubscribe(_Context, _Binding) ->
    {'error', <<"Unmatched binding">>}.

-spec fax_status_bind_options(ne_binary(), ne_binary()) -> kz_proplist().
fax_status_bind_options(AccountId, FaxId) ->
    [{'restrict_to', ['status']}
    ,{'account_id', AccountId}
    ,{'fax_id', FaxId}
    ,'federate'
    ].

-spec fax_object_bind_options(ne_binary(), ne_binary()) -> kz_json:object().
fax_object_bind_options(AccountId, Action) ->
    MODB = kazoo_modb:get_modb(AccountId),
    [{'keys', [[{'action', Action}, {'db', MODB}, {'doc_type', <<"fax">>}]]}
    ,'federate'
    ].

-spec fax_object_bind_key(ne_binary(), ne_binary()) -> ne_binary().
fax_object_bind_key(AccountId, Action) ->
    MODB = kazoo_modb:get_modb(AccountId),
    <<Action/binary, ".", MODB/binary, ".fax.*">>.
