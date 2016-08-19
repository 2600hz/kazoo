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
-module(bh_skel).
-include("blackhole.hrl").

-export([handle_event/2, subscribe/3, unsubscribe/3]).

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    blackhole_util:handle_event(Context, EventJObj, event_name(EventJObj)).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

%% Binding must match module name
-spec subscribe(bh_context:context(), ne_binary(), kz_json:object()) -> bh_subscribe_result().
subscribe(Context, <<"skel.", _Args/binary>> = _Binding, _JObj) ->
    {'ok', Context};
subscribe(_Context, _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>}.

-spec unsubscribe(bh_context:context(), ne_binary(), kz_json:object()) -> bh_subscribe_result().
unsubscribe(Context, <<"skel.", _Args/binary>> = _Binding, _JObj) ->
    {'ok', Context};
unsubscribe(_Context, _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>}.
