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

-export([handle_event/2
        ,subscribe/2
        , unsubscribe/2
        ]).

-include("blackhole.hrl").

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    blackhole_util:handle_event(Context, EventJObj, event_name(EventJObj)).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

%% Binding must match module name
-spec subscribe(bh_context:context(), ne_binary()) -> bh_subscribe_result().
subscribe(Context, <<"skel.", _Args/binary>> = _Binding) ->
    {'ok', Context};
subscribe(_Context, _Binding) ->
    {'error', <<"Unmatched binding">>}.

-spec unsubscribe(bh_context:context(), ne_binary()) -> bh_subscribe_result().
unsubscribe(Context, <<"skel.", _Args/binary>> = _Binding) ->
    {'ok', Context};
unsubscribe(_Context, _Binding) ->
    {'error', <<"Unmatched binding">>}.
