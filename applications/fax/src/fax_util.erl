%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_util).

-export([fax_properties/1]).
-export([collect_channel_props/1]).

-include("fax.hrl").

-spec fax_properties(wh_json:object()) -> wh_proplist().
fax_properties(JObj) ->
    [{wh_json:normalize_key(K), V} || {<<"Fax-", K/binary>>, V} <- wh_json:to_proplist(JObj)].


collect_channel_props(JObj) ->
    collect_channel_props(JObj, ?FAX_CHANNEL_DESTROY_PROPS).

collect_channel_props(JObj, List) ->
    lists:foldl(fun({Key, Keys}, Acc0) ->
                        Acc = collect_channel_props(wh_json:get_value(Key, JObj), Keys),
                        Acc ++ Acc0;
                   (Key, Acc) ->
                        [collect_channel_prop(Key, JObj)  | Acc]
                end, [], List).
                        
collect_channel_prop(<<"Hangup-Code">> = Key, JObj) ->
    <<"sip:", Code/binary>> = wh_json:get_value(Key, JObj),
    {Key, Code};
collect_channel_prop(Key, JObj) ->
    {Key, wh_json:get_value(Key, JObj)}.
                        
