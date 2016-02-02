%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_number_properties).

-export([get/2, get/3
         ,should_force_outbound/1
         ,has_pending_port/1
         ,is_local_number/1
         ,inbound_cnam_enabled/1
         ,transfer_media_id/1
         ,ringback_media_id/1
         ,prepend/1
         ,account_id/1, set_account_id/2
         ,number/1
        ]).

-include("wnm.hrl").

-spec get(atom(), number_properties()) -> any().
-spec get(atom(), number_properties(), any()) -> any().
get(Key, Props) ->
    get(Key, Props, 'undefined').
get(Key, Props, Default) ->
    props:get_value(Key, Props, Default).

-spec should_force_outbound(number_properties()) -> boolean().
should_force_outbound(Props) ->
    props:get_is_true('force_outbound', Props).

-spec has_pending_port(number_properties()) -> boolean().
has_pending_port(Props) ->
    props:get_is_true('pending_port', Props).

-spec is_local_number(number_properties()) -> boolean().
is_local_number(Props) ->
    props:get_is_true('local', Props).

-spec inbound_cnam_enabled(number_properties()) -> boolean().
inbound_cnam_enabled(Props) ->
    props:get_is_true('inbound_cnam', Props).

-spec transfer_media_id(number_properties()) -> api_binary().
transfer_media_id(Props) ->
    props:get_value('transfer_media', Props).

-spec ringback_media_id(number_properties()) -> api_binary().
ringback_media_id(Props) ->
    props:get_value('ringback_media', Props).

-spec prepend(number_properties()) -> api_binary() | 'false'.
prepend(Props) ->
    props:get_value('prepend', Props).

-spec account_id(number_properties()) -> api_binary().
account_id(Props) ->
    props:get_value('account_id', Props).

-spec set_account_id(number_properties(), ne_binary()) -> number_properties().
set_account_id(Props, AccountId) ->
    props:set_value('account_id', AccountId, Props).

-spec number(number_properties()) -> api_binary().
number(Props) ->
    props:get_value('number', Props).
