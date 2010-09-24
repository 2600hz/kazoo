-module(ecallmgr_util).

-export([get_sip_to/1, get_sip_from/1, get_orig_ip/1, custom_channel_vars/1]).

-import(proplists, [get_value/2, get_value/3]).

-include("whistle_api.hrl").

%% retrieves the sip address for the 'to' field
-spec(get_sip_to/1 :: (Prop :: proplist()) -> binary()).
get_sip_to(Prop) ->
    list_to_binary([get_value(<<"sip_to_user">>, Prop, get_value(<<"variable_sip_to_user">>, Prop, ""))
		    , "@"
		    , get_value(<<"sip_to_host">>, Prop, get_value(<<"variable_sip_to_host">>, Prop, ""))
		   ]).

%% retrieves the sip address for the 'from' field
-spec(get_sip_from/1 :: (Prop :: proplist()) -> binary()).
get_sip_from(Prop) ->
    list_to_binary([
		    get_value(<<"sip_from_user">>, Prop, get_value(<<"variable_sip_from_user">>, Prop, ""))
		    ,"@"
		    , get_value(<<"sip_from_host">>, Prop, get_value(<<"variable_sip_from_host">>, Prop, ""))
		   ]).

get_orig_ip(Prop) ->
    get_value(<<"ip">>, Prop).

%% Extract custom channel variables to include in the event
-spec(custom_channel_vars/1 :: (Prop :: proplist()) -> proplist()).
custom_channel_vars(Prop) ->
    Custom = lists:filter(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, _Key/binary>>, _V}) -> true;
			     (_) -> false
			  end, Prop),
    lists:map(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}) -> {Key, V} end, Custom).
