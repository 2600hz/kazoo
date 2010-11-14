-module(ecallmgr_util).

-export([get_sip_to/1, get_sip_from/1, get_orig_ip/1, custom_channel_vars/1]).
-export([route_to_dialstring/2, route_to_dialstring/3]).

-import(props, [get_value/2, get_value/3]).

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

route_to_dialstring(<<"sip:", _Rest/binary>>=DS, _D) -> %% assumes _Rest is properly formatted (may want to force to E.164?)
    DS;
route_to_dialstring([<<"user:", User/binary>>, DID], Domain) ->
    list_to_binary([DID, "${regex(${sofia_contact(sipinterface_1/",User, "@", Domain, ")}|^[^\@]+(.*)|%1)}"]);
route_to_dialstring(DS, _D) -> DS.

%% for some commands, like originate, macros are not run; need to progressively build the dialstring
route_to_dialstring([<<"user:", User/binary>>, DID], Domain, FSNode) ->
    {ok, SC} = freeswitch:api(FSNode, sofia_contact, binary_to_list(list_to_binary(["sipinterface_1/",User, "@", Domain]))),
    Regex = list_to_binary([SC, "|^[^\@]+(.*)|", DID, "%1"]),
    io:format("Regex: ~p~n", [Regex]),
    {ok, DS} = freeswitch:api(FSNode, regex, binary_to_list(Regex)),
    DS;
route_to_dialstring(X, _, _) -> X.

