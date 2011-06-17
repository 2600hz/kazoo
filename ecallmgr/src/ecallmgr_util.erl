%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Various utilities specific to ecallmgr. More general utilities go
%%% in whistle_util.erl
%%% @end
%%% Created : 15 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(ecallmgr_util).

-export([get_sip_to/1, get_sip_from/1, get_sip_request/1, get_orig_ip/1, custom_channel_vars/1]).
-export([eventstr_to_proplist/1]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("ecallmgr.hrl").

%% retrieves the sip address for the 'to' field
-spec(get_sip_to/1 :: (Prop :: proplist()) -> binary()).
get_sip_to(Prop) ->
    list_to_binary([get_value(<<"sip_to_user">>, Prop, get_value(<<"variable_sip_to_user">>, Prop, "nouser"))
		    , "@"
		    , get_value(<<"sip_to_host">>, Prop, get_value(<<"variable_sip_to_host">>, Prop, "nodomain"))
		   ]).

%% retrieves the sip address for the 'from' field
-spec(get_sip_from/1 :: (Prop :: proplist()) -> binary()).
get_sip_from(Prop) ->
    list_to_binary([
		    get_value(<<"sip_from_user">>, Prop, get_value(<<"variable_sip_from_user">>, Prop, "nouser"))
		    ,"@"
		    , get_value(<<"sip_from_host">>, Prop, get_value(<<"variable_sip_from_host">>, Prop, "nodomain"))
		   ]).

%% retrieves the sip address for the 'request' field
-spec(get_sip_request/1 :: (Prop :: proplist()) -> binary()).
get_sip_request(Prop) ->
    list_to_binary([
		    get_value(<<"Caller-Destination-Number">>, Prop, get_value(<<"variable_sip_req_user">>, Prop, "nouser"))
		    ,"@"
                    ,get_value(<<"variable_sip_req_host">>, Prop
                               ,get_value( list_to_binary(["variable_", ?CHANNEL_VAR_PREFIX, "Realm"]), Prop, "nodomain"))
		   ]).

-spec(get_orig_ip/1 :: (Prop :: proplist()) -> binary()).
get_orig_ip(Prop) ->
    get_value(<<"ip">>, Prop).

%% Extract custom channel variables to include in the event
-spec(custom_channel_vars/1 :: (Prop :: proplist()) -> proplist()).
custom_channel_vars(Prop) ->
    lists:foldl(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
		   ({<<?CHANNEL_VAR_PREFIX, Key/binary>>, V}, Acc) -> [{Key, V} | Acc];
		   (_, Acc) -> Acc
		end, [], Prop).

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec(eventstr_to_proplist/1 :: (EvtStr :: string()) -> proplist()).
eventstr_to_proplist(EvtStr) ->
    [begin
	 [K, V] = string:tokens(X, ": "),
	 [{V1,[]}] = mochiweb_util:parse_qs(V),
	 {whistle_util:to_binary(K), whistle_util:to_binary(V1)}
     end || X <- string:tokens(whistle_util:to_list(EvtStr), "\n")].
