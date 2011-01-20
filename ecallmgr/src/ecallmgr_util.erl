%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Various utilities specific to ecallmgr. More general utilities go
%%% in whistle_util.erl
%%% @end
%%% Created : 15 Nov 2010 by James Aimonetti <james@2600hz.org>

-module(ecallmgr_util).

-export([get_sip_to/1, get_sip_from/1, get_orig_ip/1, custom_channel_vars/1]).
-export([to_hex/1, route_to_dialstring/3, to_list/1, to_binary/1]).
-export([eventstr_to_proplist/1]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("../include/amqp_client/include/amqp_client.hrl").
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

-spec(get_orig_ip/1 :: (Prop :: proplist()) -> binary()).
get_orig_ip(Prop) ->
    get_value(<<"ip">>, Prop).

%% Extract custom channel variables to include in the event
-spec(custom_channel_vars/1 :: (Prop :: proplist()) -> proplist()).
custom_channel_vars(Prop) ->
    Custom = lists:filter(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, _Key/binary>>, _V}) -> true;
			     (_) -> false
			  end, Prop),
    lists:map(fun({<<"variable_", ?CHANNEL_VAR_PREFIX, Key/binary>>, V}) -> {Key, V} end, Custom).

-spec(to_hex/1 :: (S :: term()) -> string()).
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- whistle_util:to_list(S)])).

-spec(route_to_dialstring/3 :: (AmqpHost :: binary(), Route :: binary() | list(binary()), Domain :: binary() | string()) -> binary()).
route_to_dialstring(_AmqpHost, <<"sip:", _Rest/binary>>=DS, _D) -> DS;
route_to_dialstring(AmqpHost, [<<"user:", User/binary>>, DID], Domain) ->
    route_to_dialstring(AmqpHost, Domain, User, DID);
route_to_dialstring(AmqpHost, [Domain, <<"user:", User/binary>>, DID], _D) ->
    route_to_dialstring(AmqpHost, Domain, User, DID);
route_to_dialstring(_AmqpHost, DS, _D) -> DS.

%% how to know whether to replace user with DID or leave user in place?
route_to_dialstring(AmqpHost, Domain, User, DID) ->
    Self = self(),
    spawn(fun() ->
		  Q = amqp_util:new_targeted_queue(AmqpHost, <<>>),
		  amqp_util:bind_q_to_targeted(AmqpHost, Q),
		  amqp_util:basic_consume(AmqpHost, Q),
		  ReqProp = [{<<"User">>, User}, {<<"Host">>, Domain}, {<<"Fields">>, [<<"Contact">>, <<"Realm">>]}
			     | whistle_api:default_headers(Q, <<"directory">>, <<"reg_query">>, <<"ecallmgr">>, <<>>) ],
		  {ok, JSON} = whistle_api:reg_query(ReqProp),
		  amqp_util:broadcast_publish(AmqpHost, JSON, <<"application/json">>),
		  C = receive_reg_query_resp(User),
		  Self ! {contact, C}
	  end),
    receive
	{contact, C} -> C
    after 1500 -> User
    end.

receive_reg_query_resp(User) ->
    receive
	#'basic.consume_ok'{} ->
	    receive_reg_query_resp(User);
	{_, #amqp_msg{payload = Payload}} ->
	    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
	    format_log(info, "ECALL_UTIL: RegQResp:~n~p~n", [Prop]),
	    true = whistle_api:reg_query_resp_v(Prop),

	    {struct, [{<<"Contact">>, Contact}, {<<"Realm">>, Realm}]} = props:get_value(<<"Fields">>, Prop),
	    
	    binary:replace(re:replace(Contact, "^[^\@]+", User, [{return, binary}]), <<">">>, <<";fs_path=sip:", Realm/binary>>)
    after 1000 ->
	    format_log(error, "ECALL_UTIL: Timed out waiting for Contact for ~p~n", [User]),
	    User
    end.

-spec(to_list/1 :: (X :: atom() | list() | binary() | integer() | float()) -> list()).
to_list(X) when is_float(X) ->
    mochinum:digits(X);
to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_binary(X) ->
    binary_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) when is_list(X) ->
    X.

-spec(to_binary/1 :: (X :: atom() | list() | binary() | integer() | float()) -> binary()).
to_binary(X) when is_float(X) ->
    to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) ->
    to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_binary(X) ->
    X.

%% convert a raw FS string of headers to a proplist
%% "Event-Name: NAME\nEvent-Timestamp: 1234\n" -> [{<<"Event-Name">>, <<"NAME">>}, {<<"Event-Timestamp">>, <<"1234">>}]
-spec(eventstr_to_proplist/1 :: (EvtStr :: string()) -> proplist()).
eventstr_to_proplist(EvtStr) when is_list(EvtStr) ->
    lists:map(fun(X) ->
		      [K, V] = string:tokens(X, ": "),
		      [{V1,[]}] = mochiweb_util:parse_qs(V),
		      {whistle_util:to_binary(K), whistle_util:to_binary(V1)}
	      end, string:tokens(whistle_util:to_list(EvtStr), "\n")).
