%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Functions shared between crossbar modules
%%% @end
%%% Created : 17 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_modules_util).

-export([lookup_regs/1, pass_hashes/2, get_devices_owned_by/2]).

-include("../../include/crossbar.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For a given [{Realm, Username}|...], returns  [{{Realm, Username}, RegistrationJObj} | ...]
%% whose device is registered, ready to be used in a view filter
%% @end
%%--------------------------------------------------------------------
-spec lookup_regs/1 :: ([{ne_binary(), ne_binary()},...]) -> [{{ne_binary(), ne_binary()}, wh_json:json_object()},...] | [].
lookup_regs(RealmUserList) ->
    Q = amqp_util:new_queue(),
    ok = amqp_util:bind_q_to_targeted(Q),
    ok = amqp_util:basic_consume(Q),
    Pids = [spawn(fun() -> lookup_registration({Realm, User}, Q) end) || {Realm, User} <- RealmUserList],
    wait_for_reg_resp(Pids, []). %% number of devices we're supposed to get an answer from

lookup_registration({Realm, User}, Q) ->
    ?LOG_SYS("Looking up registration information for ~s@~s", [User, Realm]),
    RegProp = [{<<"Username">>, User}
	       ,{<<"Realm">>, Realm}
	       ,{<<"Fields">>, []}
	       ,{<<"Server-ID">>, Q}
	       ,{<<"App-Name">>, ?MODULE}
	       ,{<<"App-Version">>, ?APP_VERSION}
	      ],
    wapi_registration:publish_query_req(RegProp).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Collect Len number of registrations in Acc unless the timeout
%% occurs
%% @end
%%--------------------------------------------------------------------
-spec wait_for_reg_resp/2 :: ([pid(),...] | [], [{{ne_binary(), ne_binary()}, wh_json:json_object()},...] | []) -> [{{ne_binary(), ne_binary()}, wh_json:json_object()},...] | [].
wait_for_reg_resp([], Acc) -> Acc;
wait_for_reg_resp([_|Ps]=Pids, Acc) ->
    receive
	{amqp_host_down, _} ->
	    ?LOG("lost AMQP connection"),
	    Acc;
	{amqp_lost_channel,no_connection} ->
	    ?LOG("lost AMQP connection"),
	    Acc;
	{_, #amqp_msg{payload = Payload}} ->
	    JRegResp = mochijson2:decode(Payload),
	    true = wapi_registration:query_resp_v(JRegResp),

	    Fields = wh_json:get_value([<<"Fields">>], JRegResp),
	    Realm = wh_json:get_value([<<"Realm">>], Fields),
	    User = wh_json:get_value([<<"Username">>], Fields),

	    ?LOG("Received reg for ~s @ ~s", [User, Realm]),

	    case lists:keyfind(RU={Realm, User}, 1, Acc) of
		false -> wait_for_reg_resp(Ps, [{RU, Fields} | Acc]);
		_ -> wait_for_reg_resp(Pids, Acc)
	    end;
	#'basic.consume_ok'{} ->
	    wait_for_reg_resp(Pids, Acc)
    after
	1000 ->
	    ?LOG("timeout for registration query"),
	    Acc
    end.

-spec pass_hashes/2 :: (ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = wh_util:to_binary(wh_util:to_hex(crypto:sha(Creds))),
    MD5 = wh_util:to_binary(wh_util:to_hex(erlang:md5(Creds))),
    {MD5, SHA1}.

-spec get_devices_owned_by/2 :: (ne_binary(), ne_binary()) -> wh_json:json_objects().
get_devices_owned_by(OwnerID, DB) ->
    case couch_mgr:get_results(DB, <<"cf_attributes/owned">>, [{<<"key">>, [OwnerID, <<"device">>]}, {<<"include_docs">>, true}]) of
	{ok, JObjs} ->
	    ?LOG("Found ~b devices owned by ~s", [length(JObjs), OwnerID]),
	    [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
	{error, _R} ->
	    ?LOG("unable to fetch devices: ~p", [_R]),
	    []
    end.
