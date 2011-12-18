%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(stepswitch_util).

-export([lookup_number/1, build_loopback_request/4, evaluate_number/2
	 ,evaluate_flags/2, build_bridge_request/3, respond_erroneously/2
	 ,respond_bridged_to_resource/2, respond_resource_failed/3
	 ,lookup_account_by_number/1, get_dialstring/2, wait_for_bridge/1
	]).

-include("stepswitch.hrl").

-spec lookup_number/1 :: (ne_binary()) -> {'ok', ne_binary(), boolean()} | {'error', term()}.
lookup_number(Number) ->
    Num = wh_util:to_e164(wh_util:to_binary(Number)),
    case lookup_account_by_number(Num) of
	{ok, AccountId, _}=Ok ->
	    ?LOG("found number is associated to account ~s", [AccountId]),
	    Ok;
	{error, Reason}=E ->
	    ?LOG("number is not associated to any account, ~w", [Reason]),
	    E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% lookup the account ID by number
%% @end
%%--------------------------------------------------------------------
-spec lookup_account_by_number/1 :: (ne_binary()) -> {'ok', ne_binary(), boolean()} |
						     {'error', atom()}.
-spec lookup_account_by_number/2 :: (ne_binary(), pid()) -> {'ok', ne_binary(), boolean()} |
							    {'error', atom()}.
lookup_account_by_number(Number) ->
    ?LOG("lookup account for ~s", [Number]),
    {ok, Cache} = stepswitch_sup:cache_proc(),
    lookup_account_by_number(Number, Cache).
lookup_account_by_number(Number, Cache) ->
    case wh_cache:fetch_local(Cache, {stepswitch_number, Number}) of
	{ok, {AccountId, ForceOut}} ->
            {ok, AccountId, ForceOut};
	{error, not_found} ->
            Options = [{<<"key">>, Number}],
	    case couch_mgr:get_results(?ROUTES_DB, ?LIST_ROUTES_BY_NUMBER, Options) of
		{error, _}=E ->
		    E;
		{ok, []} ->
		    {error, not_found};
		{ok, [JObj]} ->
                    AccountId = wh_json:get_value(<<"id">>, JObj),
                    ForceOut = wh_util:is_true(wh_json:get_value([<<"value">>, <<"force_outbound">>], JObj, false)),
                    wh_cache:store_local(Cache, {stepswitch_number, Number}, {AccountId, ForceOut}),
		    {ok, AccountId, ForceOut};
		{ok, [JObj | _Rest]} ->
		    whapps_util:alert(<<"alert">>, ["Source: ~s(~p)~n"
						    ,"Alert: Number ~s found more than once in the ~s DB~n"
						    ,"Fault: Number should be listed, at most, once~n"
						    ,"Call-ID: ~s~n"
						   ]
				      ,[?MODULE, ?LINE, Number, ?ROUTES_DB, get(callid)]),

		    ?LOG("number lookup resulted in more than one result, using the first"),
                    AccountId = wh_json:get_value(<<"id">>, JObj),
                    ForceOut = wh_util:is_true(wh_json:get_value([<<"value">>, <<"force_outbound">>], JObj, false)),
                    wh_cache:store(Cache, {stepswitch_number, Number}, {AccountId, ForceOut}),
		    {ok, AccountId, ForceOut}
	    end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% creates an offnet resource response and sends it to the tageted
%% queue of the requestor
%% @end
%%--------------------------------------------------------------------
-spec respond_bridged_to_resource/2 :: (json_object(), json_object()) -> 'ok'.
respond_bridged_to_resource(BridgeResp, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, BridgeResp),
    Resp = [{<<"Call-ID">>, wh_json:get_value(<<"Other-Leg-Unique-ID">>, BridgeResp)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
            ,{<<"Control-Queue">>, wh_json:get_value(<<"Control-Queue">>, JObj)}
            ,{<<"To">>, wh_json:get_value(<<"Other-Leg-Destination-Number">>, BridgeResp)}
            ,{<<"Caller-ID-Name">>, wh_json:get_value(<<"Other-Leg-Caller-ID-Name">>, BridgeResp)}
            ,{<<"Caller-ID-Number">>, wh_json:get_value(<<"Other-Leg-Caller-ID-Number">>, BridgeResp)}
            ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, BridgeResp)}
            ,{<<"Timestamp">>, wh_json:get_value(<<"Timestamp">>, BridgeResp)}
            ,{<<"Channel-Call-State">>, wh_json:get_value(<<"Channel-Call-State">>, BridgeResp)}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)],
    wapi_resource:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% creates an offnet resource failure response and sends it back to
%% the targeted queue of the requestor
%% @end
%%--------------------------------------------------------------------
-spec respond_resource_failed/3 :: (json_object(), non_neg_integer(), json_object()) -> 'ok'.
respond_resource_failed(BridgeResp, Attempts, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, BridgeResp, <<>>),
    Error = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
             ,{<<"Failed-Attempts">>, wh_util:to_binary(Attempts)}
             ,{<<"Failure-Message">>,
               wh_json:get_value(<<"Application-Response">>, BridgeResp, wh_json:get_value(<<"Hangup-Cause">>, BridgeResp))}
             ,{<<"Failure-Code">>, wh_json:get_value(<<"Hangup-Code">>, BridgeResp)}
             ,{<<"Hangup-Cause">>, wh_json:get_value(<<"Hangup-Cause">>, BridgeResp)}
             ,{<<"Hangup-Code">>, wh_json:get_value(<<"Hangup-Code">>, BridgeResp)}
             | wh_api:default_headers(Q, <<"error">>, <<"originate_error">>, ?APP_NAME, ?APP_VERSION)],
    wapi_resource:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Error).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% creates a generic error response and sends it back to
%% the targeted queue of the requestor
%% @end
%%--------------------------------------------------------------------
-spec respond_erroneously/2 :: (json_object(), json_object()) -> 'ok'.
respond_erroneously(ErrorResp, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, ErrorResp, <<>>),
    Response = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, ErrorResp)}
               | wh_api:default_headers(Q, <<"error">>, <<"resource_error">>, ?APP_NAME, ?APP_VERSION)
            ],
    wapi_resource:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Response).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec build_loopback_request/4 :: (json_object(), ne_binary(), ne_binary(), ne_binary()) -> proplist().
build_loopback_request(JObj, Number, LoopAccount, Q) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Endpoints = [wh_json:from_list([{<<"Invite-Format">>, <<"route">>}
				    ,{<<"Route">>, list_to_binary(["loopback/", Number])}
				    ,{<<"Custom-Channel-Vars">>, wh_json:from_list([{<<"Offnet-Loopback-Number">>, Number}
										    ,{<<"Offnet-Loopback-Account-ID">>, AccountId}
										    ,{<<"Account-ID">>, LoopAccount}
										   ])}
				   ])],
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               %% TODO: Do not enable this feature until WHISTLE-408 is completed
               %% ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj)}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    [ KV || {_, V}=KV <- Command, V =/= 'undefined' ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter the list of resources returning only those with a rule that
%% matches the number.  The list is of tuples with three elements,
%% the weight, the captured component of the number, and the gateways.
%% @end
%%--------------------------------------------------------------------
-spec evaluate_number/2 :: (ne_binary(), [#resrc{}]) -> endpoints().
evaluate_number(Number, Resrcs) ->
    sort_endpoints(get_endpoints(Number, Resrcs)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Filter the list of resources returning only those that have every
%% flag provided
%% @end
%%--------------------------------------------------------------------
-spec evaluate_flags/2 :: (list(), [#resrc{}]) -> [#resrc{}].
evaluate_flags(F1, Resrcs) ->
    [Resrc
     || #resrc{flags=F2}=Resrc <- Resrcs,
	lists:all(fun(Flag) -> lists:member(Flag, F2) end, F1)
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec build_bridge_request/3 :: (json_object(), endpoints(), ne_binary()) -> proplist().
build_bridge_request(JObj, Endpoints, Q) ->
    CCVs = wh_json:set_value(<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj, <<>>)
                             ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT)),
    {CIDNum, CIDName} = case contains_emergency_endpoint(Endpoints) of
                            'true' ->
                                ?LOG("outbound call is using an emergency route, attempting to set CID accordingly"),
                                {wh_json:get_value(<<"Emergency-Caller-ID-Number">>, JObj,
                                                   wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)),
                                 wh_json:get_value(<<"Emergency-Caller-ID-Name">>, JObj,
                                                   wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj))};
                            false  ->
                                {wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj),
                                 wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
                        end,
    ?LOG("set outbound caller id to ~s '~s'", [CIDNum, CIDName]),
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, build_endpoints(Endpoints, 0, [])}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
               ,{<<"Hold-Media">>, wh_json:get_value(<<"Hold-Media">>, JObj)}
               ,{<<"Presence-ID">>, wh_json:get_value(<<"Presence-ID">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
               ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               %% TODO: Do not enable this feature until WHISTLE-408 is completed
               %% ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    [ KV || {_, V}=KV <- Command, V =/= 'undefined' ].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% waits for the return from the bridge request
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge/1 :: (non_neg_integer()) -> {'ok' | 'fail' | 'hungup' | 'error' | 'rate_resp', json_object()} |
						  {'error', 'timeout'}.
wait_for_bridge(Timeout) ->
    Start = erlang:now(),
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
                    {ok, JObj};
                { <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
		    case wh_json:get_value(<<"Application-Response">>, JObj) of
			<<"SUCCESS">> -> {ok, JObj};
			_ -> {fail, JObj}
		    end;
                { _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {hungup, JObj};
                { _, _, <<"error">> } ->
                    {error, JObj};
		{_, <<"rating_resp">>, <<"call_mgmt">>} ->
		    {rate_resp, JObj};
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_bridge(Timeout - (DiffMicro div 1000))
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_bridge(Timeout - (DiffMicro div 1000))
    after
        Timeout ->
            {error, timeout}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sort the gateway tuples returned by evalutate_resrcs according to
%% weight.
%% @end
%%--------------------------------------------------------------------
-spec sort_endpoints/1 :: (endpoints()) -> endpoints().
sort_endpoints(Endpoints) ->
    lists:sort(fun({W1, _, _, _, _}, {W2, _, _, _, _}) ->
                       W1 =< W2
               end, Endpoints).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints/2 :: (ne_binary(), [#resrc{}]) -> endpoints().
get_endpoints(Number, Resrcs) ->
    EPs = [get_endpoint(Number, R) || R <- Resrcs],
    [Endpoint || Endpoint <- EPs, Endpoint =/= no_match].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint/2 :: (ne_binary(), #resrc{}) -> endpoint() | 'no_match'.
get_endpoint(Number, #resrc{weight_cost=WC, gateways=Gtws, rules=Rules, grace_period=GP, is_emergency=IsEmergency}) ->
    case evaluate_rules(Rules, Number) of
        {ok, DestNum} ->
            {WC, GP, DestNum, Gtws, IsEmergency};
        {error, no_match} ->
            no_match
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function loops over rules (regex) and until one matches
%% the destination number.  If the matching rule has a
%% capture group return the largest group, otherwise return the whole
%% number.  In the event that no rules match then return an error.
%% @end
%%--------------------------------------------------------------------
-spec evaluate_rules/2 :: (re:mp(), ne_binary()) -> {'ok', ne_binary()} | {'error', 'no_match'}.
evaluate_rules([], _) ->
    {error, no_match};
evaluate_rules([Regex|T], Number) ->
    case re:run(Number, Regex) of
        {match, [{Start,End}]} ->
            {ok, binary:part(Number, Start, End)};
        {match, CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {ok, binary:part(Number, Start, End)};
        _ ->
            evaluate_rules(T, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Digs through all the given resources and determines if any of them
%% are for emergency services
%% @end
%%--------------------------------------------------------------------
-spec contains_emergency_endpoint/1 :: (Endpoints) -> boolean() when
      Endpoints :: endpoints().
-spec contains_emergency_endpoint/2 :: (Endpoints, UseEmergency) -> boolean() when
      Endpoints :: endpoints(),
      UseEmergency :: boolean().

contains_emergency_endpoint(Endpoints) ->
    contains_emergency_endpoint(Endpoints, false).

contains_emergency_endpoint([], UseEmergency) ->
    UseEmergency;
contains_emergency_endpoint([{_, _, _, _, IsEmergency}|T], UseEmergency) ->
    contains_emergency_endpoint(T, IsEmergency or UseEmergency).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec build_endpoints/3 :: (Endpoints, Delay, Acc) -> proplist() when
      Endpoints :: endpoints(),
      Delay :: non_neg_integer(),
      Acc :: proplist().
build_endpoints([], _, Acc) ->
    lists:reverse(Acc);
build_endpoints([{_, GracePeriod, Number, [Gateway], _}|T], Delay, Acc0) ->
    build_endpoints(T, Delay + GracePeriod, [build_endpoint(Number, Gateway, Delay)|Acc0]);
build_endpoints([{_, GracePeriod, Number, Gateways, _}|T], Delay, Acc0) ->
    {D2, Acc1} = lists:foldl(fun(Gateway, {0, AccIn}) ->
                                     {2, [build_endpoint(Number, Gateway, 0)|AccIn]};
                                 (Gateway, {D0, AccIn}) ->
                                     {D0 + 2, [build_endpoint(Number, Gateway, D0)|AccIn]}
                            end, {Delay, Acc0}, Gateways),
    build_endpoints(T, D2 - 2 + GracePeriod, Acc1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the endpoint for use in the route request
%% @end
%%--------------------------------------------------------------------
-spec build_endpoint/3 :: (Number, Gateway, Delay) -> json_object() when
      Number :: binary(),
      Gateway :: #gateway{},
      Delay :: non_neg_integer().
build_endpoint(Number, Gateway, Delay) ->
    Route = get_dialstring(Gateway, Number),
    ?LOG("using ~s on ~s delayed by ~b sec", [Route, Gateway#gateway.resource_id, Delay]),
    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"Route">>, get_dialstring(Gateway, Number)}
            ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
            ,{<<"Caller-ID-Type">>, Gateway#gateway.caller_id_type}
            %% TODO: Do not enable this feature until WHISTLE-408 is completed
            %%,{<<"Endpoint-Delay">>, wh_util:to_binary(Delay)}
            ,{<<"Bypass-Media">>, Gateway#gateway.bypass_media}
            ,{<<"Endpoint-Progress-Timeout">>, wh_util:to_binary(Gateway#gateway.progress_timeout)}
            ,{<<"Codecs">>, Gateway#gateway.codecs}
            ,{<<"Auth-User">>, Gateway#gateway.username}
            ,{<<"Auth-Password">>, Gateway#gateway.password}
            ,{<<"SIP-Headers">>, Gateway#gateway.sip_headers}
            ,{<<"Custom-Channel-Vars">>, wh_json:set_value(<<"Resource-ID">>, Gateway#gateway.resource_id, wh_json:new())}
           ],
    wh_json:from_list([ KV || {_, V}=KV <- Prop, V =/= 'undefined' andalso V =/= <<"0">>]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the route dialstring
%% @end
%%--------------------------------------------------------------------
-spec get_dialstring/2 :: (#gateway{}, ne_binary()) -> ne_binary().
get_dialstring(#gateway{route = undefined, prefix=Prefix, suffix=Suffix, server=Server}, Number) ->
    list_to_binary(["sip:"
		    ,wh_util:to_binary(Prefix)
		    ,Number
		    ,wh_util:to_binary(Suffix)
		    ,"@"
		    ,wh_util:to_binary(Server)
		   ]);
get_dialstring(#gateway{route=Route}, _) ->
    Route.
