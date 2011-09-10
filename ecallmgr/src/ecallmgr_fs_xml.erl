%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Generate the XML for various FS responses
%%% @end
%%% Created : 25 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_xml).

-export([route_resp_xml/1, build_route/2, get_leg_vars/1, get_channel_vars/1, authn_resp_xml/1]).

-include("ecallmgr.hrl").

authn_resp_xml({struct, RespProp}) ->
    authn_resp_xml(RespProp);
authn_resp_xml(RespProp) ->
    authn_resp_xml(props:get_value(<<"Auth-Method">>, RespProp), RespProp).

authn_resp_xml(<<"password">>, Prop) ->
    User = props:get_value(<<"Auth-User">>, Prop),
    Realm = props:get_value(<<"Auth-Realm">>, Prop),
    Pass = props:get_value(<<"Auth-Password">>, Prop),
    ChannelParams = get_channel_params(Prop),
    {ok, lists:flatten(io_lib:format(?REGISTER_PASS_RESPONSE, [Realm, User, Pass, ChannelParams]))};
authn_resp_xml(<<"a1-hash">>, Prop) ->
    User = props:get_value(<<"Auth-User">>, Prop),
    Realm = props:get_value(<<"Auth-Realm">>, Prop),
    Hash = props:get_value(<<"Auth-Password">>, Prop),
    ChannelParams = get_channel_params(Prop),
    {ok, lists:flatten(io_lib:format(?REGISTER_HASH_RESPONSE, [Realm, User, Hash, ChannelParams]))};
authn_resp_xml(<<"ip">>, _Prop) ->
    {ok, ?EMPTYRESPONSE};
authn_resp_xml(_, _) ->
    {ok, ?EMPTYRESPONSE}.

route_resp_xml({struct, RespProp}=RespJObj) ->
    route_resp_xml(wh_json:get_value(<<"Method">>, RespJObj), wh_json:get_value(<<"Routes">>, RespJObj), RespProp);
route_resp_xml(RespProp) ->
    route_resp_xml(props:get_value(<<"Method">>, RespProp), props:get_value(<<"Routes">>, RespProp), RespProp).

%% Prop = Route Response
route_resp_xml(<<"bridge">>, Routes, _Prop) ->
    ?LOG("Creating a bridge XML response"),
    %% format the Route based on protocol
    {_Idx, Extensions, Errors} = lists:foldr(fun({struct, RouteProp}, {Idx, Acc, ErrAcc}) ->
						     case build_route(RouteProp, props:get_value(<<"Invite-Format">>, RouteProp)) of
							 {error, timeout} ->
							     {Idx+1, Acc, ErrAcc};
							 Route ->
							     BypassMedia = case props:get_value(<<"Media">>, RouteProp) of
									       <<"bypass">> -> "true";
									       %% <<"process">> -> "false";
									       _ -> "false" %% default to not bypassing media
									   end,

							     RP1 = case props:get_value(<<"Progress-Timeout">>, RouteProp) of
								       undefined -> [ {<<"Progress-Timeout">>, <<"6">>} | RouteProp];
								       I when is_integer(I) -> [ {<<"Progress-Timeout">>, integer_to_list(I)}
												 | lists:keydelete(<<"Progress-Timeout">>, 1, RouteProp) ];
								       _ -> RouteProp
								   end,

							     ChannelVars = get_channel_vars(RP1),
							     {Idx+1, [io_lib:format(?ROUTE_BRIDGE_EXT, [Idx, BypassMedia, ChannelVars, Route]) | Acc], ErrAcc}
						     end
					     end, {1, "", ""}, Routes),

    case Extensions of
	[] ->
	    ?LOG("No endpoints to route to"),
	    {ok, lists:flatten(io_lib:format(?ROUTE_BRIDGE_RESPONSE, [?WHISTLE_CONTEXT, Errors]))};
	_ ->
	    Xml = io_lib:format(?ROUTE_BRIDGE_RESPONSE, [?WHISTLE_CONTEXT, Extensions]),
	    ?LOG("Bridge XML generated: ~s", [Xml]),
	    {ok, lists:flatten(Xml)}
    end;
route_resp_xml(<<"park">>, _Routes, _Prop) ->
    Park = lists:flatten(io_lib:format(?ROUTE_PARK_RESPONSE, [?WHISTLE_CONTEXT])),
    ?LOG("Creating park XML: ~s", [Park]),
    {ok, Park};
route_resp_xml(<<"error">>, _Routes, Prop) ->
    ErrCode = props:get_value(<<"Route-Error-Code">>, Prop),
    ErrMsg = list_to_binary([" ", props:get_value(<<"Route-Error-Message">>, Prop, <<"">>)]),
    Xml = io_lib:format(?ROUTE_ERROR_RESPONSE, [?WHISTLE_CONTEXT, ErrCode, ErrMsg]),
    ?LOG("Creating error XML: ~s", [Xml]),
    {ok, lists:flatten(Xml)}.

-spec(build_route/2 :: (RouteProp :: proplist() | json_object(), DIDFormat :: binary()) -> binary() | tuple(error, timeout)).
build_route(Route, undefined) ->
    build_route(Route, <<"username">>);
build_route({struct, RouteProp}, DIDFormat) ->
    build_route(RouteProp, DIDFormat);
build_route(RouteProp, <<"route">>) ->
    case props:get_value(<<"Route">>, RouteProp) of
        <<"sip:", _/binary>> = R1 -> <<?SIP_INTERFACE, (R1)/binary>>;
        R2 -> R2
    end;
build_route(RouteProp, <<"username">>) ->
    User = props:get_value(<<"To-User">>, RouteProp),
    Realm = props:get_value(<<"To-Realm">>, RouteProp),
    case ecallmgr_registrar:lookup(Realm, User, [<<"Contact">>]) of
	[{<<"Contact">>, Contact}] ->
	    RURI = binary:replace(re:replace(Contact, "^[^\@]+", User, [{return, binary}]), <<">">>, <<"">>),
            <<?SIP_INTERFACE, (RURI)/binary>>;
	{error, timeout}=E ->
            ?LOG("failed to lookup user ~s@~s in the registrar", [User, Realm]),
	    E
    end;
build_route(RouteProp, DIDFormat) ->
    User = props:get_value(<<"To-User">>, RouteProp),
    Realm = props:get_value(<<"To-Realm">>, RouteProp),
    DID = format_did(props:get_value(<<"To-DID">>, RouteProp), DIDFormat),
    case ecallmgr_registrar:lookup(Realm, User, [<<"Contact">>]) of
	[{<<"Contact">>, Contact}] ->
	    RURI = binary:replace(re:replace(Contact, "^[^\@]+", DID, [{return, binary}]), <<">">>, <<"">>),
            <<?SIP_INTERFACE, (RURI)/binary>>;
	{error, timeout}=E ->
            ?LOG("failed to lookup user ~s@~s in the registrar", [User, Realm]),
	    E
    end.

-spec(format_did/2 :: (DID :: binary(), Format :: binary()) -> binary()).
format_did(DID, <<"e164">>) ->
    wh_util:to_e164(DID);
format_did(DID, <<"npan">>) ->
    wh_util:to_npan(DID);
format_did(DID, <<"1npan">>) ->
    wh_util:to_1npan(DID).

-spec(get_leg_vars/1 :: (JObj :: json_object() | proplist()) -> iolist()).
get_leg_vars({struct, Prop}) -> get_leg_vars(Prop);
get_leg_vars(Prop) ->
    ["[", string:join([binary_to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], Prop)], ","), "]"].

-spec(get_channel_vars/1 :: (JObj :: json_object() | proplist()) -> list(string())).
get_channel_vars({struct, Prop}) -> get_channel_vars(Prop);
get_channel_vars(Prop) ->
    P = Prop ++ [{<<"Overwrite-Channel-Vars">>, <<"true">>}],
    ["{", string:join([binary_to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], P)], ","), "}"].

-spec(get_channel_vars/2 :: (Pair :: tuple(binary(), term()), Vars :: list(binary())) -> list(binary())).
get_channel_vars({<<"Outgoing-Caller-ID-Name">>, V}, Vars) ->
    [ list_to_binary(["origination_caller_id_name='", V, "'"]) | Vars];
get_channel_vars({<<"Outgoing-Caller-ID-Number">>, V}, Vars) ->
    [ list_to_binary(["origination_caller_id_number='", V, "'"]) | Vars];
get_channel_vars({<<"Outgoing-Callee-ID-Name">>, V}, Vars) ->
    [ list_to_binary(["origination_callee_id_name='", V, "'"]) | Vars];
get_channel_vars({<<"Outgoing-Callee-ID-Number">>, V}, Vars) ->
    [ list_to_binary(["origination_callee_id_number='", V, "'"]) | Vars];
get_channel_vars({<<"Auth-User">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_username='", V, "'"]) | Vars];
get_channel_vars({<<"Auth-Password">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_password='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Name">>, V}, Vars) ->
    [ list_to_binary(["effective_caller_id_name='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Number">>, V}, Vars) ->
    [ list_to_binary(["effective_caller_id_number='", V, "'"]) | Vars];
get_channel_vars({<<"Callee-ID-Name">>, V}, Vars) ->
    [ list_to_binary(["effective_callee_id_name='", V, "'"]) | Vars];
get_channel_vars({<<"Callee-ID-Number">>, V}, Vars) ->
    [ list_to_binary(["effective_callee_id_number='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"from">>}, Vars) ->
    [ <<"sip_cid_type=none">> | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"rpid">>}, Vars) ->
    [ <<"sip_cid_type=rpid">> | Vars];
get_channel_vars({<<"Caller-ID-Type">>, <<"pid">>}, Vars) ->
    [ <<"sip_cid_type=pid">> | Vars];
get_channel_vars({<<"Codecs">>, []}, Vars) ->
    Vars;
get_channel_vars({<<"Codecs">>, Cs}, Vars) ->
    Codecs = [ binary_to_list(C) || C <- Cs ],
    CodecStr = string:join(Codecs, ","),
    [ list_to_binary(["codec_string='", CodecStr, "'"]) | Vars];
get_channel_vars({<<"Progress-Timeout">>, V}, Vars) ->
    [ list_to_binary([<<"progress_timeout=">>, V]) | Vars];
get_channel_vars({<<"Rate">>, V}, Vars) ->
    [ list_to_binary([<<"rate=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Rate-Increment">>, V}, Vars) ->
    [ list_to_binary([<<"rate_increment=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Rate-Minimum">>, V}, Vars) ->
    [ list_to_binary([<<"rate_minimum=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Surcharge">>, V}, Vars) ->
    [ list_to_binary([<<"surcharge=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Ignore-Early-Media">>, V}, Vars) ->
    [ list_to_binary([<<"ignore_early_media=">>, wh_util:to_list(V)]) | Vars];
%%get_channel_vars({<<"Bypass-Media">>, V}, Vars) ->
%%    [ list_to_binary([<<"bypass_media_after_bridge=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Continue-On-Fail">>, V}, Vars) ->
    [ list_to_binary([<<"continue_on_fail=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Timeout">>, V}, Vars) ->
    [ list_to_binary([<<"leg_timeout=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Progress-Timeout">>, V}, Vars) ->
    [ list_to_binary([<<"leg_progress_timeout=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Delay">>, V}, Vars) ->
    [ list_to_binary([<<"leg_delay_start=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Ignore-Forward">>, V}, Vars) ->
    [ list_to_binary([<<"outbound_redirect_fatal=">>, wh_util:to_list(V)]) | Vars];
get_channel_vars({<<"Overwrite-Channel-Vars">>, V}, Vars) ->
    [ list_to_binary([<<"local_var_clobber=">>, wh_util:to_list(V)]) | Vars];
%% SPECIAL CASE: Custom Channel Vars
get_channel_vars({<<"Custom-Channel-Vars">>, {struct, Custom}}, Vars) ->
    lists:foldl(fun
                    %% These are a temporary abstraction leak until we can locate a call via the API, originate
                    %% on the located server only and transfer to an existing UUID...
                    ({<<"Confirm-File">>, V}, Vars0) ->
                        [ list_to_binary([<<"group_confirm_file=">>, wh_util:to_list(V)]) | Vars0];
                    ({<<"Confirm-Key">>, V}, Vars0) ->
                       [ list_to_binary([<<"group_confirm_key=">>, wh_util:to_list(V)]) | Vars0];
                    ({<<"Confirm-Cancel-Timeout">>, V}, Vars0) ->
                       [ list_to_binary([<<"group_confirm_cancel_timeout=">>, wh_util:to_list(V)]) | Vars0];
                    %% end of leak
                    ({K,V}, Vars0) ->
                       [ list_to_binary([?CHANNEL_VAR_PREFIX, wh_util:to_list(K), "=", wh_util:to_list(V)]) | Vars0]
               end, Vars, Custom);
%% SPECIAL CASE: SIP Headers
get_channel_vars({<<"SIP-Headers">>, {struct, SIPHeaders}}, Vars) ->
    lists:foldl(fun({K,V}, Vars0) ->
			[ list_to_binary(["sip_h_", K, "=", V]) | Vars0]
		end, Vars, SIPHeaders);
%% SPECIAL CASE: Timeout must be larger than zero
get_channel_vars({<<"Timeout">>, V}, Vars) ->
    case wh_util:to_integer(V) of
        TO when TO > 0 ->
            [ <<"call_timeout=", (wh_util:to_binary(TO))/binary>> | Vars];
        _Else ->
            Vars
    end;
get_channel_vars(_, Vars) ->
    Vars.

get_channel_params(Prop) ->
    CV0 = case props:get_value(<<"Tenant-ID">>, Prop) of
	      undefined -> [];
	      TID -> [io_lib:format(?REGISTER_CHANNEL_PARAM
				    ,[list_to_binary([?CHANNEL_VAR_PREFIX, "Tenant-ID"]), TID])]
	  end,
    CV1 = case props:get_value(<<"Access-Group">>, Prop) of
    	      undefined -> CV0;
	      AG -> [io_lib:format(?REGISTER_CHANNEL_PARAM
				   ,[list_to_binary([?CHANNEL_VAR_PREFIX, "Access-Group"]), AG]) | CV0]
	  end,
    {struct, Custom} = props:get_value(<<"Custom-Channel-Vars">>, Prop, ?EMPTY_JSON_OBJECT),
    lists:foldl(fun({K,V}, CV) ->
			[io_lib:format(?REGISTER_CHANNEL_PARAM
				       ,[list_to_binary([?CHANNEL_VAR_PREFIX, K]), V]) | CV]
		end, CV1, Custom).
