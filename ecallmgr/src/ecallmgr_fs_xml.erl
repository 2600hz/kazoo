%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Generate the XML for various FS responses
%%% @end
%%% Created : 25 Mar 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_xml).

-export([route_resp_xml/1, build_route/2, get_leg_vars/1, auth_resp_xml/1]).

-include("ecallmgr.hrl").

auth_resp_xml({struct, RespProp}) ->
    auth_resp_xml(RespProp);
auth_resp_xml(RespProp) ->
    auth_resp_xml(props:get_value(<<"Auth-Method">>, RespProp), RespProp).

auth_resp_xml(<<"password">>, Prop) ->
    User = props:get_value(<<"Auth-User">>, Prop),
    Domain = props:get_value(<<"Auth-Domain">>, Prop),
    Pass = props:get_value(<<"Auth-Password">>, Prop),
    ChannelParams = get_channel_params(Prop),
    {ok, lists:flatten(io_lib:format(?REGISTER_PASS_RESPONSE, [Domain, User, Pass, ChannelParams]))};
auth_resp_xml(<<"a1-hash">>, Prop) ->
    User = props:get_value(<<"Auth-User">>, Prop),
    Domain = props:get_value(<<"Auth-Domain">>, Prop),
    Hash = props:get_value(<<"Auth-Password">>, Prop),
    ChannelParams = get_channel_params(Prop),
    {ok, lists:flatten(io_lib:format(?REGISTER_HASH_RESPONSE, [Domain, User, Hash, ChannelParams]))};
auth_resp_xml(<<"ip">>, _Prop) ->
    {ok, ?EMPTYRESPONSE};
auth_resp_xml(_, _) ->
    {ok, ?EMPTYRESPONSE}.

route_resp_xml({struct, RespProp}) ->
    route_resp_xml(RespProp);
route_resp_xml(RespProp) ->
    route_resp_xml(props:get_value(<<"Method">>, RespProp), props:get_value(<<"Routes">>, RespProp), RespProp).

%% Prop = Route Response
route_resp_xml(<<"bridge">>, Routes, _Prop) ->
    logger:format_log(info, "R_R_XML(~p): BRIDGEXML: Routes:~n~p~n", [self(), Routes]),
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
	    logger:format_log(info, "R_R_XML(~p): ErrorXML: ~s~n", [self(), Errors]),
	    {ok, lists:flatten(io_lib:format(?ROUTE_BRIDGE_RESPONSE, [Errors]))};
	_ ->
	    logger:format_log(info, "R_R_XML(~p): RoutesXML: ~s~n", [self(), Extensions]),
	    {ok, lists:flatten(io_lib:format(?ROUTE_BRIDGE_RESPONSE, [Extensions]))}
    end;
route_resp_xml(<<"park">>, _Routes, _Prop) ->
    {ok, ?ROUTE_PARK_RESPONSE};
route_resp_xml(<<"error">>, _Routes, Prop) ->
    ErrCode = props:get_value(<<"Route-Error-Code">>, Prop),
    ErrMsg = list_to_binary([" ", props:get_value(<<"Route-Error-Message">>, Prop, <<"">>)]),
    logger:format_log(info, "R_R_XML(~p): ErrorXML: ~s ~s~n", [self(), ErrCode, ErrMsg]),
    {ok, lists:flatten(io_lib:format(?ROUTE_ERROR_RESPONSE, [ErrCode, ErrMsg]))}.

-spec(build_route/2 :: (RouteProp :: proplist() | json_object(), DIDFormat :: binary()) -> binary() | tuple(error, timeout)).
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
	    E
    end.

-spec(format_did/2 :: (DID :: binary(), Format :: binary()) -> binary()).
format_did(DID, <<"e164">>) ->
    whistle_util:to_e164(DID);
format_did(DID, <<"npan">>) ->
    whistle_util:to_npan(DID);
format_did(DID, <<"1npan">>) ->
    whistle_util:to_1npan(DID).

-spec(get_leg_vars/1 :: (JObj :: json_object() | proplist()) -> iolist()).
get_leg_vars({struct, Prop}) -> get_leg_vars(Prop);
get_leg_vars(Prop) ->
    ["[", string:join([binary_to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], Prop)], ","), "]"].

-spec(get_channel_vars/1 :: (JObj :: json_object() | proplist()) -> string()).
get_channel_vars({struct, Prop}) -> get_channel_vars(Prop);
get_channel_vars(Prop) ->
    ["{", string:join([binary_to_list(V) || V <- lists:foldr(fun get_channel_vars/2, [], Prop)], ","), "}"].

-spec(get_channel_vars/2 :: (Pair :: tuple(binary(), term()), Vars :: list(binary())) -> list(binary())).
get_channel_vars({<<"Auth-User">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_username='", V, "'"]) | Vars];
get_channel_vars({<<"Auth-Password">>, V}, Vars) ->
    [ list_to_binary(["sip_auth_password='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Name">>, V}, Vars) ->
    [ list_to_binary(["origination_caller_id_name='", V, "'"]) | Vars];
get_channel_vars({<<"Caller-ID-Number">>, V}, Vars) ->
    [ list_to_binary(["origination_caller_id_number='", V, "'"]) | Vars];
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
get_channel_vars({<<"Codecs">>, Cs}, Vars) ->
    Codecs = [ binary_to_list(C) || C <- Cs ],
    CodecStr = string:join(Codecs, ","),
    [ list_to_binary(["absolute_codec_string='", CodecStr, "'"]) | Vars];
get_channel_vars({<<"Progress-Timeout">>, V}, Vars) ->
    [ list_to_binary([<<"progress_timeout=">>, V]) | Vars];
get_channel_vars({<<"Rate">>, V}, Vars) ->
    [ list_to_binary([<<"rate=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Rate-Increment">>, V}, Vars) ->
    [ list_to_binary([<<"rate_increment=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Rate-Minimum">>, V}, Vars) ->
    [ list_to_binary([<<"rate_minimum=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Surcharge">>, V}, Vars) ->
    [ list_to_binary([<<"surcharge=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Ignore-Early-Media">>, V}, Vars) ->   
    [ list_to_binary([<<"ignore_early_media=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Bypass-Media">>, V}, Vars) ->   
    [ list_to_binary([<<"bypass_media=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Continue-On-Fail">>, V}, Vars) ->   
    [ list_to_binary([<<"continue_on_fail=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Timeout">>, V}, Vars) ->   
    [ list_to_binary([<<"leg_timeout=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Progress-Timeout">>, V}, Vars) ->   
    [ list_to_binary([<<"leg_progress_timeout=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Delay">>, V}, Vars) ->   
    [ list_to_binary([<<"leg_delay_start=">>, whistle_util:to_list(V)]) | Vars];
get_channel_vars({<<"Endpoint-Ignore-Forward">>, V}, Vars) ->   
    [ list_to_binary([<<"outbound_redirect_fatal=">>, whistle_util:to_list(V)]) | Vars];
%% SPECIAL CASE: Custom Channel Vars
get_channel_vars({<<"Custom-Channel-Vars">>, {struct, Custom}}, Vars) ->
    lists:foldl(fun
                    %% These are a temporary abstraction leak until we can locate a call via the API, originate 
                    %% on the located server only and transfer to an existing UUID...
                    ({<<"Confirm-File">>, V}, Vars0) ->
                        [ list_to_binary([<<"group_confirm_file=">>, whistle_util:to_list(V)]) | Vars0];
                    ({<<"Confirm-Key">>, V}, Vars0) ->
                       [ list_to_binary([<<"group_confirm_key=">>, whistle_util:to_list(V)]) | Vars0];
                    ({<<"Confirm-Cancel-Timeout">>, V}, Vars0) ->   
                       [ list_to_binary([<<"group_confirm_cancel_timeout=">>, whistle_util:to_list(V)]) | Vars0];
                    %% end of leak
                    ({K,V}, Vars0) ->                        
                       [ list_to_binary([?CHANNEL_VAR_PREFIX, whistle_util:to_list(K), "=", whistle_util:to_list(V)]) | Vars0]
               end, Vars, Custom);
%% SPECIAL CASE: SIP Headers
get_channel_vars({<<"SIP-Headers">>, {struct, [_]}=SIPHeaders}, Vars) ->
    lists:foldl(fun({K,V}, Vars0) ->
			[ list_to_binary(["sip_h_", K, "=", V]) | Vars0]
		end, Vars, SIPHeaders);
get_channel_vars({_K, _V}, Vars) ->
    %logger:format_log(info, "L/U.route(~p): Unknown channel var ~p::~p~n", [self(), _K, _V]),
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
