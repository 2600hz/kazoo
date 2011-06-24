%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 6 May 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_endpoint).

-include("callflow.hrl").

-export([build/2, build/3]).

-define(CONFIRM_FILE, <<"/opt/freeswitch/sounds/en/us/callie/ivr/8000/ivr-accept_reject_voicemail.wav">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a endpoint defintion from the database and returns the
%% whistle_api endpoint json
%% @end
%%--------------------------------------------------------------------
-spec(build/2 :: (Id :: binary(), Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
build(EndpointId, Call) ->
    build(EndpointId, ?EMPTY_JSON_OBJECT, Call).

build(EndpointId, Properties, #cf_call{account_db=Db}=Call) ->
    case couch_mgr:open_doc(Db, EndpointId) of
        {ok, Endpoint} ->
            case whistle_util:is_true(wh_json:get_value([<<"call_forward">>, <<"enabled">>], Endpoint)) of
                true ->
                    {ok, [create_call_fwd_endpoint(Endpoint, wh_json:get_value(<<"call_forward">>, Endpoint), Properties, Call)]};
                false ->
                    {ok, [create_endpoint(Endpoint, Properties, Call)]}
            end;
        {error, Reason}=E ->
            ?LOG("failed to load endpoint ~s, ~w", [EndpointId, Reason]),
            E
    end.

create_endpoint(Endpoint, Properties, #cf_call{request_user=ReqUser, inception=Inception}=Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    ?LOG("creating endpoint for ~s", [EndpointId]),

    {CalleeNum, CalleeName} = case Inception of
                                     <<"off-net">> ->
                                         cf_attributes:callee_id(<<"external">>, EndpointId, OwnerId, Call);
                                     _ ->
                                         cf_attributes:callee_id(<<"internal">>, EndpointId, OwnerId, Call)
                                 end,

    SIP = wh_json:get_value(<<"sip">>, Endpoint, ?EMPTY_JSON_OBJECT),
    Media = wh_json:get_value(<<"media">>, Endpoint, ?EMPTY_JSON_OBJECT),

    Prop = [{<<"Invite-Format">>, wh_json:get_value(<<"invite_format">>, SIP, <<"username">>)}
            ,{<<"To-User">>, wh_json:get_value(<<"username">>, SIP)}
            ,{<<"To-Realm">>, wh_json:get_value(<<"realm">>, SIP)}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, SIP, ReqUser)}
            ,{<<"Route">>, wh_json:get_value(<<"route">>, SIP)}
            ,{<<"Callee-ID-Number">>, CalleeNum}
            ,{<<"Callee-ID-Name">>, CalleeName}
            ,{<<"Ignore-Early-Media">>, wh_json:get_binary_boolean(<<"ignore_early_media">>, Media)}
            ,{<<"Bypass-Media">>, wh_json:get_binary_boolean(<<"bypass_media">>, Media)}
            ,{<<"Endpoint-Progress-Timeout">>, wh_json:get_binary_value(<<"progress_timeout">>, Media)}
            ,{<<"Endpoint-Leg-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Leg-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"Codecs">>, wh_json:get_value(<<"codecs">>, Media)}
            ,{<<"SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, SIP)}
            ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Endpoint-ID">>, EndpointId}]}}
           ],
    {struct, [ KV || {_, V}=KV <- Prop, V =/= undefined ]}.

create_call_fwd_endpoint(Endpoint, CallFwd, Properties, #cf_call{request_user=ReqUser, inception=Inception
                                                                 ,authorizing_id=AuthId, owner_id=AuthOwnerId
                                                                 ,cid_number=CIDNum, cid_name=CIDName}=Call) ->
    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
    ?LOG("creating call forward endpoint for ~s", [EndpointId]),

    {CalleeNum, CalleeName} = cf_attributes:callee_id(<<"external">>, EndpointId, OwnerId, Call),

    {CallerNum, CallerName} = case Inception of
                                  <<"off-net">> -> {CIDNum, CIDName};
                                  _ ->
                                      cf_attributes:callee_id(<<"internal">>, AuthId, AuthOwnerId, Call)
                              end,

    SIP = wh_json:get_value(<<"sip">>, Endpoint, ?EMPTY_JSON_OBJECT),
    Media = wh_json:get_value(<<"media">>, Endpoint, ?EMPTY_JSON_OBJECT),

    CCV1 = case whistle_util:is_true(wh_json:get_value(<<"keep_caller_id">>, CallFwd)) of
               true ->
                   [{<<"Call-Forward">>, <<"true">>}
                    ,{<<"Retain-CID">>, <<"true">>}];
               false ->
                   [{<<"Call-Forward">>, <<"true">>}]
           end,

    CCV2 = case whistle_util:is_true(wh_json:get_value(<<"require_keypress">>, CallFwd)) of
               true ->
                   IgnoreEarlyMedia = <<"true">>,
                   [{<<"Confirm-Key">>, <<"1">>}
                    ,{<<"Confirm-Cancel-Timeout">>, <<"2">>}
                    ,{<<"Confirm-File">>, ?CONFIRM_FILE}] ++ CCV1;
               false ->
                   IgnoreEarlyMedia
                       = wh_json:get_binary_boolean(<<"ignore_early_media">>, Media),
                   CCV1
           end,

    Prop = [{<<"Invite-Format">>, <<"route">>}
            ,{<<"To-DID">>, wh_json:get_value(<<"number">>, Endpoint, ReqUser)}
            ,{<<"Route">>, <<"loopback/", (wh_json:get_value(<<"number">>, CallFwd, <<"unknown">>))/binary>>}
            ,{<<"Caller-ID-Number">>, CallerNum}
            ,{<<"Caller-ID-Name">>, CallerName}
            ,{<<"Callee-ID-Number">>, CalleeNum}
            ,{<<"Callee-ID-Name">>, CalleeName}
            ,{<<"Ignore-Early-Media">>, IgnoreEarlyMedia}
            ,{<<"Bypass-Media">>, <<"false">>}
            ,{<<"Endpoint-Leg-Timeout">>, wh_json:get_binary_value(<<"timeout">>, Properties)}
            ,{<<"Endpoint-Leg-Delay">>, wh_json:get_binary_value(<<"delay">>, Properties)}
            ,{<<"SIP-Headers">>, wh_json:get_value(<<"custom_sip_headers">>, SIP)}
            ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Authorizing-ID">>, EndpointId}] ++ CCV2}}
           ],
    {struct, [ KV || {_, V}=KV <- Prop, V =/= undefined ]}.
