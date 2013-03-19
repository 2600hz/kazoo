%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_offnet).

-include("../callflow.hrl").

-export([handle/2
         ,offnet_req/2
         ,wait_for_offnet/0
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = offnet_req(Data, Call),
    case wait_for_offnet() of
        {<<"SUCCESS">>, _} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(Call);
        {Cause, Code} ->
            lager:info("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
            case (cf_util:handle_bridge_failure(Cause, Call) =:= ok)
                orelse (cf_util:handle_bridge_failure(Code, Call) =:= ok) of
                true -> ok;
                false ->
                    cf_util:send_default_response(Cause, Call),
                    cf_exe:continue(Call)
            end
    end.

-spec offnet_req(wh_json:json_object(), whapps_call:call()) -> 'ok'.
offnet_req(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),

    Endpoint = case cf_endpoint:get(Call) of
                   {ok, JObj} -> JObj;
                   {error, _} -> wh_json:new()
               end,

    Req = [{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, get_to_did(Data, Call)}
           ,{<<"Account-ID">>, whapps_call:account_id(Call)}
           ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
           ,{<<"Control-Queue">>, cf_exe:control_queue(Call)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Flags">>, wh_json:get_value(<<"outbound_flags">>, Endpoint
                                            ,wh_json:get_value(<<"outbound_flags">>, Data))}
           ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
           ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, Data)}
           ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
           ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
           ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
           ,{<<"Outgoing-Caller-ID-Number">>, CIDNumber}
           ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
           ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
           ,{<<"SIP-Headers">>, build_sip_headers(Data, Call)}
           ,{<<"Media">>, wh_json:get_value(<<"Media">>, Data)}
           ,{<<"Force-Fax">>, wh_json:is_true([<<"media">>, <<"fax_option">>], Endpoint)}
           | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)
          ],
    wapi_offnet_resource:publish_req(props:filter_undefined(Req)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Use the e164 normalized number unless the callflow options specify 
%% otherwise
%% @end
%%--------------------------------------------------------------------
-spec get_to_did(wh_json:json_object(), whapps_call:call()) -> ne_binary().
get_to_did(Data, Call) ->
    case wh_json:is_true(<<"do_not_normalize">>, Data) of
        false -> whapps_call:request_user(Call);
        true ->
            Request = whapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),            
            RequestUser
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% @end
%%--------------------------------------------------------------------
-spec wait_for_offnet() -> {ne_binary(), ne_binary() | 'undefined'}.
wait_for_offnet() ->
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    {wh_json:get_value(<<"Response-Message">>, JObj)
                     ,wh_json:get_value(<<"Response-Code">>, JObj)
                    };
                { <<"call_event">>, <<"CHANNEL_DESTROY">> } ->
                    lager:info("recv channel destroy"),
                    {wh_json:get_value(<<"Hangup-Cause">>, JObj)
                     ,wh_json:get_value(<<"Hangup-Code">>, JObj)
                    };
                _ ->
                    wait_for_offnet()
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_offnet()
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% if the callflow data object for this instance of the global resources
%% defines custom sip headers or flags to include custom sip headers
%% build a json object of those now.
%% @end
%%--------------------------------------------------------------------
-spec build_sip_headers(wh_json:json_object(), whapps_call:call()) -> 'undefined' | wh_json:json_object().
build_sip_headers(Data, Call) ->
    Builders = [fun(J) ->
                        case wh_json:is_true(<<"emit_account_id">>, Data) of
                            false -> J;
                            true -> 
                                wh_json:set_value(<<"X-Account-ID">>, whapps_call:account_id(Call), J)
                        end
                end
               ],
    CustomHeaders = wh_json:get_value(<<"custom_sip_headers">>, Data, wh_json:new()),
    JObj = lists:foldl(fun(F, J) -> F(J) end, CustomHeaders, Builders),
    case wh_util:is_empty(JObj) of
        true -> undefined;
        false -> JObj
    end.
