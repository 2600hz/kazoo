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

-export([handle/2, offnet_req/2, wait_for_offnet/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = offnet_req(Data, Call),
    case wait_for_offnet() of
        {<<"SUCCESS">>, _} ->
            lager:debug("completed successful offnet request"),
            cf_exe:stop(Call);
        {Cause, Code} ->
            lager:debug("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
            case (cf_util:handle_bridge_failure(Cause, Call) =:= ok)
                orelse (cf_util:handle_bridge_failure(Code, Call) =:= ok) of
                true -> ok;
                false ->
                    cf_util:send_default_response(Cause, Call),
                    cf_exe:continue(Call)
            end
    end.

-spec offnet_req/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
offnet_req(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    CIDNum = case whapps_call:kvs_fetch(dynamic_cid, Call) of
                 undefined -> CIDNumber;
                 DynamicCID -> DynamicCID
             end,
    Req = [{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, whapps_call:request_user(Call)}
           ,{<<"Account-ID">>, whapps_call:account_id(Call)}
           ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
           ,{<<"Control-Queue">>, cf_exe:control_queue(Call)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Flags">>, wh_json:get_value(<<"flags">>, Data)}
           ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
           ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, Data)}
           ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
           ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
           ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
           ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
           ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
           ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
           ,{<<"SIP-Headers">>, build_sip_headers(Data, Call)}
           ,{<<"Media">>, wh_json:get_value(<<"Media">>, Data)}
           | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)],
    wapi_offnet_resource:publish_req(Req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% @end
%%--------------------------------------------------------------------
-spec wait_for_offnet/0 :: () -> {ne_binary(), ne_binary() | 'undefined'}.
wait_for_offnet() ->
    receive
        {amqp_msg, JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    {wh_json:get_value(<<"Response-Message">>, JObj)
                     ,wh_json:get_value(<<"Response-Code">>, JObj)
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
-spec build_sip_headers/2 :: (wh_json:json_object(), whapps_call:call()) -> 'undefined' | wh_json:json_object().
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
                 
                         
