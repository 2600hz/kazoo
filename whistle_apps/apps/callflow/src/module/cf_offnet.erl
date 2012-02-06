%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_offnet).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> ok.
handle(Data, #cf_call{account_id=AccountId, from_realm=AccountRealm, request_user=ReqNum}=Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNum, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    Req = [{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, ReqNum}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Account-Realm">>, AccountRealm}
           ,{<<"Control-Queue">>, cf_exe:control_queue_name(Call)}
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
           | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)],
    wapi_offnet_resource:publish_req(Req),
    case wait_for_offnet() of
        {<<"SUCCESS">>, _} ->
            ?LOG("completed successful offnet request"),
            cf_exe:stop(Call);
        {<<"ERROR">>, Msg} ->
            ?LOG("offnet request error: ~p", [Msg]),
            cf_exe:continue(Call);
        {Cause, Code} ->
            cf_util:handle_bridge_failure(Cause, Code, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% @end
%%--------------------------------------------------------------------
-spec wait_for_offnet/0 :: () -> {ne_binary(), ne_binary() | undefined}.
wait_for_offnet() ->
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    {wh_json:get_value(<<"Response-Message">>, JObj)
                     ,wh_json:get_value(<<"Error-Message">>, JObj
                                        ,wh_json:get_value(<<"Response-Code">>, JObj))};
                _ ->
                    wait_for_offnet()
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            wait_for_offnet()
    end.
