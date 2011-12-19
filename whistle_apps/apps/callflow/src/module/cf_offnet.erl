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
-spec handle/2 :: (json_object(), #cf_call{}) -> ok.
handle(Data, #cf_call{account_id=AccountId, request_user=ReqNum, owner_id=OwnerId, authorizing_id=AuthId}=Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(AuthId, OwnerId, <<"emergency">>, Call),
    Req = [{<<"Call-ID">>, cf_exe:callid(Call)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, ReqNum}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Control-Queue">>, cf_exe:control_queue_name(Call)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Flags">>, wh_json:get_value(<<"flags">>, Data)}
           ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
           ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, Data)}
           ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
           ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
           ,{<<"Presence-ID">>, cf_attributes:presence_id(AuthId, Call)}
           ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
           | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)],
    wapi_offnet_resource:publish_req(Req),
    case wait_for_offnet(60000, Call) of
        {ok, _} ->
            ?LOG("completed successful offnet bridge"),
            cf_exe:stop(Call);
        {fail, _}=F ->
            ?CF_ALERT(F, Call),
            cf_util:handle_bridge_failure(F, Call);            
        {error, _}=E ->
            ?CF_ALERT(E, "offnet_resource error", Call),
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on resource completion message
%% @end
%%--------------------------------------------------------------------
-spec wait_for_offnet/2 :: (non_neg_integer(), #cf_call{}) -> {ok | fail, json_object()}
                                                                  | {error, channel_hungup | execution_failed | timeout}.
wait_for_offnet(Timeout, Call) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case wh_util:get_event_type(JObj) of
                { <<"resource">>, <<"offnet_resp">> } ->
                    cf_call_command:wait_for_bridge(infinity, Call);
                { <<"resource">>, <<"resource_error">> } ->
                    {fail, JObj};
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    {error, channel_hungup};
                { <<"error">>, _ } ->
                    {error, execution_failed};
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_offnet(Timeout - (DiffMicro div 1000), Call)
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_offnet(Timeout - (DiffMicro div 1000), Call)
    after
        Timeout ->
            {error, timeout}
    end.
