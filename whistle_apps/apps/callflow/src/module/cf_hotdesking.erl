%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_hotdesking).

-include("../callflow.hrl").

-export([handle/2]).

-record(prompts, {
	  good_morning = <<"/system_media/ivr-good_morning">>
	 }).

-import(cf_call_command, [answer/1, play/2, b_play/2, say/3, tones/2, b_record/2
                          ,b_store/3, b_play_and_collect_digits/5, b_play_and_collect_digit/2
                          ,noop/1, b_flush/1, wait_for_dtmf/1, wait_for_application_or_dtmf/2
                          ,audio_macro/2, flush_dtmf/1
			 ]).

-import(cf_call_command, [b_bridge/6, wait_for_unbridge/0]).

-record(hotdesk, {
          hotdesk_id = undefined :: undefined | binary()
	  ,pin = undefined :: binary()
	  ,max_login_attempts = 3 :: non_neg_integer()
	  ,require_pin = false :: boolean()
	  ,check_if_owner = true :: boolean()
	  ,keep_logged_elsewhere = false :: boolean()
	  ,owner_id = <<>> :: binary()
          ,prompts = #prompts{}
	 }).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    H = get_hotdesk_profile(Data, Call),
    Devices = cf_attributes:owned_by(H#hotdesk.owner_id, Call, device),
    Endpoints = lists:foldl(fun(Device) -> cf_endpoint:build(Device, Call) end, [], Devices),

    Bridge = bridge_to_endpoints(Endpoints, 2000, Call),
    %wait_for_unbridge(),
    case wh_json:get_value(<<"action">>, Data) of
	<<"bridge">> ->

        <<"login">> ->
            answer(Call),
            CFPid ! {stop};
        <<"logout">> ->
            answer(Call),
            CFPid ! {stop};
        _ ->
            CFPid ! {continue}
    end.

login(Data, Call) ->
    ok.

logout(Data, Call) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the hotdesking parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec(get_hotdesk_profile/2 :: (Data :: json_object(), Call :: #cf_call{}) -> #hotdesk_profile{}).
get_hotdesk_profile(Data, #cf_call{account_db=Db, request_user=ReqUser, last_action=LastAct}) ->
    Id = wh_json:get_value(<<"id">>, Data, <<"undefined">>),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded hotdesking profile ~s", [Id]),
            CheckIfOwner = ((undefined =:= LastAct) orelse (cf_device =:= LastAct)),
            #hotdesking_profile{
                     hotdesk_id = wh_util:to_binary(wh_json:get_value(<<"id">>, JObj, <<>>))
                     ,pin = wh_util:to_binary(wh_json:get_value(<<"pin">>, JObj, <<>>))
                     ,require_pin = wh_util:is_true(wh_json:get_value(<<"require_pin">>, JObj, false))
                     ,check_if_owner = wh_util:is_true(wh_json:get_value(<<"check_if_owner">>, JObj, CheckIfOwner))
                     ,owner_id = ReqUser
                     ,keep_logged_elsewhere = wh_util:to_binary(wh_json:get_value(<<"keep_logged_elsewhere">>, JObj, <<>>))
                    };
        {error, R} ->
            ?LOG("failed to load hotdesking profile ~s, ~w", [Id, R]),
            #hotdesking_profile{}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints/3 :: (Endpoints, Timeout, Call) -> cf_api_bridge_return() when
      Endpoints :: json_objects(),
      Timeout :: binary(),
      Call :: #cf_call{}.
bridge_to_endpoints(Endpoints, Timeout, Call) ->
    IgnoreEarlyMedia = ignore_early_media(Endpoints),
    case b_bridge(Endpoints, Timeout, <<"internal">>, <<"single">>, IgnoreEarlyMedia, Call) of
        {ok, _} ->
            ?LOG("bridged to endpoint"),
            wait_for_unbridge();
        {fail, Reason}=Fail ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("failed to bridge to endpoint ~s:~s", [Code, Cause]),
            Fail;
        {error, R}=Error ->
            ?LOG("failed to bridge to endpoint ~w", [R]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if we should ignore early media
%% @end
%%--------------------------------------------------------------------
-spec ignore_early_media/1 :: (Endpoints) -> binary() when
      Endpoints :: json_objects().
ignore_early_media(Endpoints) ->
    Ignore = lists:foldr(fun(Endpoint, Acc) ->
                                 wh_json:is_true(<<"Ignore-Early-Media">>, Endpoint)
                                     or Acc
                         end, false, Endpoints),
    wh_util:to_binary(Ignore).
