%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  4 Aug 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_hotdesk).

-include("../callflow.hrl").

-export([handle/2]).

-record(prompts, {
          enter_hotdesk = <<"/system_media/hotdesk-enter_id">>
          ,enter_password = <<"/system_media/hotdesk-enter_pin">>
          ,invalid_login = <<"/system_media/vm-fail_auth">>
          ,abort_login = <<"/system_media/vm-abort">>
          ,hotdesk_login = <<"/system_media/hotdesk-logged_in">>
          ,hotdesk_logout = <<"/system_media/hotdesk-logged_out">>
	  ,goodbye = <<"/system_media/vm-goodbye">>
	 }).

-import(cf_call_command, [answer/1, play/2, b_play/2, say/3, tones/2, b_record/2
                          ,b_store/3, b_play_and_collect_digits/5, b_play_and_collect_digit/2
                          ,noop/1, b_flush/1, wait_for_dtmf/1, wait_for_application_or_dtmf/2
                          ,audio_macro/2, flush_dtmf/1
			 ]).

-import(cf_call_command, [b_bridge/6, wait_for_unbridge/0]).

-define(MAX_LOGIN_ATTEMPTS, 3).
-define(CF_HOTDESK_VIEW, <<"cf_attributes/hotdesk_id">>).

-record(hotdesk, {
	  enabled = false :: boolean()
          ,hotdesk_id = undefined :: 'undefined' | binary()
	  ,pin = undefined :: 'undefined' | binary()
	  ,login_attempts = 1 :: non_neg_integer()
	  ,require_pin = false :: boolean()
	  ,keep_logged_in_elsewhere = 'false' :: boolean()
	  ,owner_id = undefined :: 'undefined' | binary()
          ,prompts = #prompts{}
	 }).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data, Call) -> no_return() when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    ?LOG(">>> Getting Owner Id"),
    H = case wh_json:get_value(<<"id">>, Data) of
	    undefined ->
		?LOG(">>> owner id undefined in callflow, asking for hotdesk id"),
		P = #prompts{},
		answer(Call),
		{ok, HId} = b_play_and_collect_digits(<<"1">>, <<"10">>, P#prompts.enter_hotdesk, <<"1">>, Call),
		get_hotdesk_profile({hotdesk_id, HId}, Call);
	    UserId ->
		?LOG(">>> owner_id defined in callflow, value ~p", [UserId]),
		get_hotdesk_profile({user_id, UserId}, Call)
	end,

    Devices = cf_attributes:owned_by(H#hotdesk.owner_id, Call, device),
    case wh_json:get_value(<<"action">>, Data) of
	<<"bridge">> ->
	    %% bridge only if hotdesk is enabled
	    case H#hotdesk.enabled of
		<<"true">> ->
		    Endpoints = lists:foldl(fun(Device, Acc) ->
						    case cf_endpoint:build(Device, Call) of
							{ok, Endpoint} -> Endpoint ++ Acc;
							{error, _} -> Acc
						    end
					    end, [], Devices),
		    case bridge_to_endpoints(Endpoints, <<"2000">>, Call) of
			{ok, _} ->
			    {ok, _} = wait_for_unbridge(),
			    CFPid ! {stop};
			{fail, Reason} ->
			    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
			    ?LOG("failed to bridge to hotdesk user ~p:~p", [Code, Cause]),
			    CFPid ! {continue};
			{error, R} ->
			    ?LOG("failed to bridge to hotdesk user ~p, error : ~p", [H#hotdesk.owner_id, R]),
			    CFPid ! {continue}
		    end;
		<<"false">> ->
		    CFPid ! {continue}
	    end;
        <<"login">> ->
            answer(Call),
	    login(Devices, H, Call),
            CFPid ! {stop};
        <<"logout">> ->
            answer(Call),
	    logout(Devices, H, Call),
            CFPid ! {stop};
	<<"toggle">> ->
	    case Devices of
		[] -> login(Devices, H, Call);
		_  -> logout(Devices, H, Call)
	    end,
            CFPid ! {stop}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Conditions that this needs to handle:
%% 0) sanity check the authorizing id
%% 1) Has there been to many attempts to enter a valid pwd/id
%% 2) Do we know the user id? if not ask for the hotdesk id...
%% 3) Is the pin required?
%% 3y) Is the pin valid?
%% 3n) Login
%%--------------------------------------------------------------------

-spec login/3 :: (Devices, H, Call) -> no_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{}.
-spec login/4 :: (Devices, H, Call, Loop) -> no_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{},
      Loop :: non_neg_integer().

login(Devices, H, Call) ->
    login(Devices, H, Call, 1).

login(_, #hotdesk{prompts=#prompts{abort_login=AbortLogin}}, #cf_call{authorizing_id=AId}=Call, _) when AId == <<>> orelse AId == undefined->
    %% sanitize authorizing_id
    b_play(AbortLogin, Call);
login(_, #hotdesk{prompts=#prompts{abort_login=AbortLogin}}, Call, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS->
    %% if we have exceeded the maximum loop attempts then terminate this call
    ?LOG("maximum number of invalid attempts to check mailbox"),
    b_play(AbortLogin, Call);
login(Devices, #hotdesk{hotdesk_id=HId, require_pin=true, pin=Pin, prompts=#prompts{enter_password=EnterPass, invalid_login=InvalidLogin}}=H, Call, Loop)
  when HId =/= undefined orelse HId =/= <<>> ->
    try
        %% Request the pin number from the caller but crash if it doesnt match
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPass, <<"1">>, Call),
        do_login(Devices, H, Call)
    catch
        _:_ ->
            ?LOG("invalid hotdesk PIN ~p", [Pin]),
            {ok, _} = b_play(InvalidLogin, Call),
            login(Devices, H, Call, Loop+1)
    end;
login(Devices, #hotdesk{hotdesk_id=HId, require_pin=false}=H, Call, _)
  when HId =/= undefined orelse HId =/= <<>> ->
    do_login(Devices, H, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do Login process
%% 1) Should the user remain logged in elsewhere?
%% 2y) Change the owner_id of the authorizing object
%% 2n) Remove this owner_id from any devices, then set the auth'n object
%% 3) Infrom the user
%% @end
%%--------------------------------------------------------------------
-spec do_login/3 :: (Devices, H, Call) -> cf_api_std_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{}.
do_login(_, #hotdesk{keep_logged_in_elsewhere=true, owner_id=OwnerId,prompts=#prompts{hotdesk_login=HotdeskLogin, goodbye=Bye}}, #cf_call{authorizing_id=AId, account_db=Db}=Call) ->
    %% keep logged in elsewhere, so we update only the device used to call
    {ok, _} = set_device_owner(Db, AId, OwnerId),
    {ok, _} = b_play(HotdeskLogin, Call),
    b_play(Bye, Call);
do_login(Devices, #hotdesk{keep_logged_in_elsewhere=false, owner_id=OwnerId, prompts=#prompts{hotdesk_login=HotdeskLogin, goodbye=Bye}}, #cf_call{authorizing_id=AId, account_db=Db}=Call) ->
    %% log out from owned devices , since we don't want to keep logged in elsewhere, then log unto the device currently used
    logout_from_elsewhere(Db, Devices),
    set_device_owner(Db, AId, OwnerId),
    {ok, _} = b_play(HotdeskLogin, Call),
    b_play(Bye, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Logout process
%% 0) sanity check the authorizing id
%%
%% Do Logout process
%% 1) Should the user remain logged in elsewhere?
%% 2y) Remove the owner_id of the authorizing object
%% 2n) Remove this owner_id from any devices
%% 3) Infrom the user
%% @end
%%--------------------------------------------------------------------
-spec logout/3 :: (Devices, H, Call) -> cf_api_std_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{}.
logout(_, #hotdesk{keep_logged_in_elsewhere=true, prompts=#prompts{hotdesk_logout=HotdeskLogout, goodbye=Bye}}, #cf_call{authorizing_id=AId, account_db=Db}=Call) ->
    {ok, _} = set_device_owner(Db, AId, <<>>),
    {ok, _} = b_play(HotdeskLogout, Call),
    b_play(Bye, Call);
logout(Devices, #hotdesk{keep_logged_in_elsewhere=false, prompts=#prompts{hotdesk_logout=HotdeskLogout, goodbye=Bye}}, #cf_call{authorizing_id=AId, account_db=Db}=Call) ->
    logout_from_elsewhere(Db, Devices),
    set_device_owner(Db, AId, <<>>),
    {ok, _} = b_play(HotdeskLogout, Call),
    b_play(Bye, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the hotdesk parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_hotdesk_profile/2 :: (Id, Call) -> #hotdesk{} when
      Id :: {_, undefined} | {user_id, binary()} | {hotdesk_id, binary()},
      Call :: #cf_call{}.
get_hotdesk_profile({_, undefined}, _) ->
    #hotdesk{};
get_hotdesk_profile({user_id, Id}, #cf_call{account_db=Db}) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            #hotdesk{
                     hotdesk_id = wh_json:get_value([<<"hotdesk">>, <<"id">>], JObj)
                     ,enabled = wh_json:get_binary_boolean([<<"hotdesk">>, <<"enabled">>], JObj)
                     ,pin = wh_json:get_value([<<"hotdesk">>, <<"pin">>], JObj)
                     ,require_pin = wh_json:get_value([<<"hotdesk">>, <<"require_pin">>], JObj)
                     ,owner_id = Id
                     ,keep_logged_in_elsewhere = wh_json:get_value([<<"hotdesk">>, <<"keep_logged_in_elsewhere">>], JObj)
                    };
        {error, R} ->
            ?LOG("failed to load hotdesking profile for user ~p because ~p", [Id, R]),
            #hotdesk{}
    end;
get_hotdesk_profile({hotdesk_id, HId}, #cf_call{account_db=Db}=Call) ->
    %% get user id from hotdesk id

    case couch_mgr:get_results(Db, <<"cf_attributes/hotdesk_id">>, [{<<"key">>, HId}]) of
	{ok, Doc} ->
	    case Doc of
		[JObj] ->
		    get_hotdesk_profile({user_id, wh_json:get_value([<<"value">>, <<"owner_id">>], JObj)}, Call);
		_ ->
		    %% set incorrect credentials so that the user dont know this hotdesk_id doesnt exist, avoid brute force
		    #hotdesk{hotdesk_id = abc, pin = xyz, require_pin = true}
	    end;
	{error, R} ->
	    ?LOG("failed to get user id from hotdesk id  ~p, ~p", [HId, R]),
	    #hotdesk{}
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
    case b_bridge(Endpoints, Timeout, <<"internal">>, <<"simultaneous">>, IgnoreEarlyMedia, Call) of
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reset owner_id for all specified devices
%% @end
%%--------------------------------------------------------------------
-spec logout_from_elsewhere/2 :: (Db, Devices) -> no_return() when
      Db :: binary(),
      Devices :: list(binary()).
logout_from_elsewhere(Db, Devices) ->
    ?LOG("Db is ~p and devices are ~p~n", [Db, Devices]),
    case Devices of
	undefined -> nothing;
	[]  -> nothing;
	_ ->   lists:foreach(fun(D) -> set_device_owner(Db, D, <<>>) end, Devices)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set owner id for a specified device
%% @end
%%--------------------------------------------------------------------
-spec set_device_owner/3 :: (Db, Device, OwnerId) -> {ok, _} | {error, _} when
      Db :: binary(),
      Device :: binary(),
      OwnerId :: binary().
set_device_owner(Db, Device, OwnerId) ->
    ?LOG(" >> Setting device ~p for owner_id ~p on db ~p", [Device, OwnerId, Db]),
    case couch_mgr:open_doc(Db, Device) of
	{ok, JObj} -> ?LOG(">> setting owner ~p~n to device", [OwnerId]), couch_mgr:save_doc(Db, wh_json:set_value(<<"owner_id">>, OwnerId, JObj));
	{error, _} -> ?LOG(">> error while setting owner for device ~p~n", [Device])
    end.
