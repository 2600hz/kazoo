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

-define(MAX_LOGIN_ATTEMPTS, 3).

-record(hotdesk, {
          hotdesk_id = undefined :: undefined | binary()
	  ,pin = undefined :: binary()
	  ,login_attempts = 1 :: non_neg_integer()
	  ,require_pin = false :: boolean()
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
-spec handle/2 :: (Data, Call) -> no_return() when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    UserId = wh_json:get_value(<<"id">>, Data),
    H = get_hotdesk_profile({user_id, UserId}, Call),
    Devices = cf_attributes:owned_by(H#hotdesk.owner_id, Call, device),
    case wh_json:get_value(<<"action">>, Data) of
	<<"bridge">> ->
	    Endpoints = lists:foldl(fun(Device, Acc) ->
					    case cf_endpoint:build(Device, Call) of
						{ok, Endpoint} -> [Endpoint | Acc];
						{error, _} -> Acc
					    end
				    end, [], Devices),
	    case bridge_to_endpoints(Endpoints, 2000, Call) of
		{ok, _} ->
		    wait_for_unbridge(),
		    CFPid ! {stop};
		{fail, Reason} ->
		    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
		    ?LOG("failed to bridge to hotdesk user ~s:~s", [Code, Cause]),
		    CFPid ! {continue};
		{error, R} ->
		    ?LOG("failed to bridge to hotdesk user ~w, error : ~p", [H#hotdesk.owner_id], R),
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
%%
%% Do Login process
%% 1) Should the user remain logged in elsewhere?
%% 2y) Change the owner_id of the authorizing object
%% 2n) Remove this owner_id from any devices, then set the auth'n object
%% 3) Infrom the user
%% @end
%%--------------------------------------------------------------------

-spec login/3 :: (Devices, H, Call) -> no_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{}.
-spec login/4 :: (Devices, H, Call) -> no_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{},
      Loop :: non_neg_integer().

login(Devices, H, Call) ->
    login(Devices, H, Call, 1).

login(_, #hotdesk{prompts=#prompts{abort_login=AbortLogin}}, #cf_call{authorizing_id=AId}=Call, _) when AId =/= <<>> orelse AId =/= undefined->
    %% sanitize authorizing_id
    b_play(AbortLogin, Call);
login(_, #hotdesk{prompts=#prompts{abort_login=AbortLogin}}, Call, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS->
    %% if we have exceeded the maximum loop attempts then terminate this call
    ?LOG("maximum number of invalid attempts to check mailbox"),
    b_play(AbortLogin, Call);
login(Devices, #hotdesk{hotdesk_id=undefined, prompts=#prompts{enter_hotdesk=EnterHId}}=H, Call, Loop) ->
    %% if the callflow did not define the owner_id, ask for hotdesk id and load hotdesk profile from hotdesk_id
    ?LOG("requesting hotdesk id"),
    {ok, HId} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterHId, <<"1">>, Call),
    H#hotdesk.hotdesk_id = HId,
    login(Devices, H, Call, Loop);
login(Devices, #hotdesk{hotdesk_id=HId, require_pin=true, pin=Pin, prompt=#prompt{enter_password=EnterPass}=H}, Call, Loop) ->
    try
        %% Request the pin number from the caller but crash if it doesnt match
	?LOG("Matching PIN for this hotdesk_id as required"),
        {ok, Pin} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPass, <<"1">>, Call),
        do_login(Devices, H, Call)
    catch
        _:R ->
            ?LOG("invalid mailbox login ~w", [R]),
            {ok, _} = b_play(InvalidLogin, Call),
            login(Devices, H, Call, Loop+1)
    end.
login(Devices, #hotdesk{hotdesk_id=HId, require_pin=false}=H, Call, Loop) ->
    ?LOG("caller is the owner of this mailbox, and requires no pin"),
    do_login(Devices, H, Call).


check_mailbox(#mailbox{exists=false}=Box, Call, Loop) ->
    %% if the callflow did not define the mailbox to check then request the mailbox ID from the user
    find_mailbox(Box, Call, Loop);
check_mailbox(#mailbox{require_pin=false, owner_id=OwnerId}=Box, #cf_call{owner_id=OwnerId}=Call, _) when
      is_binary(OwnerId), OwnerId =/= <<>> ->
    %% If this is the owner of the mailbox calling in and it doesn't require a pin then jump
    %% right to the main menu
    ?LOG("caller is the owner of this mailbox, and requires no pin"),
    main_menu(Box, Call);
check_mailbox(#mailbox{prompts=Prompts, pin = <<>>, exists=true}, Call, _) ->
    %% If the caller is not the owner or the mailbox requires a pin to access it but has none set
    %% then terminate this call.
    ?LOG("attempted to sign into a mailbox with no pin"),
    b_play(Prompts#prompts.no_access, Call);
check_mailbox(#mailbox{prompts=#prompts{enter_password=EnterPass, invalid_login=InvalidLogin}
		       ,pin=Pin}=Box, Call, Loop) ->
    
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
-spec logout/3 :: (Devices, H, Call) -> no_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{}.
logout(Devices, H, Call) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the hotdesk parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_hotdesk_profile/2 :: (Id, Call) -> #hotdesk{} when
      Id :: undefined | binary(),
      Call :: #cf_call{}.
get_hotdesk_profile(undefined, _) ->
    #hotdesk{};
get_hotdesk_profile({user_id, Id}, #cf_call{account_db=Db}) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded hotdesking profile ~s", [Id]),
            #hotdesk{
                     hotdesk_id = wh_json:get_binary_value(<<"id">>, JObj, <<>>)
                     ,pin = wh_json:get_binary_value(<<"pin">>, JObj, <<>>)
                     ,require_pin = wh_json:get_binary_boolean(<<"require_pin">>, JObj, false)
                     ,owner_id = Id
                     ,keep_logged_elsewhere = wh_json:get_binary_value(<<"keep_logged_elsewhere">>, JObj, <<>>)
                    };
        {error, R} ->
            ?LOG("failed to load hotdesking profile ~s, ~w", [Id, R]),
            #hotdesk{}
    end.
get_hotdesk_profile({hotdesk_id, HId}, #cf_call{account_db=Db}) ->
    #hotdesk{}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Find user from its hotdesk ID
%% @end
%%--------------------------------------------------------------------
-spec find_user/4 :: (Devices, H, Call, Loop) -> no_return() when
      Devices :: list(),
      H :: #hotdesk{},
      Call :: #cf_call{},
      Loop :: non_neg_integer().
find_user(Devices, #hotdesk{prompts=#prompts{enter_hotdesk=EnterH, enter_password=EnterPwd, invalid_login=Invalid}}=Box
             ,#cf_call{account_db=Db}=Call, Loop) ->
    ?LOG("requesting mailbox number to check"),
    {ok, Mailbox} = b_play_and_collect_digits(<<"1">>, <<"6">>, EnterBox, <<"1">>, Call),
    BoxNum = try wh_util:to_integer(Mailbox) catch _:_ -> 0 end,

    %% find the voicemail box, by making a fake 'callflow data payload' we look for it now because if the
    %% caller is the owner, and the pin is not required then we skip requesting the pin
    case couch_mgr:get_results(Db, {<<"vmboxes">>, <<"listing_by_mailbox">>}, [{<<"key">>, BoxNum}]) of
        {ok, [JObj]} ->
            ReqBox = get_mailbox_profile({struct, [{<<"id">>, wh_json:get_value(<<"id">>, JObj)}]}, Call),
            check_mailbox(ReqBox, Call, Loop);
        _ ->
            %% we dont want to alert the caller that the mailbox number doesnt match or people could use
            %% that to determine the mailboxs on this system then try brute force to guess the pwd.
            ?LOG("invalid mailbox ~s, faking user out...", [Mailbox]),
            b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPwd, <<"1">>, Call),
            _ = b_play(Invalid, Call),
            check_mailbox(Box, Call, Loop + 1)
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
