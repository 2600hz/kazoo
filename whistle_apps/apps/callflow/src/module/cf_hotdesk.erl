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

-define(MAX_LOGIN_ATTEMPTS, 3).
-define(CF_HOTDESK_VIEW, <<"cf_attributes/hotdesk_id">>).

-record(hotdesk, {enabled = false :: boolean()
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
-spec handle(wh_json:json_object(), whapps_call:call()) -> ok.
handle(Data, Call) ->
    {ok, H} = get_hotdesk_profile(wh_json:get_value(<<"id">>, Data), Call),
    Devices = cf_attributes:fetch_owned_by(H#hotdesk.owner_id, device, Call),
    case wh_json:get_value(<<"action">>, Data) of
        <<"bridge">> when H#hotdesk.enabled ->
            %% bridge only if hotdesk is enabled
            case bridge_to_endpoints(Devices, Data, Call) of
                {ok, _} ->
                    lager:info("completed successful bridge to the hotdesk"),
                    cf_exe:stop(Call);
                {fail, _}=Failure ->
                    cf_util:handle_bridge_failure(Failure, Call);
                {error, _R} ->
                    lager:info("error bridging to hotdesk: ~p", [_R]),
                    cf_exe:continue(Call)
            end;
        <<"bridge">> ->
            cf_exe:continue(Call);
        <<"login">> ->
            whapps_call_command:answer(Call),          
            login(Devices, H, Call),
            cf_exe:stop(Call);
        <<"logout">> ->
            whapps_call_command:answer(Call),
            logout(Devices, H, Call),
            cf_exe:stop(Call);
        <<"toggle">> ->
            case Devices of
                [] -> 
                    login(Devices, H, Call);
                _  -> 
                    logout(Devices, H, Call)
            end,
            cf_exe:stop(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints([ne_binary(),...], wh_json:json_object(), whapps_call:call()) -> {'ok', wh_json:json_object()} |
                                                                                         {'fail', wh_json:json_object()} |
                                                                                         {'error', term()}.
bridge_to_endpoints(Devices, Data, Call) ->
    Endpoints = [Endpoint || Device <- Devices
                                 ,(case (Endpoint = cf_endpoint:build(Device, Data, Call)) of
                                       {error, _} -> false;
                                       {ok, _} -> true
                                   end)],
    Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
    whapps_call_command:b_bridge(Endpoints, Timeout, <<"simultaneous">>, IgnoreEarlyMedia, Call).
    
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
-spec login([ne_binary(),...] | [], #hotdesk{}, whapps_call:call()) -> ok.
-spec login([ne_binary(),...] | [], #hotdesk{}, undefined | ne_binary(), whapps_call:call(), non_neg_integer()) -> ok.

login(Devices, H, Call) ->
    login(Devices, H, whapps_call:authorizing_id(Call), Call, 1).

login(_, #hotdesk{prompts=#prompts{abort_login=AbortLogin}}, undefined, Call, _) ->
    %% sanitize authorizing_id
    _ = whapps_call_command:b_play(AbortLogin, Call),
    ok;
login(_, #hotdesk{prompts=#prompts{abort_login=AbortLogin}}, _, Call, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS->
    %% if we have exceeded the maximum loop attempts then terminate this call
    lager:info("maximum number of invalid attempts to check mailbox"),
    _ = whapps_call_command:b_play(AbortLogin, Call),
    ok;
login(Devices, #hotdesk{prompts=#prompts{enter_password=EnterPass, invalid_login=InvalidLogin}
                        ,require_pin=true, pin=Pin}=H, AuthorizingId, Call, Loop) ->
    case whapps_call_command:b_play_and_collect_digits(<<"1">>, <<"6">>, EnterPass, <<"1">>, Call) of
        {ok, Pin} ->
            do_login(Devices, H, Call);
        {ok, _} ->
            case whapps_call_command:b_play(InvalidLogin, Call) of
                {ok, _} ->
                    login(Devices, H, AuthorizingId, Call, Loop + 1);
                {error, _} ->
                    lager:info("caller hungup during login")
            end;
        {error, _} ->
            lager:info("caller hungup during login")
    end,
    ok;
login(Devices, #hotdesk{require_pin=false}=H, _, Call, _) ->
    do_login(Devices, H, Call),
    ok.

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
-spec do_login([ne_binary(),...] | [], #hotdesk{}, whapps_call:call()) -> ok.
do_login(_, #hotdesk{keep_logged_in_elsewhere=true, owner_id=OwnerId, prompts=#prompts{hotdesk_login=HotdeskLogin, goodbye=Bye}}, Call) ->
    %% keep logged in elsewhere, so we update only the device used to call
    ok = set_device_owner(OwnerId, whapps_call:authorizing_id(Call), Call),
    _ = whapps_call_command:b_play(HotdeskLogin, Call),
    _ = whapps_call_command:b_play(Bye, Call),
    ok;
do_login(Devices, #hotdesk{keep_logged_in_elsewhere=false, owner_id=OwnerId
                           ,prompts=#prompts{hotdesk_login=HotdeskLogin, goodbye=Bye}}, Call) ->
    %% log out from owned devices , since we don't want to keep logged in elsewhere, then log unto the device currently used
    _ = logout_from_elsewhere(Devices, Call),
    ok = set_device_owner(OwnerId, whapps_call:authorizing_id(Call), Call),
    _ = whapps_call_command:b_play(HotdeskLogin, Call),
    _ = whapps_call_command:b_play(Bye, Call),
    ok.

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
-spec logout([ne_binary(),...], #hotdesk{}, whapps_call:call()) -> 'ok'.
logout(_, #hotdesk{keep_logged_in_elsewhere=true, prompts=#prompts{hotdesk_logout=HotdeskLogout, goodbye=Bye}}, Call) ->
    ok = set_device_owner(undefined, whapps_call:authorizing_id(Call), Call),
    _ = whapps_call_command:b_play(HotdeskLogout, Call),
    _ = whapps_call_command:b_play(Bye, Call),
    ok;
logout(Devices, #hotdesk{keep_logged_in_elsewhere=false, prompts=#prompts{hotdesk_logout=HotdeskLogout, goodbye=Bye}}, Call) ->
    _ = logout_from_elsewhere(Devices, Call),
    ok = set_device_owner(undefined, whapps_call:authorizing_id(Call), Call),
    _ = whapps_call_command:b_play(HotdeskLogout, Call),
    _ = whapps_call_command:b_play(Bye, Call),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the hotdesk parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_hotdesk_profile(undefined | ne_binary(), whapps_call:call()) -> {ok, #hotdesk{}} |
                                                                                {error, term()}.
-spec get_hotdesk_profile(undefined | ne_binary(), whapps_call:call(), non_neg_integer()) -> {ok, #hotdesk{}} |
                                                                                                   {error, term()}.

get_hotdesk_profile(OwnerId, Call) ->
    get_hotdesk_profile(OwnerId, Call, 1).

get_hotdesk_profile(_, _, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS ->
    lager:info("too many failed attempts to get the hotdesk id"),
    {error, too_many_attempts};
get_hotdesk_profile(undefined, Call, Loop) ->
    P = #prompts{},
    whapps_call_command:answer(Call),
    case whapps_call_command:b_play_and_collect_digits(<<"1">>, <<"10">>, P#prompts.enter_hotdesk, <<"1">>, Call) of
        {ok, <<>>} ->
            get_hotdesk_profile(undefined, Call, Loop + 1);
        {ok, HId} ->
            %% get user id from hotdesk id
            AccountDb = whapps_call:account_db(Call),
            case couch_mgr:get_results(AccountDb, <<"cf_attributes/hotdesk_id">>, [{<<"key">>, HId}]) of
                {ok, [JObj]} ->
                    lager:info("found hotdesk id ~s", [HId]),
                    get_hotdesk_profile(wh_json:get_ne_value([<<"value">>, <<"owner_id">>], JObj), Call, Loop);
                _ ->
                    lager:info("failed to load hotdesk id ~s", [HId]),
                    %% set incorrect credentials so that the user dont know this hotdesk_id doesnt exist, avoid brute force
                    {ok, #hotdesk{hotdesk_id = abc, pin = xyz, require_pin = true}}
            end;
        {error, R}=E ->
            lager:info("failed to get owner id from caller: ~p", [R]),
            E
    end;
get_hotdesk_profile(OwnerId, Call, _) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, OwnerId) of
        {ok, JObj} ->
            lager:info("using hotdesk owner id ~s", [OwnerId]), 
            {ok, #hotdesk{hotdesk_id = wh_json:get_value([<<"hotdesk">>, <<"id">>], JObj)
                          ,enabled = wh_json:is_true([<<"hotdesk">>, <<"enabled">>], JObj)
                          ,pin = wh_json:get_binary_value([<<"hotdesk">>, <<"pin">>], JObj)
                          ,require_pin = wh_json:is_true([<<"hotdesk">>, <<"require_pin">>], JObj)
                          ,owner_id = OwnerId
                          ,keep_logged_in_elsewhere = wh_json:is_true([<<"hotdesk">>, <<"keep_logged_in_elsewhere">>], JObj)
                         }};
        {error, R}=E ->
            lager:info("failed to load hotdesking profile for user ~s: ~p", [OwnerId, R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reset owner_id for all specified devices
%% @end
%%--------------------------------------------------------------------
-spec logout_from_elsewhere([ne_binary(),...], whapps_call:call()) -> any().
logout_from_elsewhere(Devices, Call) ->
    [set_device_owner(undefined, Device, Call) || Device <- Devices].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Set owner id for a specified device
%% @end
%%--------------------------------------------------------------------
-spec set_device_owner('undefined' | ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
set_device_owner(undefined, Device, Call) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_doc(AccountDb, Device) of
            {ok, JObj} -> 
                lager:info("removing owner id from device ~s in ~s", [Device, AccountDb]),
                couch_mgr:save_doc(AccountDb, wh_json:delete_key(<<"owner_id">>, JObj));
            {error, R} -> 
                lager:info("failed to load device ~s in ~s: ~p", [Device, AccountDb, R])
        end,
    ok;
set_device_owner(OwnerId, Device, Call) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_doc(AccountDb, Device) of
            {ok, JObj} -> 
                lager:info("setting owner id to ~s on device ~s in ~s", [OwnerId, Device, AccountDb]),
                couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"owner_id">>, OwnerId, JObj));
            {error, R} -> 
                lager:info("failed load device ~s in ~s: ~p", [Device, AccountDb, R])
        end,
    ok.
