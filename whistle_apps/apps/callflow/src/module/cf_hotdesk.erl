%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%-------------------------------------------------------------------
-module(cf_hotdesk).

-include("../callflow.hrl").

-export([handle/2]).

-define(MAX_LOGIN_ATTEMPTS, 3).
-record(hotdesk, {enabled = 'false' :: boolean()
                  ,hotdesk_id :: api_binary()
                  ,pin :: api_binary()
                  ,require_pin = 'false' :: boolean()
                  ,keep_logged_in_elsewhere = 'false' :: boolean()
                  ,owner_id :: api_binary()
                  ,endpoint_ids = [] :: ne_binaries()
                  ,jobj = wh_json:new() :: wh_json:object()
                  ,account_db :: ne_binary()
                 }).
-type hotdesk() :: #hotdesk{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    Action = wh_json:get_value(<<"action">>, Data),

    case get_hotdesk_profile(hotdesk_id(Data, Action, Call), Call) of
        #hotdesk{}=Hotdesk -> handle_action(Action, Hotdesk, Call);
        {'error', _} -> cf_exe:continue(Call)
    end.

hotdesk_id(Data, <<"logout">>, Call) ->
    case cf_attributes:owner_ids(whapps_call:authorizing_id(Call), Call) of
        [] -> wh_json:get_value(<<"id">>, Data);
        [Id] -> wh_json:get_value(<<"id">>, Data, Id);
        [_|_] -> wh_json:get_value(<<"id">>, Data)
    end;
hotdesk_id(Data, _, _) ->
    wh_json:get_value(<<"id">>, Data).

-spec handle_action(ne_binary(), hotdesk(), whapps_call:call()) -> 'ok'.
handle_action(<<"bridge">>, #hotdesk{enabled='false'}, Call) ->
    cf_exe:continue(Call);
handle_action(<<"bridge">>, Hotdesk, Call) ->
    case bridge_to_endpoints(Hotdesk, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the hotdesk"),
            cf_exe:continue(Call);
        {'fail', _}=Failure ->
            cf_util:handle_bridge_failure(Failure, Call);
        {'error', _R} ->
            lager:info("error bridging to hotdesk: ~p", [_R]),
            cf_exe:continue(Call)
    end;
handle_action(<<"login">>, Hotdesk, Call) ->
    login(Hotdesk, Call),
    cf_exe:continue(Call);
handle_action(<<"logout">>, Hotdesk, Call) ->
    whapps_call_command:answer(Call),
    logout(Hotdesk, Call),
    cf_exe:continue(Call);
handle_action(<<"toggle">>, #hotdesk{endpoint_ids=[]}=Hotdesk, Call) ->
    handle_action(<<"login">>, Hotdesk, Call);
handle_action(<<"toggle">>, Hotdesk, Call) ->
    handle_action(<<"logout">>, Hotdesk, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints(hotdesk(), whapps_call:call()) -> {'ok', wh_json:object()} |
                                                            {'fail', wh_json:object()} |
                                                            {'error', _}.

bridge_to_endpoints(#hotdesk{endpoint_ids=EndpointIds}, Call) ->
    Endpoints = build_endpoints(sets:to_list(EndpointIds), Call),
%%    Timeout = wh_json:get_binary_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT),
    IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
    whapps_call_command:b_bridge(Endpoints, ?DEFAULT_TIMEOUT, <<"simultaneous">>, IgnoreEarlyMedia, Call).

build_endpoints(EndpointIds, Call) ->
    build_endpoints(EndpointIds, [], Call).

build_endpoints([], Endpoints, _) -> Endpoints;
build_endpoints([EndpointId|EndpointIds], Endpoints, Call) ->
    case cf_endpoint:build(EndpointId, Call) of
        {'ok', Endpoint} ->
            build_endpoints(EndpointIds, [Endpoint|Endpoints], Call);
        {'error', _} ->
            build_endpoints(EndpointIds, Endpoints, Call)
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
-spec login(hotdesk(), whapps_call:call()) -> 'ok'.
%% TODO: a UI bug keeps the hotdesk enabled from ever being true
%%login(#hotdesk{enabled=false}, Call) ->
%%    whapps_call_command:b_prompt(<<"hotdesk-disabled">>, Call);
login(Hotdesk, Call) -> maybe_require_login_pin(Hotdesk, Call).

-spec maybe_require_login_pin(hotdesk(), whapps_call:call()) -> 'ok'.
maybe_require_login_pin(#hotdesk{require_pin='false'}=Hotdesk, Call) ->
    maybe_logout_elsewhere(Hotdesk, Call);
maybe_require_login_pin(Hotdesk, Call) ->
    require_login_pin(Hotdesk, Call).

-spec require_login_pin(hotdesk(), whapps_call:call()) -> 'ok'.
require_login_pin(Hotdesk, Call) ->
    require_login_pin(Hotdesk, Call, 1).

-spec require_login_pin(hotdesk(), whapps_call:call(), 1..?MAX_LOGIN_ATTEMPTS) -> 'ok'.
require_login_pin(_, Call, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS ->
    lager:info("maximum number of invalid hotdesk pin attempts"),
    whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
    whapps_call_command:b_prompt(<<"vm-goodbye">>, Call);
require_login_pin(#hotdesk{require_pin='true', pin=Pin}=Hotdesk, Call, Loop) ->
    _ = whapps_call_command:answer(Call),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"6">>, <<"hotdesk-enter_pin">>, <<"1">>, Call) of
        {'ok', Pin} ->
            maybe_logout_elsewhere(Hotdesk, Call);
        {'ok', _} ->
            _ = whapps_call_command:play(<<"hotdesk-invalid_entry">>, Call),
            require_login_pin(Hotdesk, Call, Loop + 1);
        {'error', _} ->
            lager:info("caller hungup during login")
    end.

-spec maybe_logout_elsewhere(hotdesk(), whapps_call:call()) -> 'ok'.
maybe_logout_elsewhere(#hotdesk{keep_logged_in_elsewhere='false'}=Hotdesk, Call) ->
    H = remove_from_endpoints(Hotdesk, Call),
    get_authorizing_id(H, Call);
maybe_logout_elsewhere(Hotdesk, Call) ->
    get_authorizing_id(Hotdesk, Call).

-spec get_authorizing_id(hotdesk(), whapps_call:call()) -> 'ok'.
get_authorizing_id(Hotdesk, Call) ->
    login_authorizing_id(whapps_call:authorizing_id(Call), Hotdesk, Call).

-spec login_authorizing_id(api_binary(), hotdesk(), whapps_call:call()) -> 'ok'.
login_authorizing_id('undefined', _, Call) ->
    whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
    whapps_call_command:b_prompt(<<"vm-goodbye">>, Call);
login_authorizing_id(AuthorizingId, #hotdesk{owner_id=OwnerId}=Hotdesk, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Fun = fun(JObj) ->
                  wh_json:set_value([<<"hotdesk">>, <<"users">>, OwnerId], wh_json:new(), JObj) 
          end,
    case update_hotdesk_endpoint(AccountDb, AuthorizingId, Fun) of
        {'ok', _} -> logged_in(Hotdesk, Call);
        {'error', _} ->
            whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            whapps_call_command:b_prompt(<<"vm-goodbye">>, Call)
    end.

-spec logged_in(hotdesk(), whapps_call:call()) -> 'ok'.
logged_in(_, Call) ->
    whapps_call_command:b_prompt(<<"hotdesk-logged_in">>, Call),
    whapps_call_command:b_prompt(<<"vm-goodbye">>, Call).

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
-spec logout(hotdesk(), whapps_call:call()) -> 'ok'.
logout(Hotdesk, Call) -> maybe_keep_logged_in_elsewhere(Hotdesk, Call).

-spec maybe_keep_logged_in_elsewhere(hotdesk(), whapps_call:call()) -> 'ok'.
maybe_keep_logged_in_elsewhere(#hotdesk{keep_logged_in_elsewhere='true'}=Hotdesk
                               ,Call) ->
    keep_logged_in_elsewhere(whapps_call:authorizing_id(Call), Hotdesk, Call);
maybe_keep_logged_in_elsewhere(Hotdesk, Call) ->
    H = remove_from_endpoints(Hotdesk, Call),
    logged_out(H, Call).

-spec keep_logged_in_elsewhere(api_binary(), hotdesk(), whapps_call:call()) -> 'ok'.
keep_logged_in_elsewhere('undefined', Hotdesk, Call) ->
    logged_out(Hotdesk, Call);
keep_logged_in_elsewhere(AuthorizingId, #hotdesk{endpoint_ids=EndpointIds
                                                 ,owner_id=OwnerId
                                                }=Hotdesk, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Fun = fun(JObj) ->
                  wh_json:delete_key([<<"hotdesk">>, <<"users">>, OwnerId], JObj)
          end,
    case update_hotdesk_endpoint(AccountDb, AuthorizingId, Fun) of
        {'ok', _} ->
            UpdatedIds = lists:delete(AuthorizingId, EndpointIds),
            logged_out(Hotdesk#hotdesk{endpoint_ids=UpdatedIds}, Call);
        {'error', _} ->
            whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            whapps_call_command:b_prompt(<<"vm-goodbye">>, Call)
    end.

-spec logged_out(hotdesk(), whapps_call:call()) -> 'ok'.
logged_out(_, Call) ->
    whapps_call_command:b_prompt(<<"hotdesk-logged_out">>, Call),
    whapps_call_command:b_prompt(<<"vm-goodbye">>, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the hotdesk parameters from the datastore and loads the
%% mailbox record
%% @end
%%--------------------------------------------------------------------
-spec get_hotdesk_profile(api_binary(), whapps_call:call()) -> hotdesk() | {'error', _}.
get_hotdesk_profile('undefined', Call) -> find_hotdesk_profile(Call, 1);
get_hotdesk_profile(OwnerId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} -> from_json(JObj, Call);
        {'error', R}=E ->
            lager:info("failed to load hotdesking profile for user ~s: ~p", [OwnerId, R]),
            whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            E
    end.

-spec find_hotdesk_profile(whapps_call:call(), 1..?MAX_LOGIN_ATTEMPTS) ->
                                  hotdesk() | {'error', _}.
find_hotdesk_profile(Call, Loop) when Loop > ?MAX_LOGIN_ATTEMPTS ->
    lager:info("too many failed attempts to get the hotdesk id"),
    whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
    {'error', 'too_many_attempts'};
find_hotdesk_profile(Call, Loop) ->
    whapps_call_command:answer(Call),
    case whapps_call_command:b_prompt_and_collect_digits(<<"1">>, <<"10">>, <<"hotdesk-enter_id">>, <<"1">>, Call) of
        {'ok', <<>>} -> find_hotdesk_profile(Call, Loop + 1);
        {'ok', HotdeskId} ->
            case lookup_hotdesk_id(HotdeskId, Call) of
                #hotdesk{}=Hotdesk -> Hotdesk;
                {'error', _} -> find_hotdesk_profile(Call, Loop + 1)
            end;
        {'error', R}=E ->
            lager:info("failed to get owner id from caller: ~p", [R]),
            whapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            E
    end.

-spec lookup_hotdesk_id(ne_binary(), whapps_call:call()) ->  hotdesk() | {'error', _}.
lookup_hotdesk_id(HotdeskId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    ViewOptions = [{'key', HotdeskId}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/hotdesk_id">>, ViewOptions) of
        {'ok', [JObj]} ->
            lager:info("found hotdesk id ~s", [HotdeskId]),
            from_json(wh_json:get_value(<<"doc">>, JObj), Call);
        _Else ->
            lager:info("failed to load hotdesk id ~s", [HotdeskId]),
            _ = whapps_call_command:play(<<"hotdesk-invalid_entry">>, Call),
            {'error', 'not_found'}
    end.

-spec from_json(wh_json:object(), whapps_call:call()) -> hotdesk().
from_json(JObj, Call) ->
    OwnerId = wh_json:get_value(<<"_id">>, JObj),
    Hotdesk =  wh_json:get_value(<<"hotdesk">>, JObj),
    lager:info("creating hotdesk profile from ~s", [OwnerId]),
    #hotdesk{hotdesk_id = wh_json:get_value(<<"id">>, Hotdesk)
             ,enabled = wh_json:is_true(<<"enabled">>, Hotdesk)
             ,pin = wh_json:get_binary_value(<<"pin">>, Hotdesk)
             ,require_pin = wh_json:is_true(<<"require_pin">>, Hotdesk)
             ,keep_logged_in_elsewhere = wh_json:is_true(<<"keep_logged_in_elsewhere">>, Hotdesk)
             ,endpoint_ids = get_endpoint_ids(OwnerId, Call)
             ,owner_id = OwnerId
            }.

-spec remove_from_endpoints(hotdesk(), whapps_call:call()) -> hotdesk().
remove_from_endpoints(#hotdesk{endpoint_ids=[]}=Hotdesk, _) -> Hotdesk;
remove_from_endpoints(#hotdesk{endpoint_ids=[EndpointId|Endpoints]
                               ,owner_id=OwnerId
                              }=Hotdesk, Call) ->
    AccountDb = whapps_call:account_db(Call),
    Fun = fun(JObj) ->
                  wh_json:delete_key([<<"hotdesk">>, <<"users">>, OwnerId], JObj)
          end,
    _ = update_hotdesk_endpoint(AccountDb, EndpointId, Fun),
    remove_from_endpoints(Hotdesk#hotdesk{endpoint_ids=Endpoints}, Call).

-spec update_hotdesk_endpoint(ne_binary(), api_binary(), function()) -> wh_jobj_return().
update_hotdesk_endpoint(_, 'undefined', _) -> {'error', 'not_found'};
update_hotdesk_endpoint(AccountDb, EndpointId, Fun) when is_binary(EndpointId) ->
    case couch_mgr:open_doc(AccountDb, EndpointId) of
        {'ok', JObj} -> update_hotdesk_endpoint(AccountDb, JObj, Fun);
        {'error', _R}=E ->
            lager:warning("unable to fetch hotdesk endpoint ~s: ~p", [EndpointId, _R]),
            E
    end;
update_hotdesk_endpoint(AccountDb, JObj, Fun) ->
    EndpointId = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:save_doc(AccountDb, Fun(JObj)) of
        {'ok', _}=Ok ->
            _ = cf_util:unsolicited_endpoint_mwi_update(AccountDb, EndpointId),
            Ok;
        {'error', 'conflict'} ->
            update_hotdesk_endpoint(AccountDb, EndpointId, Fun);
        {'error', _R}=E ->
            lager:warning("unable to update hotdesk endpoint ~s: ~p", [EndpointId, _R]),
            E
    end.

-spec get_endpoint_ids(ne_binary(), whapps_call:call()) -> ne_binaries().
get_endpoint_ids(OwnerId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    ViewOptions = [{'key', OwnerId}],
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/hotdesk_users">>, ViewOptions) of
        {'ok', JObjs} ->
            [wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to fetch endpoints used by ~s: ~p", [OwnerId, _R]),
            []
    end.
