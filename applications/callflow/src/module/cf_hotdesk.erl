%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Desk is Hot.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>One of the `logout', `login', `toggle' or `bridge' values.</dd>
%%%
%%%   <dt>`id'</dt>
%%%   <dd>User ID.</dd>
%%%
%%%   <dt>`interdigit_timeout'</dt>
%%%   <dd><strong>Optional: </strong>How long to wait for the next DTMF, in milliseconds</dd>
%%% </dl>
%%%
%%% @author Edouard Swiac
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_hotdesk).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(HD_CONFIG_CAT, <<?CF_CONFIG_CAT/binary, ".hotdesk">>).
-define(MAX_LOGIN_ATTEMPTS, kapps_config:get_integer(?HD_CONFIG_CAT, <<"max_login_attempts">>, 3)).
-define(PIN_LENGTH, kapps_config:get_integer(?HD_CONFIG_CAT, <<"max_pin_length">>, 6)).
-define(HOTDESK_ID_LENGTH, kapps_config:get_integer(?HD_CONFIG_CAT, <<"max_hotdesk_id_length">>, 10)).

-record(hotdesk, {enabled = 'false' :: boolean()
                 ,hotdesk_id :: kz_term:api_binary()
                 ,pin :: kz_term:api_binary()
                 ,require_pin = 'false' :: boolean()
                 ,keep_logged_in_elsewhere = 'false' :: boolean()
                 ,owner_id :: kz_term:api_binary()
                 ,endpoint_ids = [] :: kz_term:ne_binaries()
                 ,interdigit_timeout = kapps_call_command:default_interdigit_timeout() :: pos_integer()
                 }).
-type hotdesk() :: #hotdesk{}.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Action = kz_json:get_ne_binary_value(<<"action">>, Data),

    case get_hotdesk_profile(hotdesk_id(Data, Action, Call), Data, Call) of
        {'error', _} -> cf_exe:continue(Call);
        Hotdesk -> handle_action(Action, Hotdesk, Call)
    end.

hotdesk_id(Data, <<"logout">>, Call) ->
    UserId = kz_json:get_ne_binary_value(<<"id">>, Data),

    case UserId =:= 'undefined'
        andalso kz_attributes:owner_ids(kapps_call:authorizing_id(Call), Call)
    of
        'false' -> UserId;
        [Id] -> Id;
        [] -> UserId;
        [_|_] -> UserId
    end;
hotdesk_id(Data, _, _) ->
    kz_json:get_ne_binary_value(<<"id">>, Data).

-spec handle_action(kz_term:ne_binary(), hotdesk(), kapps_call:call()) -> 'ok'.
handle_action(<<"bridge">>, #hotdesk{enabled='false'}, Call) ->
    cf_exe:continue(Call);
handle_action(<<"bridge">>, Hotdesk, Call) ->
    case bridge_to_endpoints(Hotdesk, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the hotdesk"),
            cf_exe:continue(Call);
        {'fail', _}=Failure ->
            case cf_util:handle_bridge_failure(Failure, Call) of
                'ok' -> lager:debug("bridge failure handled");
                'not_found' -> cf_exe:continue(Call)
            end;
        {'error', _R} ->
            lager:info("error bridging to hotdesk: ~p", [_R]),
            cf_exe:continue(Call)
    end;
handle_action(<<"login">>, Hotdesk, Call) ->
    _ = login(Hotdesk, Call),
    cf_exe:continue(Call);
handle_action(<<"logout">>, Hotdesk, Call) ->
    kapps_call_command:answer(Call),
    _ = logout(Hotdesk, Call),
    cf_exe:continue(Call);
handle_action(<<"toggle">>, #hotdesk{endpoint_ids=[]}=Hotdesk, Call) ->
    handle_action(<<"login">>, Hotdesk, Call);
handle_action(<<"toggle">>, Hotdesk, Call) ->
    handle_action(<<"logout">>, Hotdesk, Call).

%%------------------------------------------------------------------------------
%% @doc Attempts to bridge to the endpoints created to reach this device
%% @end
%%------------------------------------------------------------------------------
-spec bridge_to_endpoints(hotdesk(), kapps_call:call()) ->
                                 {'ok', kz_json:object()} |
                                 {'fail', kz_json:object()} |
                                 {'error', any()}.

bridge_to_endpoints(#hotdesk{endpoint_ids=EndpointIds}, Call) ->
    Endpoints = build_endpoints(EndpointIds, Call),
    IgnoreEarlyMedia = kz_endpoints:ignore_early_media(Endpoints),
    kapps_call_command:b_bridge(Endpoints, ?DEFAULT_TIMEOUT_S, <<"simultaneous">>, IgnoreEarlyMedia, Call).

build_endpoints(EndpointIds, Call) ->
    build_endpoints(EndpointIds, [], Call).

build_endpoints([], Endpoints, _) -> Endpoints;
build_endpoints([EndpointId|EndpointIds], Endpoints, Call) ->
    case kz_endpoint:build(EndpointId, Call) of
        {'ok', Endpoint} ->
            build_endpoints(EndpointIds, [Endpoint|Endpoints], Call);
        {'error', _} ->
            build_endpoints(EndpointIds, Endpoints, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Conditions that this needs to handle:
%% 0) sanity check the authorizing id
%% 1) Has there been to many attempts to enter a valid pwd/id
%% 2) Do we know the user id? if not ask for the hotdesk id...
%% 3) Is the pin required?
%% 3y) Is the pin valid?
%% 3n) Login
%%
%% TODO: a UI bug keeps the hotdesk enabled from ever being true
%% @end
%%------------------------------------------------------------------------------
-spec login(hotdesk(), kapps_call:call()) -> kapps_api_std_return().
%% %login(#hotdesk{enabled=false}, Call) ->
%% %    kapps_call_command:b_prompt(<<"hotdesk-disabled">>, Call);
login(Hotdesk, Call) -> maybe_require_login_pin(Hotdesk, Call).

-spec maybe_require_login_pin(hotdesk(), kapps_call:call()) -> kapps_api_std_return().
maybe_require_login_pin(#hotdesk{require_pin='false'}=Hotdesk, Call) ->
    maybe_logout_elsewhere(Hotdesk, Call);
maybe_require_login_pin(Hotdesk, Call) ->
    require_login_pin(Hotdesk, Call).

-spec require_login_pin(hotdesk(), kapps_call:call()) -> kapps_api_std_return().
require_login_pin(Hotdesk, Call) ->
    require_login_pin(Hotdesk, Call, ?MAX_LOGIN_ATTEMPTS, 1).

-spec require_login_pin(hotdesk(), kapps_call:call(), pos_integer(), pos_integer()) ->
                               kapps_api_std_return().
require_login_pin(_, Call, Max, Loop) when Loop > Max ->
    lager:info("maximum number of invalid hotdesk pin attempts"),
    _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
    kapps_call_command:b_prompt(<<"vm-goodbye">>, Call);
require_login_pin(#hotdesk{require_pin='true'
                          ,pin=Pin
                          ,interdigit_timeout=Interdigit
                          }=Hotdesk, Call, Max, Loop) ->
    _ = kapps_call_command:answer(Call),

    NoopId = kapps_call_command:prompt(<<"hotdesk-enter_pin">>, Call),

    case kapps_call_command:collect_digits(?PIN_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', Pin} ->
            maybe_logout_elsewhere(Hotdesk, Call);
        {'ok', _} ->
            _ = kapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            require_login_pin(Hotdesk, Call, Max, Loop + 1);
        {'error', _} ->
            lager:info("caller hungup during login")
    end.

-spec maybe_logout_elsewhere(hotdesk(), kapps_call:call()) ->
                                    kapps_api_std_return().
maybe_logout_elsewhere(#hotdesk{keep_logged_in_elsewhere='false'}=Hotdesk, Call) ->
    H = remove_from_endpoints(Hotdesk, Call),
    get_authorizing_id(H, Call);
maybe_logout_elsewhere(Hotdesk, Call) ->
    get_authorizing_id(Hotdesk, Call).

-spec get_authorizing_id(hotdesk(), kapps_call:call()) ->
                                kapps_api_std_return().
get_authorizing_id(Hotdesk, Call) ->
    login_authorizing_id(kapps_call:authorizing_id(Call), Hotdesk, Call).

-spec login_authorizing_id(kz_term:api_binary(), hotdesk(), kapps_call:call()) ->
                                  kapps_api_std_return().
login_authorizing_id('undefined', _, Call) ->
    _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
    kapps_call_command:b_prompt(<<"vm-goodbye">>, Call);
login_authorizing_id(AuthorizingId, #hotdesk{owner_id=OwnerId}=Hotdesk, Call) ->
    AccountDb = kapps_call:account_db(Call),
    Fun = fun(JObj) ->
                  kz_json:set_value([<<"hotdesk">>, <<"users">>, OwnerId], kz_json:new(), JObj)
          end,
    case update_hotdesk_endpoint(AccountDb, AuthorizingId, Fun) of
        {'ok', _} -> logged_in(Hotdesk, Call);
        {'error', _} ->
            _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            kapps_call_command:b_prompt(<<"vm-goodbye">>, Call)
    end.

-spec logged_in(hotdesk(), kapps_call:call()) ->
                       kapps_api_std_return().
logged_in(_, Call) ->
    _ = kapps_call_command:b_prompt(<<"hotdesk-logged_in">>, Call),
    kapps_call_command:b_prompt(<<"vm-goodbye">>, Call).

%%------------------------------------------------------------------------------
%% @doc Logout process
%% 0) sanity check the authorizing id
%%
%% Do Logout process
%% 1) Should the user remain logged in elsewhere?
%% 2y) Remove the owner_id of the authorizing object
%% 2n) Remove this owner_id from any devices
%% 3) Infrom the user
%% @end
%%------------------------------------------------------------------------------
-spec logout(hotdesk(), kapps_call:call()) -> kapps_api_std_return().
logout(Hotdesk, Call) -> maybe_keep_logged_in_elsewhere(Hotdesk, Call).

-spec maybe_keep_logged_in_elsewhere(hotdesk(), kapps_call:call()) ->
                                            kapps_api_std_return().
maybe_keep_logged_in_elsewhere(#hotdesk{keep_logged_in_elsewhere='true'}=Hotdesk
                              ,Call) ->
    keep_logged_in_elsewhere(kapps_call:authorizing_id(Call), Hotdesk, Call);
maybe_keep_logged_in_elsewhere(Hotdesk, Call) ->
    H = remove_from_endpoints(Hotdesk, Call),
    logged_out(H, Call).

-spec keep_logged_in_elsewhere(kz_term:api_binary(), hotdesk(), kapps_call:call()) ->
                                      kapps_api_std_return().
keep_logged_in_elsewhere('undefined', Hotdesk, Call) ->
    logged_out(Hotdesk, Call);
keep_logged_in_elsewhere(AuthorizingId, #hotdesk{endpoint_ids=EndpointIds
                                                ,owner_id=OwnerId
                                                }=Hotdesk, Call) ->
    AccountDb = kapps_call:account_db(Call),
    Fun = fun(JObj) ->
                  kz_json:delete_key([<<"hotdesk">>, <<"users">>, OwnerId], JObj)
          end,
    case update_hotdesk_endpoint(AccountDb, AuthorizingId, Fun) of
        {'ok', _} ->
            UpdatedIds = lists:delete(AuthorizingId, EndpointIds),
            logged_out(Hotdesk#hotdesk{endpoint_ids=UpdatedIds}, Call);
        {'error', _} ->
            _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            kapps_call_command:b_prompt(<<"vm-goodbye">>, Call)
    end.

-spec logged_out(hotdesk(), kapps_call:call()) -> kapps_api_std_return().
logged_out(_, Call) ->
    _ = kapps_call_command:b_prompt(<<"hotdesk-logged_out">>, Call),
    kapps_call_command:b_prompt(<<"vm-goodbye">>, Call).

%%------------------------------------------------------------------------------
%% @doc Fetches the hotdesk parameters from the datastore and loads the
%% mailbox record
%% @end
%%------------------------------------------------------------------------------
-spec get_hotdesk_profile(kz_term:api_binary(), kz_json:object(), kapps_call:call()) ->
                                 hotdesk() |
                                 {'error', any()}.
get_hotdesk_profile('undefined', Data, Call) ->
    find_hotdesk_profile(Call, Data, ?MAX_LOGIN_ATTEMPTS, 1);
get_hotdesk_profile(OwnerId, Data, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} -> from_json(JObj, Data, Call);
        {'error', _R}=E ->
            lager:info("failed to load hotdesking profile for user ~s: ~p", [OwnerId, _R]),
            _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            E
    end.

-spec find_hotdesk_profile(kapps_call:call(), kz_json:object(), pos_integer(), pos_integer()) ->
                                  hotdesk() |
                                  {'error', any()}.
find_hotdesk_profile(Call, _Data, Max, Loop) when Loop > Max ->
    lager:info("too many failed attempts to get the hotdesk id"),
    _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
    {'error', 'too_many_attempts'};
find_hotdesk_profile(Call, Data, Max, Loop) ->
    kapps_call_command:answer(Call),

    NoopId = kapps_call_command:prompt(<<"hotdesk-enter_id">>, Call),
    Interdigit = kz_json:get_integer_value(<<"interdigit_timeout">>, Data, kapps_call_command:default_interdigit_timeout()),

    case kapps_call_command:collect_digits(?HOTDESK_ID_LENGTH
                                          ,kapps_call_command:default_collect_timeout()
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', <<>>} ->
            find_hotdesk_profile(Call, Data, Max, Loop + 1);
        {'ok', HotdeskId} ->
            case lookup_hotdesk_id(HotdeskId, Data, Call) of
                #hotdesk{}=Hotdesk -> Hotdesk;
                {'error', _} ->
                    find_hotdesk_profile(Call, Data, Max, Loop + 1)
            end;
        {'error', R}=E ->
            lager:info("failed to get owner id from caller: ~p", [R]),
            _ = kapps_call_command:b_prompt(<<"hotdesk-abort">>, Call),
            E
    end.

-spec lookup_hotdesk_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
                               hotdesk() |
                               {'error', any()}.
lookup_hotdesk_id(HotdeskId, Data, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'key', HotdeskId}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"attributes/hotdesk_id">>, ViewOptions) of
        {'ok', [JObj]} ->
            lager:info("found hotdesk id ~s", [HotdeskId]),
            from_json(kz_json:get_json_value(<<"doc">>, JObj), Data, Call);
        _Else ->
            lager:info("failed to load hotdesk id ~s", [HotdeskId]),
            _ = kapps_call_command:play(<<"hotdesk-invalid_entry">>, Call),
            {'error', 'not_found'}
    end.

-spec from_json(kz_json:object(), kz_json:object(), kapps_call:call()) -> hotdesk().
from_json(JObj, Data, Call) ->
    OwnerId = kz_doc:id(JObj),
    Hotdesk =  kz_json:get_json_value(<<"hotdesk">>, JObj),
    lager:info("creating hotdesk profile from ~s", [OwnerId]),
    #hotdesk{hotdesk_id = kz_doc:id(Hotdesk)
            ,enabled = kz_json:is_true(<<"enabled">>, Hotdesk)
            ,pin = kz_json:get_binary_value(<<"pin">>, Hotdesk)
            ,require_pin = kz_json:is_true(<<"require_pin">>, Hotdesk)
            ,keep_logged_in_elsewhere = kz_json:is_true(<<"keep_logged_in_elsewhere">>, Hotdesk)
            ,endpoint_ids = get_endpoint_ids(OwnerId, Call)
            ,owner_id = OwnerId
            ,interdigit_timeout = kz_json:find(<<"interdigit_timeout">>, [JObj, Data], kapps_call_command:default_interdigit_timeout())
            }.

-spec remove_from_endpoints(hotdesk(), kapps_call:call()) -> hotdesk().
remove_from_endpoints(#hotdesk{endpoint_ids=[]}=Hotdesk, _) -> Hotdesk;
remove_from_endpoints(#hotdesk{endpoint_ids=[EndpointId|Endpoints]
                              ,owner_id=OwnerId
                              }=Hotdesk, Call) ->
    AccountDb = kapps_call:account_db(Call),
    Fun = fun(JObj) ->
                  kz_json:delete_key([<<"hotdesk">>, <<"users">>, OwnerId], JObj)
          end,
    _ = update_hotdesk_endpoint(AccountDb, EndpointId, Fun),
    remove_from_endpoints(Hotdesk#hotdesk{endpoint_ids=Endpoints}, Call).

-spec update_hotdesk_endpoint(kz_term:ne_binary(), kz_term:api_binary() | kz_json:object(), function()) ->
                                     kz_term:jobj_return().
update_hotdesk_endpoint(_, 'undefined', _) -> {'error', 'not_found'};
update_hotdesk_endpoint(AccountDb, EndpointId, Fun) when is_binary(EndpointId) ->
    case kz_datamgr:open_doc(AccountDb, EndpointId) of
        {'ok', JObj} -> update_hotdesk_endpoint(AccountDb, JObj, Fun);
        {'error', _R}=E ->
            lager:warning("unable to fetch hotdesk endpoint ~s: ~p", [EndpointId, _R]),
            E
    end;
update_hotdesk_endpoint(AccountDb, JObj, Fun) ->
    EndpointId = kz_doc:id(JObj),
    case kz_datamgr:save_doc(AccountDb, Fun(JObj)) of
        {'ok', _}=Ok ->
            _ = cf_util:unsolicited_endpoint_mwi_update(AccountDb, EndpointId),
            Ok;
        {'error', 'conflict'} ->
            update_hotdesk_endpoint(AccountDb, EndpointId, Fun);
        {'error', _R}=E ->
            lager:warning("unable to update hotdesk endpoint ~s: ~p", [EndpointId, _R]),
            E
    end.

-spec get_endpoint_ids(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binaries().
get_endpoint_ids(OwnerId, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'key', OwnerId}],
    case kz_datamgr:get_results(AccountDb, <<"attributes/hotdesk_users">>, ViewOptions) of
        {'ok', JObjs} ->
            [kz_doc:id(JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:warning("unable to fetch endpoints used by ~s: ~p", [OwnerId, _R]),
            []
    end.
