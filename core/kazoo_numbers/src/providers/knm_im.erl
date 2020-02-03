%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle im features
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_im).

-export([save/2]).
-export([delete/2]).
-export([available/2
        ,available/4
        ]).
-export([settings/2]).

-export([enabled/2]).

-type im_type() :: kapps_im:im_type().

-export_type([im_type/0
             ]).

-include("knm.hrl").

-define(KEY, kz_term:to_binary(Type)).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------

-spec save(im_type(), knm_phone_number:record()) -> knm_phone_number:record().
save(Type, PN) ->
    State = knm_phone_number:state(PN),
    save(Type, PN, State).

-spec save(im_type(), knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(Type, PN, ?NUMBER_STATE_IN_SERVICE) ->
    update_im(Type, PN);
save(Type, PN, _State) ->
    delete(Type, PN).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%------------------------------------------------------------------------------
-spec delete(im_type(), knm_phone_number:record()) -> knm_phone_number:record().
delete(Type, PN) ->
    case knm_phone_number:feature(PN, ?KEY) of
        'undefined' -> PN;
        _Else -> deactivate(Type, PN)
    end.

-spec enabled(knm_phone_number:record(), im_type()) -> boolean().
enabled(PN, Type) ->
    Feature = knm_phone_number:feature(PN, ?KEY),
    case kz_term:is_empty(Feature) of
        'true' -> 'false';
        'false' -> kz_json:is_true(?FEATURE_ENABLED, Feature)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_im(im_type(), knm_phone_number:record()) -> knm_phone_number:record().
update_im(Type, PN) ->
    CurrentFeature = knm_phone_number:feature(PN, ?KEY),
    Feature = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentFeature, Feature),
    case kz_term:is_empty(Feature) of
        'true'
          when CurrentFeature =/= undefined ->
            deactivate(Type, PN);
        'true' ->
            PN;
        'false'
          when NotChanged  ->
            PN;
        'false' ->
            case kz_json:is_true(?FEATURE_ENABLED, Feature) of
                'false'
                  when CurrentFeature =/= undefined ->
                    deactivate(Type, PN);
                'false' ->
                    PN;
                'true' ->
                    activate(Type, PN, Feature)
            end
    end.

-spec feature_available(kz_term:api_ne_binary(), im_type(), kz_json:object()) -> boolean().
feature_available(undefined, _Type, _Config) -> false;
feature_available(AccountId, Type, Config) ->
    Funs = [fun() -> carrier_has_feature_enabled(Type, Config) end
           ,fun() -> kz_services_im:is_enabled(AccountId, Type) end
           ],
    lists:all(fun(Fun) -> Fun() end, Funs).

-spec number_available(knm_phone_number:record()) -> boolean().
number_available(PN) ->
    number_available(knm_phone_number:state(PN), knm_phone_number:assigned_to(PN)).

-spec number_available(kz_term:ne_binary(), kz_term:api_ne_binary()) -> boolean().
number_available(State, AccountId) ->
    Funs = [fun() -> State =:= ?NUMBER_STATE_IN_SERVICE end
           ,fun() -> not kz_term:is_empty(AccountId) end
           ],
    lists:all(fun(Fun) -> Fun() end, Funs).

-spec available(im_type(), knm_phone_number:record()) -> boolean().
available(Type, PN) ->
    Config = knm_gen_carrier:configuration(PN),
    AccountId = knm_phone_number:assigned_to(PN),
    number_available(PN)
        andalso feature_available(AccountId, Type, Config).

-spec available(im_type(), module(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> boolean().
available(_Type, _Module, _State, undefined) -> false;
available(_Type, undefined, _State, _AccountId) -> false;
available(Type, Module, State, AccountId) ->
    Config = knm_gen_carrier:configuration(Module),
    number_available(State, AccountId)
        andalso feature_available(AccountId, Type, Config).

-spec settings(im_type(), knm_phone_number:record()) -> kz_json:object().
settings(Type, PN) ->
    Config = knm_gen_carrier:configuration(PN),
    carrier_settings(Type, Config).

carrier_has_feature_enabled(Type, Config) ->
    kz_term:is_true(carrier_feature(Type, ?FEATURE_ENABLED, Config)).

carrier_feature(Type, Key, Config) ->
    carrier_feature(Type, Key, Config, undefined).

carrier_feature(Type, Key, Config, Default) ->
    K = [<<"features">>, ?KEY, Key],
    kz_json:get_value(K, Config, Default).

carrier_settings(Type, Config) ->
    Props = [{?FEATURE_ENABLED, carrier_has_feature_enabled(Type, Config)}
            ,{?FEATURE_ACTIVATION, carrier_feature(Type, ?FEATURE_ACTIVATION, Config, <<"auto">>)}
            ],
    kz_json:from_list(Props).

activate(Type, PN, Feature) ->
    PN1 = knm_providers:activate_feature(PN, {?KEY, Feature}),
    case knm_phone_number:dry_run(PN1) of
        true -> PN1;
        false -> maybe_set_callback(Type, activate, PN1)
    end.

deactivate(Type, PN) ->
    PN1 = knm_providers:deactivate_feature(PN, ?KEY),
    case knm_phone_number:dry_run(PN1) of
        true -> PN1;
        false -> maybe_set_callback(Type, deactivate, PN1)
    end.

-spec requires_manual_action(im_type(), knm_phone_number:record()) -> boolean().
requires_manual_action(Type, PN) ->
    Config = knm_gen_carrier:configuration(PN),
    carrier_feature(Type, ?FEATURE_ACTIVATION, Config, <<"auto">>) =:= <<"manual">>
        andalso kz_term:is_ne_binary_or_binaries(carrier_feature(Type, ?FEATURE_ACTIVATION_NOTIFY_TO, Config)).

-spec maybe_set_callback(im_type(), atom(), knm_phone_number:record()) -> knm_phone_number:record().
maybe_set_callback(Type, Action, PN) ->
    case requires_manual_action(Type, PN) of
        true ->
            knm_phone_number:add_on_success(PN, {fun send_notify/3, [Type, Action]});
        false ->
            PN
    end.

-spec send_notify(knm_phone_number:record(), im_type(), atom()) -> ok.
send_notify(PN, Type, Action) ->
    Config = knm_gen_carrier:configuration(PN),
    To = carrier_feature(Type, ?FEATURE_ACTIVATION_NOTIFY_TO, Config),
    Feature = kz_json:from_list([{<<"Name">>, ?KEY}
                                ,{<<"Provider">>, knm_phone_number:module_name(PN)}
                                ,{<<"Action">>, kz_term:to_binary(Action)}
                                ]),
    Notification = [{<<"Account-ID">>, knm_phone_number:assigned_to(PN)}
                   ,{<<"Number">>, knm_phone_number:number(PN)}
                   ,{<<"Feature">>, Feature}
                   ,{<<"To">>, kz_json:from_list([{<<"type">>,<<"original">>}
                                                 ,{<<"email_addresses">>, To}
                                                 ])}
                    | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ],
    kz_amqp_worker:cast(Notification, fun kapi_notifications:publish_number_feature_manual_action/1),
    ok.
