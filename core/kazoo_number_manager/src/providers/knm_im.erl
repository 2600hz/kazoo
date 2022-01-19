%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
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

-type knm_number() :: knm_number:knm_number().
-type knm_phone_number() :: knm_phone_number:knm_phone_number().
-type im_number() :: knm_number() | knm_phone_number().

-export_type([im_type/0]).

-include("knm.hrl").

-define(KEY, kz_term:to_binary(Type)).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------

-spec save(im_type(), knm_number()) -> knm_number().
save(Type, Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Type, Number, State).

-spec save(im_type(), knm_number(), kz_term:ne_binary()) -> knm_number().
save(Type, Number, ?NUMBER_STATE_IN_SERVICE) ->
    update_im(Type, Number);
save(Type, Number, _State) ->
    delete(Type, Number).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%------------------------------------------------------------------------------
-spec delete(im_type(), knm_number()) -> knm_number().
delete(Type, Number) ->
    case feature(Type, Number) of
        'undefined' -> Number;
        _Else -> deactivate(Type, Number)
    end.

-spec enabled(im_number(), im_type()) -> boolean().
enabled(PN, Type) ->
    Feature = feature(Type, PN),
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
-spec update_im(im_type(),knm_number()) -> knm_number().
update_im(Type, Number) ->
    CurrentFeature = feature(Type, Number),
    PN = knm_number:phone_number(Number),
    Feature = kz_json:get_ne_value(?KEY, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentFeature, Feature),

    case kz_term:is_empty(Feature) of
        'true'
          when CurrentFeature =/= 'undefined' ->
            deactivate(Type, Number);
        'true' ->
            Number;
        'false'
          when NotChanged  ->
            Number;
        'false' ->
            case kz_json:is_true(?FEATURE_ENABLED, Feature) of
                'false'
                  when CurrentFeature =/= 'undefined' ->
                    deactivate(Type, Number);
                'false' ->
                    Number;
                'true' ->
                    activate(Type, Number, Feature)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec feature(im_type(), im_number()) -> kz_json:api_json_term().
feature(Type, Number) ->
    case knm_phone_number:is_phone_number(Number) of
        'true' -> knm_phone_number:feature(Number, ?KEY);
        'false' -> knm_phone_number:feature(knm_number:phone_number(Number), ?KEY)
    end.

-spec feature_available(kz_term:api_ne_binary(), im_type(), kz_json:object()) -> boolean().
feature_available('undefined', _Type, _Config) -> 'false';
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

-spec available(im_type(), knm_phone_number()) -> boolean().
available(Type, PN) ->
    Config = knm_gen_carrier:configuration(PN),
    AccountId = knm_phone_number:assigned_to(PN),
    number_available(PN)
        andalso feature_available(AccountId, Type, Config).

-spec available(im_type(), module(), kz_term:ne_binary(), kz_term:api_ne_binary()) -> boolean().
available(_Type, _Module, _State, 'undefined') -> 'false';
available(_Type, 'undefined', _State, _AccountId) -> 'false';
available(Type, Module, State, AccountId) ->
    Config = knm_gen_carrier:configuration(Module),
    number_available(State, AccountId)
        andalso feature_available(AccountId, Type, Config).

-spec settings(im_type(), knm_phone_number()) -> kz_json:object().
settings(Type, PN) ->
    Config = knm_gen_carrier:configuration(PN),
    carrier_settings(Type, Config).

carrier_has_feature_enabled(Type, Config) ->
    kz_term:is_true(carrier_feature(Type, ?FEATURE_ENABLED, Config)).

carrier_feature(Type, Key, Config) ->
    carrier_feature(Type, Key, Config, 'undefined').

carrier_feature(Type, Key, Config, Default) ->
    K = [<<"features">>, ?KEY, Key],
    kz_json:get_value(K, Config, Default).

carrier_settings(Type, Config) ->
    Props = [{?FEATURE_ENABLED, carrier_has_feature_enabled(Type, Config)}
            ,{?FEATURE_ACTIVATION, carrier_feature(Type, ?FEATURE_ACTIVATION, Config, <<"auto">>)}
            ],
    kz_json:from_list(Props).

-spec activate(im_type(), knm_number(), kz_json:object()) -> knm_number().
activate(Type, Number0, Feature) ->
    Number = knm_providers:activate_feature(Number0, {?KEY, Feature}),
    case knm_phone_number:dry_run(knm_number:phone_number(Number)) of
        'true' -> Number;
        'false' -> maybe_set_callback(Type, 'activate', Number)
    end.

-spec deactivate(im_type(), knm_number()) -> knm_number().
deactivate(Type, Number0) ->
    Number = knm_providers:deactivate_feature(Number0, ?KEY),
    case knm_phone_number:dry_run(knm_number:phone_number(Number)) of
        'true' -> Number;
        'false' -> maybe_set_callback(Type, 'deactivate', Number)
    end.

-spec requires_manual_action(im_type(), knm_phone_number()) -> boolean().
requires_manual_action(Type, PN) ->
    Config = knm_gen_carrier:configuration(PN),
    carrier_feature(Type, ?FEATURE_ACTIVATION, Config, <<"auto">>) =:= <<"manual">>
        andalso kz_term:is_ne_binary_or_binaries(carrier_feature(Type, ?FEATURE_ACTIVATION_NOTIFY_TO, Config)).

-spec maybe_set_callback(im_type(), atom(), knm_number()) -> knm_number().
maybe_set_callback(Type, Action, Number) ->
    PN = knm_number:phone_number(Number),
    case requires_manual_action(Type, PN) of
        'true' ->
            PN1 = knm_phone_number:add_on_success(PN, {fun send_notify/3, [Type, Action]}),
            knm_number:set_phone_number(Number, PN1);
        'false' ->
            Number
    end.

-spec send_notify(knm_phone_number(), im_type(), atom()) -> 'ok'.
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
    kz_amqp_worker:cast(Notification, fun kapi_notifications:publish_number_feature_manual_action/1).
