%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle e911 provisioning
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_telnyx_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(ADDRESS_ID, <<"address_id">>).

-define(MOD_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".telnyx">>).

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')
       ).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------

-spec save(knm_phone_number:record()) -> knm_phone_number:record().
save(PN) ->
    State = knm_phone_number:state(PN),
    save(PN, State).

-spec save(knm_phone_number:record(), kz_term:ne_binary()) -> knm_phone_number:record().
save(PN, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(PN);
save(PN, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(PN);
save(PN, ?NUMBER_STATE_PORT_IN) ->
    maybe_update_e911(PN);
save(PN, _State) ->
    delete(PN).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_phone_number:record()) -> knm_phone_number:record().
delete(PN) ->
    case knm_phone_number:feature(PN, ?FEATURE_E911) of
        'undefined' -> PN;
        _Else ->
            lager:debug("removing e911 information"),
            {'ok', NewPN} = remove_number(PN),
            knm_providers:deactivate_feature(NewPN, ?FEATURE_E911)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_e911(knm_phone_number:record()) -> knm_phone_number:record().
maybe_update_e911(PN) ->
    IsDryRun = knm_phone_number:dry_run(PN),
    maybe_update_e911(PN, (IsDryRun
                           orelse ?IS_SANDBOX_PROVISIONING_TRUE
                          )).

-spec maybe_update_e911(knm_phone_number:record(), boolean()) -> knm_phone_number:record().
maybe_update_e911(PN, 'true') ->
    CurrentE911 = knm_phone_number:feature(PN, ?FEATURE_E911),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_providers:deactivate_feature(PN, ?FEATURE_E911);
        'false' when NotChanged  ->
            PN;
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_providers:activate_feature(PN, {?FEATURE_E911, E911})
    end;

maybe_update_e911(PN, 'false') ->
    CurrentE911 = knm_phone_number:feature(PN, ?FEATURE_E911),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(PN)),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_term:is_empty(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            {'ok', NewPN} = remove_number(PN),
            knm_providers:deactivate_feature(NewPN, ?FEATURE_E911);
        'false' when NotChanged  ->
            PN;
        'false' ->
            case update_e911(PN, E911) of
                {'ok', NewPN} ->
                    lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
                    knm_providers:activate_feature(NewPN, {?FEATURE_E911, E911});
                {'error', E} ->
                    lager:error("information update failed: ~p", [E]),
                    knm_errors:unspecified(E, PN)
            end
    end.

-spec update_e911(knm_phone_number:record(), kz_json:object()) ->
          {'ok', knm_phone_number:record()} |
          {'error', kz_term:ne_binary()}.
update_e911(PN, AddressJObj) ->
    remove_number_address(PN),
    case create_address(PN, AddressJObj) of
        {'error', _}=E -> E;
        {'ok', AddressId} -> assign_address(PN, AddressId)
    end.

-spec create_address(knm_phone_number:record(), kz_json:object()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', kz_term:ne_binary() | any()}.
create_address(PN, AddressJObj) ->
    Body = e911_address(PN, AddressJObj),
    try knm_telnyx_util:req('post', ["e911_addresses"], Body) of
        Rep ->
            %% Telnyx has at least 2 different ways of returning errors:
            case 'false' =/= kz_json:get_ne_binary_value(<<"success">>, Rep)
                andalso <<"success">> =:= kz_json:get_ne_binary_value(<<"status">>, Rep)
            of
                'false' -> {'error', reason(Rep)};
                'true' ->
                    AddressId = kz_json:get_ne_binary_value(<<"id">>, Rep),
                    lager:debug("created address ~s"
                               ,[kz_json:encode(kz_json:delete_keys([<<"id">>, <<"status">>], Rep))]),
                    {'ok', AddressId}
            end
    catch
        'throw':{'error', 'by_carrier', _, {_, Reason}} ->
            {'error', Reason}
    end.

-spec assign_address(knm_phone_number:record(), kz_term:ne_binary() | 'null') ->
          {'ok', knm_phone_number:record()} |
          {'error', kz_term:ne_binary()}.
assign_address(PN, AddressId) ->
    IsEnabling = is_binary(AddressId),
    Body = kz_json:from_list([{<<"e911_enabled">>, IsEnabling}
                             ,{<<"e911_address_id">>, AddressId}
                             ]),
    try knm_telnyx_util:req('put', ["numbers", knm_telnyx_util:did(PN), "e911_settings"], Body) of
        Rep ->
            case kz_json:is_true(<<"success">>, Rep, 'true') of
                'true' ->
                    _ = remove_number_address(PN),
                    {'ok', set_address_id(PN, AddressId)};
                _ ->
                    lager:error("cannot ~s e911: ~s", [toogle(IsEnabling), kz_json:encode(Rep)]),
                    {'error', reason(Rep)}
            end
    catch
        ?STACKTRACE(_T, E, ST)
        lager:error("~p ~p", [_T, E]),
        kz_log:log_stacktrace(ST),
        {'error', kz_term:to_binary(E)}
        end.

toogle('true') -> "enable";
toogle('false') -> "disable".

-spec set_address_id(knm_phone_number:record(), kz_term:ne_binary() | 'null') -> knm_phone_number:record().
set_address_id(PN, AddressId) ->
    Data = kz_json:from_list([{?ADDRESS_ID, AddressId}]),
    knm_phone_number:update_carrier_data(PN, Data).

-spec remove_number(knm_phone_number:record()) -> {'ok', knm_phone_number:record()} |
          {'error', kz_term:ne_binary()}.
remove_number(PN) ->
    CarrierData = knm_phone_number:carrier_data(PN),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> {'ok', PN};
        AddressId ->
            lager:debug("removing previously set address: ~p", [AddressId]),
            assign_address(PN, 'null')
    end.

-spec remove_number_address(knm_phone_number:record()) -> 'ok'.
remove_number_address(PN) ->
    CarrierData = knm_phone_number:carrier_data(PN),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> 'ok';
        AddrId ->
            Path = ["e911_addresses", binary_to_list(AddrId)],
            _ = kz_process:spawn(fun() -> catch knm_telnyx_util:req('delete', Path) end),
            'ok'
    end.

-spec reason(kz_json:object()) -> kz_term:ne_binary().
reason(RepJObj) ->
    Message = <<"message">>,
    Reasons = <<"reasons">>,
    Error = kz_json:from_list(
              [{Message, kz_json:get_ne_binary_value(Message, RepJObj)}
              ,{Reasons, kz_json:get_value(Reasons, RepJObj)}
              ]),
    Reason = kz_json:encode(Error),
    lager:error("~s", [Reason]),
    Reason.

-spec e911_address(knm_phone_number:record(), kz_json:object()) -> kz_json:object().
e911_address(PN, JObj) ->
    E911Name = kz_json:get_ne_binary_value(?E911_NAME, JObj),
    CallerName = knm_providers:e911_caller_name(PN, E911Name),
    kz_json:from_list(
      props:filter_empty(
        [{<<"business_name">>, CallerName}
        ,{<<"city">>, cleanse(kz_json:get_ne_binary_value(?E911_CITY, JObj))}
        ,{<<"state">>, cleanse(kz_json:get_ne_binary_value(?E911_STATE, JObj))}
        ,{<<"postal_code">>, kz_json:get_ne_binary_value(?E911_ZIP, JObj)}
        ,{<<"line_1">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET1, JObj))}
        ,{<<"line_2">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET2, JObj))}
        ])).

-spec cleanse(kz_term:api_ne_binary()) -> kz_term:api_binary().
cleanse('undefined') -> 'undefined';
cleanse(NEBin) ->
    Upper = kz_term:to_upper_binary(NEBin),
    << <<C>> || <<C>> <= Upper, is_ALnum_or_space(C)>>.

-spec is_ALnum_or_space(char()) -> boolean().
is_ALnum_or_space(C) when $0 =< C, C =< $9 -> 'true';
is_ALnum_or_space(C) when $A =< C, C =< $Z -> 'true';
is_ALnum_or_space(C) -> $\s =:= C.
