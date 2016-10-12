%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%% Handle e911 provisioning
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_telnyx_e911).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).
-export([has_emergency_services/1]).

-include("knm.hrl").

-define(KEY, ?TELNYX_KEY).

-define(ADDRESS_ID, <<"address_id">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is saved, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
-spec save(knm_number:knm_number(), ne_binary()) -> knm_number:knm_number().
save(Number) ->
    State = knm_phone_number:state(knm_number:phone_number(Number)),
    save(Number, State).

save(Number, ?NUMBER_STATE_RESERVED) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_IN_SERVICE) ->
    maybe_update_e911(Number);
save(Number, ?NUMBER_STATE_PORT_IN) ->
    maybe_update_e911(Number);
save(Number, _State) ->
    delete(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function is called each time a number is deleted, and will
%% provision e911 or remove the number depending on the state
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(Number) ->
    case feature(Number) of
        'undefined' -> Number;
        _Else ->
            lager:debug("removing e911 information"),
            remove_number(Number),
            knm_services:deactivate_feature(Number, ?KEY)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec has_emergency_services(knm_number:knm_number()) -> boolean().
has_emergency_services(Number) ->
    feature(Number) =/= 'undefined'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?KEY).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_number(knm_number:knm_number()) -> 'ok'.
remove_number(Number) ->
    <<"+", DID/binary>> = knm_phone_number:number(knm_number:phone_number(Number)),
    Body = kz_json:from_list([{<<"e911_enabled">>, 'false'}]),
    Num = "%2B" ++ binary_to_list(DID),
    try knm_telnyx_util:req('put', ["numbers", Num, "e911_settings"], Body) of
        JObj ->
            %%TOOOOODOOOOOO
            %%TOOOOODOOOOOO
            %%TOOOOODOOOOOO: unify with assign_address
            %%TOOOOODOOOOOO
            %%TOOOOODOOOOOO
            case kz_json:is_true(<<"success">>, JObj, 'true') of
                'true' -> remove_number_address(Number);
                _ ->
                    lager:error("cannot disable e911 for ~s: ~s", [DID, kz_json:encode(JObj)])
            end
    catch
        _T:_E -> 'ok'
    end.

remove_number_address(Number) ->
    CarrierData = knm_phone_number:carrier_data(knm_number:phone_number(Number)),
    AddrId = kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData),
    _ = kz_util:spawn(fun knm_telnyx_util:req/2, ['delete', ["e911_addresses", AddrId]]),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_e911(knm_number:knm_number()) -> knm_number:knm_number().
-spec maybe_update_e911(knm_number:knm_number(), boolean()) -> knm_number:knm_number().
maybe_update_e911(Number) ->
    IsDryRun = knm_phone_number:dry_run(knm_number:phone_number(Number)),
    maybe_update_e911(Number, IsDryRun).

maybe_update_e911(Number, 'true') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?KEY, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_identical(CurrentE911, E911),
    case kz_util:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_services:deactivate_feature(Number, ?KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?KEY);
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_services:activate_feature(Number, ?KEY)
    end;

maybe_update_e911(Number, 'false') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?KEY, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_identical(CurrentE911, E911),
    case kz_util:is_empty(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            remove_number(Number),
            knm_services:deactivate_feature(Number, ?KEY);
        'false' when NotChanged  ->
            knm_services:deactivate_feature(Number, ?KEY);
        'false' ->
            lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
            N = knm_services:activate_feature(Number, ?KEY),
            case update_e911(N, E911) of
                {'ok', AddressId, NewFeature} ->
                    PN = knm_phone_number:set_feature(knm_number:phone_number(N), ?KEY, NewFeature),
                    Data = kz_json:from_list([{?ADDRESS_ID, AddressId}]),
                    PN1 = knm_phone_number:update_carrier_data(PN, Data),
                    knm_number:set_phone_number(N, PN1);
                {'error', E} ->
                    lager:error("information update failed: ~p", [E]),
                    knm_errors:unspecified(E, Number)
            end
    end.

%% @private
-spec update_e911(knm_number:knm_number(), kz_json:object()) ->
                         {'ok', ne_binary(), kz_json:object()} |
                         {'error', ne_binary()}.
update_e911(Number, AddressJObj) ->
    remove_number_address(Number),
    case create_address(AddressJObj) of
        {'error', _}=E -> E;
        {'ok', AddressId, _NewFeature}=Ok ->
            case assign_address(Number, AddressId) of
                {'error', _}=E -> E;
                'ok' -> Ok
            end
    end.

-spec create_address(kz_json:object()) ->
                            {'ok', ne_binary()} |
                            {'error', ne_binary() | any()}.
create_address(AddressJObj) ->
    Body = e911_address(AddressJObj),
    try knm_telnyz_util:req('post', ["e911_addresses"], Body) of
        Rep ->
            %% Telnyx has at least 2 different ways of returning errors:
            case 'false' =/= kz_json:get_ne_binary_value(<<"success">>, Rep)
                andalso <<"success">> =:= kz_json:get_ne_binary_value(<<"status">>, Rep)
            of
                'false' -> {'error', reason(Rep)};
                'true' ->
                    AddressId = kz_json:get_ne_binary_value(<<"id">>, Rep),
                    Address = kz_json:delete_keys([<<"id">>, <<"status">>], Rep),
                    {'ok', AddressId, Address}
            end
    catch
        _E:Reason -> {'error', Reason}
    end.

-spec assign_address(knm_number:knm_number(), ne_binary()) ->
                            'ok' | {'error', ne_binary()}.
assign_address(Number, AddressId) ->
    Body = kz_json:from_list([{<<"e911_enabled">>, 'true'}
                             ,{<<"e911_address_id">>, AddressId}
                             ]),
    <<"+", DID/binary>> = knm_phone_number:number(knm_number:phone_number(Number)),
    Num = "%2B" ++ binary_to_list(DID),
    try knm_telnyx_util:req('put', ["numbers", Num, "e911_settings"], Body) of
        Rep ->
            case kz_json:is_true(<<"success">>, Rep, 'true') of
                'true' -> remove_number_address(Number);
                _ ->
                    lager:error("cannot disable e911 for ~s: ~s", [DID, kz_json:encode(Rep)]),
                    {'error', reason(Rep)}
            end
    catch
        _T:_E -> 'ok'
    end.

-spec reason(kz_json:object()) -> ne_binary().
reason(RepJObj) ->
    Message = <<"message">>,
    Reasons = <<"reasons">>,
    Error = kz_json:from_list(
              props:filter_undefined(
                [{Message, kz_json:get_ne_binary_value(Message, RepJObj)}
                ,{Reasons, kz_json:get_value(Reasons, RepJObj)}
                ])),
    kz_json:encode(Error).

-spec e911_address(kz_json:object()) -> kz_json:object().
e911_address(JObj) ->
    kz_json:from_list(
      props:filter_empty(
        [{<<"city">>, cleanse(kz_json:get_ne_binary_value(?E911_CITY, JObj))}
        ,{<<"state">>, cleanse(kz_json:get_ne_binary_value(?E911_STATE, JObj))}
        ,{<<"postal_code">>, kz_json:get_ne_binary_value(?E911_ZIP, JObj)}
        ,{<<"line_1">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET1, JObj))}
        ,{<<"line_2">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET2, JObj))}
        ])).

-spec cleanse(api_ne_binary()) -> api_binary().
cleanse('undefined') -> 'undefined';
cleanse(NEBin) ->
    Upper = kz_json:to_upper_binary(NEBin),
    << <<C>> || <<C>> <= Upper, is_ALnum_or_space(C)>>.

-spec is_ALnum_or_space(char()) -> boolean().
is_ALnum_or_space(C) when $0 =< C andalso C =< $9 -> 'true';
is_ALnum_or_space(C) when $A =< C andalso C =< $Z -> 'true';
is_ALnum_or_space(C) -> $\s =:= C.
