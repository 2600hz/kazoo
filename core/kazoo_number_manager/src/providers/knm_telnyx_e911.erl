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

-include("knm.hrl").

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
            {'ok', NewNumber} = remove_number(Number),
            knm_services:deactivate_feature(NewNumber, ?FEATURE_E911)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec feature(knm_number:knm_number()) -> kz_json:api_json_term().
feature(Number) ->
    knm_phone_number:feature(knm_number:phone_number(Number), ?FEATURE_E911).

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
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_util:is_empty(E911) of
        'true' ->
            lager:debug("dry run: information has been removed, updating upstream"),
            knm_services:deactivate_feature(Number, ?FEATURE_E911);
        'false' when NotChanged  ->
            Number;
        'false' ->
            lager:debug("dry run: information has been changed: ~s", [kz_json:encode(E911)]),
            knm_services:activate_feature(Number, {?FEATURE_E911, E911})
    end;

maybe_update_e911(Number, 'false') ->
    CurrentE911 = feature(Number),
    E911 = kz_json:get_ne_value(?FEATURE_E911, knm_phone_number:doc(knm_number:phone_number(Number))),
    NotChanged = kz_json:are_equal(CurrentE911, E911),
    case kz_util:is_empty(E911) of
        'true' ->
            lager:debug("information has been removed, updating upstream"),
            {'ok', NewNumber} = remove_number(Number),
            knm_services:deactivate_feature(NewNumber, ?FEATURE_E911);
        'false' when NotChanged  ->
            Number;
        'false' ->
            case update_e911(Number, E911) of
                {'ok', NewNumber} ->
                    lager:debug("information has been changed: ~s", [kz_json:encode(E911)]),
                    knm_services:activate_feature(NewNumber, {?FEATURE_E911, E911});
                {'error', E} ->
                    lager:error("information update failed: ~p", [E]),
                    knm_errors:unspecified(E, Number)
            end
    end.

%% @private
-spec update_e911(knm_number:knm_number(), kz_json:object()) ->
                         {'ok', knm_number:knm_number()} |
                         {'error', ne_binary()}.
update_e911(Number, AddressJObj) ->
    remove_number_address(Number),
    case create_address(Number, AddressJObj) of
        {'error', _}=E -> E;
        {'ok', AddressId} -> assign_address(Number, AddressId)
    end.

-spec create_address(knm_number:knm_number(), kz_json:object()) ->
                            {'ok', ne_binary()} |
                            {'error', ne_binary() | any()}.
create_address(Number, AddressJObj) ->
    Body = e911_address(Number, AddressJObj),
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

-spec assign_address(knm_number:knm_number(), ne_binary() | 'null') ->
                            {'ok', knm_number:knm_number()} |
                            {'error', ne_binary()}.
assign_address(Number, AddressId) ->
    IsEnabling = is_binary(AddressId),
    Body = kz_json:from_list([{<<"e911_enabled">>, IsEnabling}
                             ,{<<"e911_address_id">>, AddressId}
                             ]),
    try knm_telnyx_util:req('put', ["numbers", knm_telnyx_util:did(Number), "e911_settings"], Body) of
        Rep ->
            case kz_json:is_true(<<"success">>, Rep, 'true') of
                'true' ->
                    _ = remove_number_address(Number),
                    {'ok', set_address_id(Number, AddressId)};
                _ ->
                    lager:error("cannot ~s e911: ~s", [toogle(IsEnabling), kz_json:encode(Rep)]),
                    {'error', reason(Rep)}
            end
    catch
        _T:E ->
            ST = erlang:get_stacktrace(),
            lager:error("~p ~p", [_T, E]),
            kz_util:log_stacktrace(ST),
            {'error', kz_util:to_binary(E)}
    end.

toogle('true') -> "enable";
toogle('false') -> "disable".

-spec set_address_id(knm_number:knm_number(), ne_binary() | 'null') -> knm_number:knm_number().
set_address_id(Number, AddressId) ->
    PN = knm_number:phone_number(Number),
    Data = kz_json:from_list([{?ADDRESS_ID, AddressId}]),
    NewPN = knm_phone_number:update_carrier_data(PN, Data),
    knm_number:set_phone_number(Number, NewPN).

-spec remove_number(knm_number:knm_number()) -> {'ok', knm_number:knm_number()} |
                                                {'error', ne_binary()}.
remove_number(Number) ->
    CarrierData = knm_phone_number:carrier_data(knm_number:phone_number(Number)),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> {'ok', Number};
        AddressId ->
            lager:debug("removing previously set address: ~p", [AddressId]),
            assign_address(Number, 'null')
    end.

-spec remove_number_address(knm_number:knm_number()) -> 'ok'.
remove_number_address(Number) ->
    CarrierData = knm_phone_number:carrier_data(knm_number:phone_number(Number)),
    case kz_json:get_ne_binary_value(?ADDRESS_ID, CarrierData) of
        'undefined' -> 'ok';
        AddrId ->
            Path = ["e911_addresses", binary_to_list(AddrId)],
            _ = kz_util:spawn(fun() -> catch knm_telnyx_util:req(delete, Path) end),
            'ok'
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
    Reason = kz_json:encode(Error),
    lager:error("~s", [Reason]),
    Reason.

-spec e911_address(knm_number:knm_number(), kz_json:object()) -> kz_json:object().
e911_address(Number, JObj) ->
    E911Name = kz_json:get_ne_binary_value(?E911_NAME, JObj),
    CallerName = knm_providers:e911_caller_name(Number, E911Name),
    kz_json:from_list(
      props:filter_empty(
        [{<<"business_name">>, CallerName}
        ,{<<"city">>, cleanse(kz_json:get_ne_binary_value(?E911_CITY, JObj))}
        ,{<<"state">>, cleanse(kz_json:get_ne_binary_value(?E911_STATE, JObj))}
        ,{<<"postal_code">>, kz_json:get_ne_binary_value(?E911_ZIP, JObj)}
        ,{<<"line_1">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET1, JObj))}
        ,{<<"line_2">>, cleanse(kz_json:get_ne_binary_value(?E911_STREET2, JObj))}
        ])).

-spec cleanse(api_ne_binary()) -> api_binary().
cleanse('undefined') -> 'undefined';
cleanse(NEBin) ->
    Upper = kz_util:to_upper_binary(NEBin),
    << <<C>> || <<C>> <= Upper, is_ALnum_or_space(C)>>.

-spec is_ALnum_or_space(char()) -> boolean().
is_ALnum_or_space(C) when $0 =< C, C =< $9 -> 'true';
is_ALnum_or_space(C) when $A =< C, C =< $Z -> 'true';
is_ALnum_or_space(C) -> $\s =:= C.
