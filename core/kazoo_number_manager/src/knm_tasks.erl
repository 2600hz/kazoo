%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_tasks).
%% behaviour: tasks_provider

-export([help/0
        ,category/0
        ,module/0
        ,output_header/1
        ]).

%% Verifiers
-export([number/1, e164/1
        ,account_id/1
        ,auth_by/1
        ,module_name/1, carrier_module/1
        ,state/1
        ]).

%% Appliers
-export([list/2
        ,import_list/17
        ,assign_to/4
        ,delete/3
        ,reserve/4
        ,add/5
        %,update_features/5
        ]).

-include("knm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec category() -> ne_binary().
category() -> <<"number_management">>.

-spec module() -> module().
module() -> kz_util:to_binary(?MODULE).

-spec output_header(atom()) -> kz_csv:row().
output_header('list') ->
    [<<"e164">>
    ,<<"account_id">>
    ,<<"previously_assigned_to">>
    ,<<"state">>
    ,<<"created">>
    ,<<"modified">>
    ,<<"used_by">>
    ,<<"port_in">>
    ,<<"carrier_module">>
    ,<<"cnam.inbound">>
    ,<<"cnam.outbound">>
    ,<<"e911.postal_code">>
    ,<<"e911.street_address">>
    ,<<"e911.extended_address">>
    ,<<"e911.locality">>
    ,<<"e911.region">>
    ].

-spec help() -> kz_proplist().
help() ->
    [{<<"list">>
     ,kz_json:from_list([{<<"description">>, <<"List all numbers in the system">>}
                        ])
     }

    ,{<<"import_list">>
     ,kz_json:from_list([{<<"description">>, <<"Import numbers that were previously listed">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"e164">>
                                           ,<<"account_id">>
                                           ,<<"state">>
                                           ,<<"carrier_module">>
                                           ]}
                        ,{<<"optional">>, [<<"port_in">>
                                          ,<<"previously_assigned_to">>
                                          ,<<"created">>
                                          ,<<"modified">>
                                          ,<<"used_by">>
                                          ,<<"cnam.inbound">>
                                          ,<<"cnam.outbound">>
                                          ,<<"e911.postal_code">>
                                          ,<<"e911.street_address">>
                                          ,<<"e911.extended_address">>
                                          ,<<"e911.locality">>
                                          ,<<"e911.region">>
                                          ]}
                        ])
     }

    ,{<<"assign_to">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-assign numbers to the provided account">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"delete">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-remove numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"reserve">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-move numbers to reserved (adding if missing)">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"add">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-create numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ,<<"module_name">>
                                          ]}
                        ])
     }

    %% ,{<<"update_features">>
    %%  ,kz_json:from_list([{<<"description">>, <<"Bulk-update features of numbers">>}
    %%                     ,{<<"expected_content">>, <<"text/csv">>}
    %%                     ,{<<"mandatory">>, [<<"number">>
    %%                                        ]}
    %%                     ,{<<"optional">>, [<<"cnam.inbound">>
    %%                                       ,<<"cnam.outbound">>
    %%                                       ,<<"e911.street_address">>
    %%                                            %%TODO: exhaustive list
    %%                                       ]}
    %%                     ])
    %%  }
    ].

%%% Verifiers

-spec number(ne_binary()) -> boolean().
number(<<"+", _/binary>>) -> 'true';
number(_) -> 'false'.

-spec e164(ne_binary()) -> boolean().
e164(Thing) -> number(Thing).

-spec account_id(ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

-spec auth_by(api_binary()) -> boolean().
auth_by('undefined') -> 'true';
auth_by(?MATCH_ACCOUNT_RAW(_)) -> 'true';
auth_by(_) -> 'false'.

-spec module_name(api_binary()) -> boolean().
module_name('undefined') -> 'true';
module_name(Thing) -> carrier_module(Thing).

-spec carrier_module(ne_binary()) -> boolean().
carrier_module(<<"knm_bandwidth2">>) -> 'true';
carrier_module(<<"knm_bandwidth">>) -> 'true';
carrier_module(<<"knm_carriers">>) -> 'true';
carrier_module(<<"knm_inum">>) -> 'true';
carrier_module(<<"knm_local">>) -> 'true';
carrier_module(<<"knm_managed">>) -> 'true';
carrier_module(<<"knm_other">>) -> 'true';
carrier_module(<<"knm_simwood">>) -> 'true';
carrier_module(<<"knm_vitelity">>) -> 'true';
carrier_module(<<"knm_voip_innovations">>) -> 'true';
carrier_module(_) -> 'false'.

-spec state(ne_binary()) -> boolean().
state(<<"port_in">>) -> 'true';
state(<<"port_out">>) -> 'true';
state(<<"discovery">>) -> 'true';
state(<<"in_service">>) -> 'true';
state(<<"released">>) -> 'true';
state(<<"reserved">>) -> 'true';
state(<<"available">>) -> 'true';
state(<<"disconnected">>) -> 'true';
state(<<"deleted">>) -> 'true';
state(_) -> 'false'.


%%% Appliers

-spec list(kz_proplist(), task_iterator()) -> task_iterator().
list(Props, 'init') ->
    ForAccount = kz_util:format_account_db(props:get_value('auth_account_id', Props)),
    {'ok', knm_numbers:account_listing(ForAccount)};
list(_, []) ->
    'stop';
list(Props, [{E164,JObj} | E164s]) ->
    Options = [{'auth_by', auth_by('undefined', Props)}
              ],
    {'ok', KNMNumber} = knm_number:get(E164, Options),
    PhoneNumber = knm_number:phone_number(KNMNumber),
    Row = [E164
          ,knm_phone_number:assigned_to(PhoneNumber)
          ,knm_phone_number:prev_assigned_to(PhoneNumber)
          ,knm_phone_number:state(PhoneNumber)
          ,integer_to_binary(kz_json:get_value(<<"created">>, JObj))
          ,integer_to_binary(kz_json:get_value(<<"updated">>, JObj))
          ,knm_phone_number:used_by(PhoneNumber)
          ,kz_util:to_binary(knm_phone_number:ported_in(PhoneNumber))
          ,knm_phone_number:module_name(PhoneNumber)
          ,'undefined'%%TODO
          ,'undefined'%%TODO
          ,'undefined'%%TODO
          ,'undefined'%%TODO
          ,'undefined'%%TODO
          ,'undefined'%%TODO
          ,'undefined'%%TODO
          ],
    {Row, E164s}.

-spec import_list(kz_proplist(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()
                 ,api_binary(), api_binary(), api_binary(), api_binary(), api_binary(), api_binary()
                 ,api_binary()
                 ,api_binary(), api_binary(), api_binary(), api_binary()) ->
                         task_return().
import_list(Props, E164, AccountId, State, Carrier
           ,_PortIn, _PrevAssignedTo, _Created, _Modified, _UsedBy, _CNAMInbound, _CNAMOutbound
           ,_E911PostalCode
           ,_E911StreetAddress, _E911ExtendedAddress, _E911Locality, _E911Region) ->
    %%TODO: use the optional fields
    Options = [{'auth_by', props:get_value('auth_account_id', Props)}
              ,{'assign_to', AccountId}
              ,{'state', State}
              ,{'module_name', Carrier}
              ],
    handle_result(knm_number:create(E164, Options)).

-spec assign_to(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> task_return().
assign_to(Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ],
    handle_result(knm_number:move(Number, AccountId, Options)).

-spec delete(kz_proplist(), ne_binary(), api_binary()) -> task_return().
delete(Props, Number, AuthBy) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ],
    handle_result(knm_number:release(Number, Options)).

-spec reserve(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> task_return().
reserve(Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ,{'assign_to', AccountId}
              ],
    handle_result(knm_number:reserve(Number, Options)).

-spec add(kz_proplist(), ne_binary(), ne_binary(), api_binary(), api_binary()) -> task_return().
add(Props, Number, AccountId, AuthBy, ModuleName0) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ,{'assign_to', AccountId}
              ,{'module_name', ModuleName0}
              ],
    handle_result(knm_number:create(Number, Options)).

%% -spec update_features(kz_proplist(), ne_binary(), api_binary(), ...) -> any().
%% update_features(Props, Number, CNAMInbound0, ...) ->


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_result(knm_number_return()) -> task_return().
handle_result({'ok', _KNMNumber}) -> 'ok';
handle_result({'dry_run', _Services, _Charges}) -> <<"accept_charges">>;
handle_result({'error', Reason})
  when is_atom(Reason) ->
    kz_util:to_binary(Reason);
handle_result({'error', KNMError}) ->
    case knm_errors:message(KNMError) of
        'undefined' -> knm_errors:error(KNMError);
        Reason -> Reason
    end.

-spec auth_by(api_binary(), kz_proplist()) -> api_binary().
auth_by('undefined', Props) -> props:get_value('auth_account_id', Props);
auth_by(AuthBy, _) -> AuthBy.

%%% End of Module.
