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
-export([e164/1
        ,account_id/1
        ,auth_by/1
        ,carrier_module/1
        ]).

%% Appliers
-export([list/2
        ,list_all/2
        ,import/16
        ,assign_to/4
        ,release/3
        ,reserve/4
        ,delete/3
        ]).

-include("knm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec category() -> ne_binary().
category() -> <<"number_management">>.

-spec module() -> ne_binary().
module() -> kz_util:to_binary(?MODULE).

-spec output_header(atom()) -> kz_csv:row().
output_header('list') ->
    list_output_header();
output_header('list_all') ->
    list_output_header().

-spec list_output_header() -> kz_csv:row().
list_output_header() ->
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

-spec list_doc() -> ne_binary().
list_doc() ->
    <<"For each number found, returns fields:\n"
      "* `e164`: phone number represented as E164 (with a leading `+` sign).\n"
      "* `account_id`: account it is assigned to (32 alphanumeric characters).\n"
      "* `previously_assigned_to`: account it was assigned to before being assigned to `account_id`.\n"
      "* `state`: either discovery, available, reserved, in_service, released, disconnected, deleted, port_in or port_out.\n"
      "* `created`: timestamp number document was created.\n"
      "* `modified`: timestamp number document was last updated.\n"
      "* `used_by`: Kazoo application handling this number.\n"
      "* `port_in`: whether this number is linked to an ongoing port request.\n"
      "* `carrier_module`: service that created the number document.\n"
      "* `cnam.inbound`: \n"
      "* `cnam.outbound`: \n"
      "* `e911.post_code`: \n"
      "* `e911.street_address`: \n"
      "* `e911.extended_address`: \n"
      "* `e911.locality`: \n"
      "* `e911.region`: \n"
    >>.

-spec help() -> kz_proplist().
help() ->
    [{<<"list">>
     ,kz_json:from_list([{<<"description">>, <<"List all numbers under the account starting the task">>}
                        ,{<<"doc">>, list_doc()}
                        ])
     }

    ,{<<"list_all">>
     ,kz_json:from_list([{<<"description">>, <<"List all numbers that exist in the system">>}
                        ,{<<"doc">>, list_doc()}
                        ])
     }

    ,{<<"import">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-import numbers">>}
                        ,{<<"doc">>, <<"Creates numbers using superadmin privileges.\n"
                                       "Note: number must be E164-formatted.\n"
                                       "Note: number must not be in the system already.\n"
                                       "If `account_id` is empty, number state will be 'available'.\n"
                                       "Otherwise, the number will be assigned to `account_id` with state 'in_service'.\n"
                                       "Note: `carrier_module` defaults to 'knm_local'.\n"
                                     >>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"e164">>
                                           ]}
                        ,{<<"optional">>, [<<"account_id">>
                                          ,<<"carrier_module">>
                                          ,<<"port_in">>
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
                        ,{<<"doc">>, <<"Assign existing numbers to another account.\n"
                                       "Note: number must be E164-formatted.\n"
                                       "Note: number must already exist.\n"
                                       "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                                       "Note: target `account_id` must exist.\n"
                                       "Note: after assignment, number state will be 'in_service'.\n"
                                     >>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"e164">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"release">>
     ,kz_json:from_list([{<<"description">>, <<"Unassign numbers from accounts">>}
                        ,{<<"doc">>, <<"Release numbers (removing happens if account is configured so).\n"
                                       "Note: number must be E164-formatted.\n"
                                       "Note: number must already exist.\n"
                                       "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                                     >>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"e164">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"reserve">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-reserve numbers">>}
                        ,{<<"doc">>, <<"Sets numbers to state 'reserved' (creating number if it is missing).\n"
                                       "Note: number must be E164-formatted.\n"
                                       "Note: account creating the task (or `auth_by` account) must have permission to proceed.\n"
                                       "Note: after transitionning state to 'reserved', number is assigned to `account_id`.\n"
                                     >>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"e164">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"delete">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-remove numbers">>}
                        ,{<<"doc">>, <<"Forces numbers to be deleted from the system.\n"
                                       "Note: number must be E164-formatted.\n"
                                       "Note: number must already exist.\n"
                                       "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                                     >>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"e164">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }
    ].

%%% Verifiers

-spec e164(ne_binary()) -> boolean().
e164(<<"+", _/binary>>) -> 'true';
e164(_) -> 'false'.

-spec account_id(ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

-spec auth_by(api_binary()) -> boolean().
auth_by('undefined') -> 'true';
auth_by(?MATCH_ACCOUNT_RAW(_)) -> 'true';
auth_by(_) -> 'false'.

-spec carrier_module(ne_binary()) -> boolean().
carrier_module(<<"knm_bandwidth2">>) -> 'true';
carrier_module(<<"knm_bandwidth">>) -> 'true';
carrier_module(<<"knm_inum">>) -> 'true';
carrier_module(<<"knm_local">>) -> 'true';
carrier_module(<<"knm_managed">>) -> 'true';
carrier_module(<<"knm_other">>) -> 'true';
carrier_module(<<"knm_reserved">>) -> 'true';
carrier_module(<<"knm_reserved_reseller">>) -> 'true';
carrier_module(<<"knm_simwood">>) -> 'true';
carrier_module(<<"knm_vitelity">>) -> 'true';
carrier_module(<<"knm_voip_innovations">>) -> 'true';
carrier_module(_) -> 'false'.


%%% Appliers

-spec list(kz_proplist(), task_iterator()) -> task_iterator().
list(Props, 'init') ->
    ForAccount = auth_by('undefined', Props),
    Subs  = [ForAccount | get_descendants(ForAccount)],
    lager:debug(">>>~p ~p", [length(Subs), Subs]),
    {'ok', Subs};
list(_, []) ->
    lager:debug(">>>"),
    'stop';
list(_, [?MATCH_ACCOUNT_RAW(AccountId) | Rest]) ->
    lager:debug(">>> ~p", [AccountId]),
    AccountDb = kz_util:format_account_db(AccountId),
    {'ok', knm_numbers:account_listing(AccountDb) ++ Rest};
list(Props, [{E164,JObj} | Rest]) ->
    lager:debug(">>> ~p", [E164]),
    AuthBy = auth_by('undefined', Props),
    Row = list_number_row(AuthBy, E164, JObj),
    {Row, Rest}.

-spec list_number_row(ne_binary(), ne_binary(), kz_json:object()) -> kz_csv:row().
list_number_row(AuthBy, E164, JObj) ->
    Options = [{'auth_by', AuthBy}
              ],
    case knm_number:get(E164, Options) of
        {'ok', KNMNumber} ->
            PhoneNumber = knm_number:phone_number(KNMNumber),
            [E164
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
            ];
        {'error', 'not_reconcilable'} ->
            %% Numbers that shouldn't be in the system (e.g. '+141510010+14')
            %% Their fields are not queriable but we return the id to show it exists.
            [E164 | lists:duplicate(length(list_output_header()) - 1, 'undefined')]
    end.

-spec list_all(kz_proplist(), task_iterator()) -> task_iterator().
list_all(_, 'init') ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    {'ok', [MasterAccountId | get_descendants(MasterAccountId)]};
list_all(_, []) ->
    'stop';
list_all(_, [?MATCH_ACCOUNT_RAW(AccountId) | Rest]) ->
    AccountDb = kz_util:format_account_db(AccountId),
    {'ok', knm_numbers:account_listing(AccountDb) ++ Rest};
list_all(_, [{E164,JObj} | Rest]) ->
    Row = list_number_row(?KNM_DEFAULT_AUTH_BY, E164, JObj),
    {Row, Rest}.

-spec import(kz_proplist(), ne_binary(), api_binary(), api_binary()
            ,api_binary(), api_binary(), api_binary(), api_binary(), api_binary()
            ,api_binary(), api_binary()
            ,api_binary(), api_binary(), api_binary(), api_binary(), api_binary()) ->
                    task_return().
import(_Props, E164, AccountId, Carrier
      ,_PortIn, _PrevAssignedTo, _Created, _Modified, _UsedBy
      ,_CNAMInbound, _CNAMOutbound
      ,_E911PostalCode, _E911StreetAddress, _E911ExtendedAddress, _E911Locality, _E911Region) ->
    %%TODO: use all the optional fields
    State = case AccountId of
                'undefined' -> ?NUMBER_STATE_AVAILABLE;
                _ -> ?NUMBER_STATE_IN_SERVICE
            end,
    ModuleName = case Carrier of
                     'undefined' -> ?CARRIER_LOCAL;
                     _ -> Carrier
                 end,
    Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
              ,{'batch_run', 'true'}
              ,{'assign_to', AccountId}
              ,{'state', State}
              ,{'module_name', ModuleName}
              ],
    handle_result(knm_number:create(E164, Options)).

-spec assign_to(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> task_return().
assign_to(Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ,{'batch_run', 'true'}
              ],
    handle_result(knm_number:move(Number, AccountId, Options)).

-spec release(kz_proplist(), ne_binary(), api_binary()) -> task_return().
release(Props, Number, AuthBy) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ,{'batch_run', 'true'}
              ],
    handle_result(knm_number:release(Number, Options)).

-spec reserve(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> task_return().
reserve(Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', auth_by(AuthBy, Props)}
              ,{'batch_run', 'true'}
              ,{'assign_to', AccountId}
              ],
    handle_result(knm_number:reserve(Number, Options)).

-spec delete(kz_proplist(), ne_binary(), api_binary()) -> task_return().
delete(Props, Number, AuthBy) ->
    AuthAccountId = auth_by(AuthBy, Props),
    case kz_util:is_system_admin(AuthAccountId) of
        'false' -> <<"not a system admin">>;
        'true' ->
            Options = [{'auth_by', AuthAccountId}
                      ,{'batch_run', 'true'}
                      ],
            handle_result(knm_number:delete(Number, Options))
    end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% List an account's descendants. Does not include the given AccountId.
%% @end
%%--------------------------------------------------------------------
-spec get_descendants(ne_binary()) -> ne_binaries().
get_descendants(?MATCH_ACCOUNT_RAW(AccountId)) ->
    View = <<"accounts/listing_by_descendants">>,
    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, View, ViewOptions) of
        {'ok', JObjs} -> [kz_account:id(JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("unable to get descendants of ~s: ~p", [AccountId, _R]),
            []
    end.

%%% End of Module.
