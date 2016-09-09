%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kt_numbers).
%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Verifiers
-export([e164/1
        ,account_id/1
        ,carrier_module/1
        ]).

%% Appliers
-export([list/2
        ,list_all/2
        ,dump/2
        ,import/17
        ,assign_to/4
        ,release/3
        ,reserve/4
        ,delete/3
        ]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo_number_manager/include/knm_phone_number.hrl").

-define(CATEGORY, "number_management").
-define(ACTIONS, [<<"list">>
                 ,<<"list_all">>
                 ,<<"dump">>
                 ,<<"import">>
                 ,<<"assign_to">>
                 ,<<"release">>
                 ,<<"reserve">>
                 ,<<"delete">>
                 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".e164">>, ?MODULE, 'e164'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".account_id">>, ?MODULE, 'account_id'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".carrier_module">>, ?MODULE, 'carrier_module'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(ne_binary()) -> kz_csv:row().
output_header(<<"list">>) ->
    list_output_header();
output_header(<<"list_all">>) ->
    list_output_header();
output_header(<<"dump">>) ->
    list_output_header().

-spec cleanup(ne_binary(), any()) -> any().
cleanup(<<"list">>, _) ->
    knm_phone_number:push_stored();
cleanup(<<"list_all">>, _) ->
    knm_phone_number:push_stored();
cleanup(<<"dump">>, _) ->
    knm_phone_number:push_stored();
cleanup(<<"import">>, 'init') ->
    %% Hit iff no rows at all succeeded.
    'ok';
cleanup(<<"import">>, AccountIds) ->
    F = fun (AccountId) ->
                lager:debug("reconciling account ~s", [AccountId]),
                kz_services:reconcile(AccountId, <<"phone_numbers">>)
        end,
    lists:foreach(F, sets:to_list(AccountIds)),
    knm_phone_number:push_stored(),
    kz_datamgr:enable_change_notice();
cleanup(<<"assign_to">>, _) ->
    knm_phone_number:push_stored();
cleanup(<<"release">>, _) ->
    knm_phone_number:push_stored();
cleanup(<<"reserve">>, _) ->
    knm_phone_number:push_stored();
cleanup(<<"delete">>, _) ->
    knm_phone_number:push_stored().

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
      "* `cnam.inbound`: whether inbound CNAM is activated.\n"
      "* `cnam.outbound`: caller ID to use on outbound calls.\n"
      "* `e911.post_code`: E911 postal code.\n"
      "* `e911.street_address`: E911 street address.\n"
      "* `e911.extended_address`: E911 street address, second field.\n"
      "* `e911.locality`: E911 locality.\n"
      "* `e911.region`: E911 region.\n"
    >>.

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), ne_binary(), ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_list(action(Action)), JObj).

-spec action(ne_binary()) -> kz_json:object().
action(<<"list">>) ->
    [{<<"description">>, <<"List all numbers assigned to the account starting the task">>}
    ,{<<"doc">>, list_doc()}
    ];
action(<<"list_all">>) ->
    [{<<"description">>, <<"List all numbers assigned to the account starting the task & its subaccounts">>}
    ,{<<"doc">>, list_doc()}
    ];
action(<<"dump">>) ->
    [{<<"description">>, <<"List all numbers that exist in the system">>}
    ,{<<"doc">>, list_doc()}
    ];

action(<<"import">>) ->
    [{<<"description">>, <<"Bulk-import numbers using superadmin privileges">>}
    ,{<<"doc">>, <<"Creates numbers from fields similar to list tasks.\n"
                   "Note: number must be E164-formatted.\n"
                   "Note: number must not be in the system already.\n"
                   "If `account_id` is empty, number will be assigned to account creating task.\n"
                   "`module_name` will be set only if account creating task is system admin.\n"
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
    ];

action(<<"assign_to">>) ->
    [{<<"description">>, <<"Bulk-assign numbers to the provided account">>}
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
    ,{<<"optional">>, [
                      ]}
    ];

action(<<"release">>) ->
    [{<<"description">>, <<"Unassign numbers from accounts">>}
    ,{<<"doc">>, <<"Release numbers (removing happens if account is configured so).\n"
                   "Note: number must be E164-formatted.\n"
                   "Note: number must already exist.\n"
                   "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                 >>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"e164">>
                       ]}
    ,{<<"optional">>, [
                      ]}
    ];

action(<<"reserve">>) ->
    [{<<"description">>, <<"Bulk-reserve numbers">>}
    ,{<<"doc">>, <<"Sets numbers to state 'reserved' (creating number if it is missing).\n"
                   "Note: number must be E164-formatted.\n"
                   "Note: account creating the task (or `auth_by` account) must have permission to proceed.\n"
                   "Note: after transitionning state to 'reserved', number is assigned to `account_id`.\n"
                 >>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"e164">>
                       ,<<"account_id">>
                       ]}
    ,{<<"optional">>, [
                      ]}
    ];

action(<<"delete">>) ->
    [{<<"description">>, <<"Bulk-remove numbers">>}
    ,{<<"doc">>, <<"Forces numbers to be deleted from the system.\n"
                   "Note: number must be E164-formatted.\n"
                   "Note: number must already exist.\n"
                   "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                 >>}
    ,{<<"expected_content">>, <<"text/csv">>}
    ,{<<"mandatory">>, [<<"e164">>
                       ]}
    ,{<<"optional">>, [
                      ]}
    ].

%%% Verifiers

-spec e164(ne_binary()) -> boolean().
e164(<<"+", _/binary>>) -> 'true';
e164(_) -> 'false'.

-spec account_id(ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

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
    ForAccount = props:get_value('account_id', Props),
    ToList = [{ForAccount, NumberDb} || NumberDb <- knm_util:get_all_number_dbs()],
    {'ok', ToList};
list(_, []) ->
    'stop';
list(Props, [{AccountId, NumberDb} | Rest]) ->
    case number_db_listing(NumberDb, AccountId) of
        [] -> {'ok', Rest};
        E164s ->
            AuthBy = props:get_value('auth_account_id', Props),
            Rows = [list_number_row(AuthBy, E164) || E164 <- E164s],
            {Rows, Rest}
    end.

-spec list_number_row(ne_binary()) -> kz_csv:row().
-spec list_number_row(ne_binary(), ne_binary()) -> kz_csv:row().
list_number_row(E164) ->
    list_number_row(?KNM_DEFAULT_AUTH_BY, E164).
list_number_row(AuthBy, E164) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    case knm_number:get(E164, Options) of
        {'ok', KNMNumber} ->
            PhoneNumber = knm_number:phone_number(KNMNumber),
            InboundCNAM = knm_phone_number:feature(PhoneNumber, <<"inbound_cnam">>),
            OutboundCNAM = knm_phone_number:feature(PhoneNumber, <<"outbound_cnam">>),
            E911 = knm_phone_number:feature(PhoneNumber, ?DASH_KEY),
            [E164
            ,knm_phone_number:assigned_to(PhoneNumber)
            ,knm_phone_number:prev_assigned_to(PhoneNumber)
            ,knm_phone_number:state(PhoneNumber)
            ,integer_to_binary(knm_phone_number:created(PhoneNumber))
            ,integer_to_binary(knm_phone_number:modified(PhoneNumber))
            ,knm_phone_number:used_by(PhoneNumber)
            ,kz_util:to_binary(knm_phone_number:ported_in(PhoneNumber))
            ,knm_phone_number:module_name(PhoneNumber)
            ,kz_util:to_binary(kz_json:is_true(<<"inbound_lookup">>, InboundCNAM))
            ,kz_json:get_ne_binary_value(<<"display_name">>, OutboundCNAM)
            ,kz_json:get_ne_binary_value(<<"post_code">>, E911)
            ,kz_json:get_ne_binary_value(<<"street_address">>, E911)
            ,kz_json:get_ne_binary_value(<<"extended_address">>, E911)
            ,kz_json:get_ne_binary_value(<<"locality">>, E911)
            ,kz_json:get_ne_binary_value(<<"region">>, E911)
            ];
        {'error', 'not_reconcilable'} ->
            %% Numbers that shouldn't be in the system (e.g. '+141510010+14')
            %% Their fields are not queriable but we return the id to show it exists.
            [E164 | lists:duplicate(length(list_output_header()) - 1, 'undefined')]
    end.

-spec list_all(kz_proplist(), task_iterator()) -> task_iterator().
list_all(Props, 'init') ->
    Account = props:get_value('account_id', Props),
    ForAccounts = [Account | get_descendants(Account)],
    ToList = [{ForAccount, NumberDb}
              || ForAccount <- ForAccounts,
                 NumberDb <- knm_util:get_all_number_dbs()
             ],
    {'ok', ToList};
list_all(_, []) ->
    'stop';
list_all(_, [{AccountId, NumberDb} | Rest]) ->
    case number_db_listing(NumberDb, AccountId) of
        [] -> {'ok', Rest};
        E164s ->
            Rows = [list_number_row(E164) || E164 <- E164s],
            {Rows, Rest}
    end.

-spec dump(kz_proplist(), task_iterator()) -> task_iterator().
dump(Props, 'init') ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    case props:get_value('auth_account_id', Props) of
        MasterAccountId -> {'ok', knm_util:get_all_number_dbs()};
        _ -> 'stop'
    end;
dump(_, []) ->
    'stop';
dump(_, [NumberDb|NumberDbs]) ->
    case kz_datamgr:get_results(NumberDb, <<"numbers/status">>) of
        {'ok', []} -> {'ok', NumberDbs};
        {'error', _R} ->
            lager:debug("could not get numbers from ~s: ~p", [NumberDb, _R]),
            {'ok', NumberDbs};
        {'ok', JObjs} ->
            Rows = [list_number_row(kz_doc:id(JObj)) || JObj <- JObjs],
            {Rows, NumberDbs}
    end.

-spec import(kz_proplist(), task_iterator()
            ,ne_binary(), api_binary(), api_binary()
            ,api_binary(), api_binary(), api_binary(), api_binary(), api_binary()
            ,api_binary(), api_binary()
            ,api_binary(), api_binary(), api_binary(), api_binary(), api_binary()) ->
                    task_return().
import(Props, 'init', _1,_2,_3, _4,_5,_6, _7,_8,_9, _10,_11,_12, _13,_14,_15) ->
    kz_datamgr:suppress_change_notice(),
    IterValue = sets:new(),
    import(Props, IterValue, _1,_2,_3, _4,_5,_6, _7,_8,_9, _10,_11,_12, _13,_14,_15);
import(Props, AccountIds
      ,E164, AccountId0, Carrier
      ,_PortIn, _PrevAssignedTo, _Created, _Modified, _UsedBy
      ,CNAMInbound0, CNAMOutbound
      ,E911PostalCode, E911StreetAddress, E911ExtendedAddress, E911Locality, E911Region) ->
    %%TODO: use all the optional fields
    AccountId = case AccountId0 of
                    'undefined' -> props:get_value('account_id', Props);
                    _ -> AccountId0
                end,
    ModuleName = case kz_util:is_system_admin(props:get_value('account_id', Props))
                     andalso Carrier
                 of
                     'false' -> ?CARRIER_LOCAL;
                     'undefined' -> ?CARRIER_LOCAL;
                     _ -> Carrier
                 end,
    CNAMInbound = kz_util:is_true(CNAMInbound0),
    E911 = props:filter_empty(
             [{<<"post_code">>, E911PostalCode}
             ,{<<"street_address">>, E911StreetAddress}
             ,{<<"extended_address">>, E911ExtendedAddress}
             ,{<<"locality">>, E911Locality}
             ,{<<"region">>, E911Region}
             ]),
    Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
              ,{'batch_run', 'true'}
              ,{'assign_to', AccountId}
              ,{'module_name', ModuleName}
              ,{'public_fields', kz_json:from_list(cnam(CNAMInbound, CNAMOutbound) ++ e911(E911))}
              ],
    case handle_result(knm_number:create(E164, Options)) of
        [] -> {[], sets:add_element(AccountId, AccountIds)};
        E -> {E, AccountIds}
    end.

%% @private
-spec cnam(boolean(), api_binary()) -> kz_proplist().
cnam(_, 'undefined') -> [];
cnam(Inbound, CallerID=?NE_BINARY) ->
    [{<<"cnam">>, kz_json:from_list([{<<"display_name">>, CallerID}
                                    ,{<<"inbound_lookup">>, Inbound}
                                    ])
     }].

%% @private
-spec e911(kz_proplist()) -> kz_proplist().
e911([]) -> [];
e911(Props) -> [{?DASH_KEY, kz_json:from_list(Props)}].

-spec assign_to(kz_proplist(), task_iterator(), ne_binary(), ne_binary()) -> task_return().
assign_to(Props, _IterValue, Number, AccountId) ->
    Options = [{'auth_by', props:get_value('auth_account_id', Props)}
              ,{'batch_run', 'true'}
              ],
    handle_result(knm_number:move(Number, AccountId, Options)).

-spec release(kz_proplist(), task_iterator(), ne_binary()) -> task_return().
release(Props, _IterValue, Number) ->
    Options = [{'auth_by', props:get_value('auth_account_id', Props)}
              ,{'batch_run', 'true'}
              ],
    handle_result(knm_number:release(Number, Options)).

-spec reserve(kz_proplist(), task_iterator(), ne_binary(), ne_binary()) -> task_return().
reserve(Props, _IterValue, Number, AccountId) ->
    Options = [{'auth_by', props:get_value('auth_account_id', Props)}
              ,{'batch_run', 'true'}
              ,{'assign_to', AccountId}
              ],
    handle_result(knm_number:reserve(Number, Options)).

-spec delete(kz_proplist(), task_iterator(), ne_binary()) -> task_return().
delete(Props, _IterValue, Number) ->
    Options = [{'auth_by', props:get_value('auth_account_id', Props)}
              ,{'batch_run', 'true'}
              ],
    handle_result(knm_number:delete(Number, Options)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_result(knm_number_return()) -> task_return().
handle_result({'ok', _KNMNumber}) -> [];
handle_result({'dry_run', _Services, _Charges}) -> <<"accept_charges">>;
handle_result({'error', Reason})
  when is_atom(Reason) ->
    kz_util:to_binary(Reason);
handle_result({'error', KNMError}) ->
    case knm_errors:message(KNMError) of
        'undefined' -> knm_errors:error(KNMError);
        Reason -> Reason
    end.

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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% List an number db's phone numbers assigned to Account.
%% @end
%%--------------------------------------------------------------------
-spec number_db_listing(ne_binary(), ne_binary()) -> ne_binaries().
number_db_listing(NumberDb, ?MATCH_ACCOUNT_RAW(AssignedTo)) ->
    ViewOptions = [{'startkey', [AssignedTo]}
                  ,{'endkey', [AssignedTo, kz_json:new()]}
                  ],
    case kz_datamgr:get_results(NumberDb, <<"numbers/assigned_to">>, ViewOptions) of
        {'ok', []} -> [];
        {'ok', JObjs} -> [kz_doc:id(JObj) || JObj <- JObjs];
        {'error', _R} ->
            lager:debug("error listing numbers for ~s: ~p", [NumberDb, _R]),
            []
    end.

%%% End of Module.
