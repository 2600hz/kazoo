%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
        ,state/1
        ,ported_in/1
        ,'cnam.inbound'/1
        ,'prepend.enabled'/1
        ,force_outbound/1
        ]).

%% Appliers
-export([list/2
        ,list_all/2
        ,find/3
        ,dump/2
        ,dump_aging/2, dump_available/2, dump_deleted/2, dump_discovery/2
        ,dump_in_service/2, dump_port_in/2, dump_port_out/2, dump_released/2, dump_reserved/2
        ,import/3
        ,assign_to/3
        ,update_merge/3, update_overwrite/3
        ,release/3
        ,reserve/3
        ,delete/3
        ]).

-include_lib("kazoo_numbers/include/knm_phone_number.hrl").
-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".numbers">>).
-define(IMPORT_DEFAULTS_TO_CARRIER
        %% Defaults to knm_carriers:default_carrier()'s default value
       ,kapps_config:get_binary(?MOD_CAT, <<"import_defaults_to_carrier">>, ?CARRIER_LOCAL)
       ).

-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).

-define(ON_SETTING_PUBLIC_FIELDS
       ,"To add a public field 'MyPub' to a number, create a column named 'opaque.MyPub'.\n"
        "Note: to nest a public field 'my_nested_field' under 'my_field', name the column thusly: 'opaque.my_field.my_nested_field'.\n"
        "Note: some fields may be disabled by configuration in which case it is forbidden to set them.\n"
       ).

-define(CATEGORY, "number_management").
-define(ACTIONS, [<<"list">>
                 ,<<"list_all">>
                 ,<<"find">>
                 ,<<"dump">>
                 ,<<"dump_aging">>
                 ,<<"dump_available">>
                 ,<<"dump_deleted">>
                 ,<<"dump_discovery">>
                 ,<<"dump_in_service">>
                 ,<<"dump_port_in">>
                 ,<<"dump_port_out">>
                 ,<<"dump_released">>
                 ,<<"dump_reserved">>
                 ,<<"import">>
                 ,<<"assign_to">>
                 ,<<"update_merge">>
                 ,<<"update_overwrite">>
                 ,<<"release">>
                 ,<<"reserve">>
                 ,<<"delete">>
                 ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(<<"tasks.help">>, ?MODULE, 'help'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".output_header">>, ?MODULE, 'output_header'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".cleanup">>, ?MODULE, 'cleanup'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".e164">>, ?MODULE, 'e164'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".account_id">>, ?MODULE, 'account_id'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".carrier_module">>, ?MODULE, 'carrier_module'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".ported_in">>, ?MODULE, 'ported_in'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(<<"list">>) ->
    list_output_header();
output_header(<<"list_all">>) ->
    list_output_header();
output_header(<<"dump">>) ->
    list_output_header();
output_header(<<"dump_", _/binary>>) ->
    list_output_header();
output_header(?NE_BINARY) ->
    result_output_header().

-spec cleanup(kz_term:ne_binary(), any()) -> any().
cleanup(<<"import">>, 'init') ->
    %% Hit iff no rows at all succeeded.
    'ok';
cleanup(<<"import">>, AccountIds) ->
    F = fun (AccountId) ->
                lager:debug("reconciling account ~s", [AccountId]),
                kz_services:reconcile(AccountId)
        end,
    lists:foreach(F, sets:to_list(AccountIds)),
    kz_datamgr:enable_change_notice();
cleanup(?NE_BINARY, _) -> 'ok'.

-spec result_output_header() -> kz_tasks:output_header().
result_output_header() ->
    {'replace', list_output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]}.

-spec list_output_header() -> kz_tasks:output_header().
list_output_header() ->
    [<<"e164">>
    ,<<"account_name">>
    ,<<"account_id">>
    ,<<"previously_assigned_to">>
    ,<<"state">>
    ,<<"created">>
    ,<<"modified">>
    ,<<"used_by">>
    ,<<"ported_in">>
    ,<<"carrier_module">>
    ,<<"cnam.inbound">>
    ,<<"cnam.outbound">>
    ,<<"e911.locality">>
    ,<<"e911.name">>
    ,<<"e911.region">>
    ,<<"e911.street_address">>
    ,<<"e911.extended_address">>
    ,<<"e911.postal_code">>
    ,<<"prepend.enabled">>
    ,<<"prepend.name">>
    ,<<"prepend.number">>
    ,<<"ringback.early">>
    ,<<"ringback.transfer">>
    ,<<"force_outbound">>
    ,<<"failover.e164">>
    ,<<"failover.sip">>
    ].

-spec list_doc() -> kz_term:ne_binary().
list_doc() ->
    <<"For each number found, returns fields:\n"
      "* `e164`: phone number represented as E164 (with a leading `+` sign).\n"
      "* `account_id`: account it is assigned to (32 alphanumeric characters).\n"
      "* `previously_assigned_to`: account it was assigned to before being assigned to `account_id`.\n"
      "* `state`: either discovery, available, reserved, in_service, released, deleted, port_in or port_out.\n"
      "* `created`: timestamp number document was created.\n"
      "* `modified`: timestamp number document was last updated.\n"
      "* `used_by`: Kazoo application handling this number.\n"
      "* `ported_in`: whether this number was imported as part of a port request ('true' of 'false').\n"
      "* `carrier_module`: service that created the number document.\n"
      "* `cnam.inbound`: whether inbound CNAM is activated ('true' or 'false').\n"
      "* `cnam.outbound`: caller ID to use on outbound calls.\n"
      "* `e911.locality`: E911 locality.\n"
      "* `e911.name`: E911 name.\n"
      "* `e911.region`: E911 region.\n"
      "* `e911.street_address`: E911 street address.\n"
      "* `e911.extended_address`: E911 street address, second field.\n"
      "* `e911.postal_code`: E911 postal code.\n"
      "* `prepend.enabled`: whether prepend is enabled ('true' or 'false').\n"
      "* `prepend.name`: prepend name.\n"
      "* `prepend.number`: prepend number.\n"
      "* `ringback.early`: ringback early.\n"
      "* `ringback.early`: ringback transfer.\n"
      "* `force_outbound`: whether this number is forced outbound ('true' or 'false').\n"
      "* `failover.e164`: number to fail over to.\n"
      "* `failover.sip`: SIP URI to fail over to.\n"
    >>.

optional_public_fields() ->
    [?FEATURE_RENAME_CARRIER]
        ++ (list_output_header() -- [<<"e164">>
                                    ,<<"account_name">>
                                    ,<<"account_id">>
                                    ,<<"previously_assigned_to">>
                                    ,<<"state">>
                                    ,<<"created">>
                                    ,<<"modified">>
                                    ,<<"used_by">>
                                    ,<<"ported_in">>
                                    ,<<"carrier_module">>
                                    ]).

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_map(action(Action)), JObj).

-spec action(kz_term:ne_binary()) -> map().
action(<<"list">>) ->
    #{<<"description">> => <<"List all numbers assigned to the account starting the task">>
     ,<<"doc">> => list_doc()
     };
action(<<"list_all">>) ->
    #{<<"description">> => <<"List all numbers assigned to the account starting the task & its subaccounts">>
     ,<<"doc">> => list_doc()
     };
action(<<"find">>) ->
    #{<<"description">> => <<"List the given numbers if the authenticated account owns them">>
     ,<<"doc">> => list_doc()
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => []
     };
action(<<"dump">>) ->
    #{<<"description">> => <<"List all numbers that exist in the system">>
     ,<<"doc">> => list_doc()
     };
action(<<"dump_", State/binary>>) ->
    #{<<"description">> => <<"List all '", State/binary, "' numbers that exist in the system">>
     ,<<"doc">> => list_doc()
     };

action(<<"import">>) ->
    #{<<"description">> => <<"Bulk-import numbers using superadmin privileges">>
     ,<<"doc">> => <<"Creates numbers from fields similar to list tasks.\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: number must not be in the system already.\n"
                     "If `account_id` is empty, number will be assigned to account creating task.\n"
                     "`module_name` will be used only if account creating task is system admin.\n"
                     "`state` will be used only if account creating task is system admin.\n"
                     "Note: create new 'available' numbers by setting their `state` to 'available' and running the task with admin credentials.\n"
                     "Note: `carrier_module` defaults to '", (?IMPORT_DEFAULTS_TO_CARRIER)/binary, "'.\n"
                     ?ON_SETTING_PUBLIC_FIELDS
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => list_output_header() -- [<<"e164">>]
     };

action(<<"assign_to">>) ->
    #{<<"description">> => <<"Bulk-assign numbers to the provided account">>
     ,<<"doc">> => <<"Assign existing numbers to another account.\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: number must already exist.\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                     "Note: target `account_id` must exist.\n"
                     "Note: after assignment, number state will be 'in_service'.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => [<<"account_id">>]
     };

action(<<"update_merge">>) ->
    #{<<"description">> => <<"Bulk-update numbers">>
     ,<<"doc">> => <<"Update features and/or public fields of existing numbers.\n"
                     "Note: if a field is already set on number but empty in the row, it will not be modified.\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: number must already exist.\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                     ?ON_SETTING_PUBLIC_FIELDS
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => optional_public_fields()
     };

action(<<"update_overwrite">>) ->
    #{<<"description">> => <<"Bulk-update numbers">>
     ,<<"doc">> => <<"Reset features and/or public fields of existing numbers.\n"
                     "Note: if a field is already set on number but empty in the row, it will be unset.\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: number must already exist.\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                     ?ON_SETTING_PUBLIC_FIELDS
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => optional_public_fields()
     };

action(<<"release">>) ->
    #{<<"description">> => <<"Unassign numbers from accounts">>
     ,<<"doc">> => <<"Release numbers (removing happens if account is configured so).\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: number must already exist.\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => []
     };

action(<<"reserve">>) ->
    #{<<"description">> => <<"Bulk-reserve numbers">>
     ,<<"doc">> => <<"Sets numbers to state 'reserved' (creating number if it is missing).\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: account creating the task (or `auth_by` account) must have permission to proceed.\n"
                     "Note: after transitioning state to 'reserved', number is assigned to `account_id`.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => [<<"account_id">>]
     };

action(<<"delete">>) ->
    #{<<"description">> => <<"Bulk-remove numbers">>
     ,<<"doc">> => <<"Forces numbers to be deleted from the system.\n"
                     "Note: number must be E164-formatted.\n"
                     "Note: number must already exist.\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on number.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => [<<"e164">>]
     ,<<"optional">> => []
     }.

%%% Verifiers

-spec e164(kz_term:ne_binary()) -> boolean().
e164(<<"+", _/binary>>) -> 'true';
e164(_) -> 'false'.

-spec account_id(kz_term:ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

-spec carrier_module(kz_term:ne_binary()) -> boolean().
carrier_module(Data) ->
    lists:member(Data, knm_carriers:all_modules()).

-spec state(kz_term:ne_binary()) -> boolean().
state(Data) ->
    knm_phone_number:is_state(Data).

-spec ported_in(kz_term:ne_binary()) -> boolean().
ported_in(Cell) -> is_cell_boolean(Cell).

-spec 'cnam.inbound'(kz_term:ne_binary()) -> boolean().
'cnam.inbound'(Cell) -> is_cell_boolean(Cell).

-spec 'prepend.enabled'(kz_term:ne_binary()) -> boolean().
'prepend.enabled'(Cell) -> is_cell_boolean(Cell).

-spec force_outbound(kz_term:ne_binary()) -> boolean().
force_outbound(Cell) -> is_cell_boolean(Cell).


-spec is_cell_boolean(kz_term:ne_binary()) -> boolean().
is_cell_boolean(<<"true">>) -> 'true';
is_cell_boolean(<<"false">>) -> 'true';
is_cell_boolean(_) -> 'false'.

-spec is_cell_true(kz_term:api_ne_binary()) -> boolean().
is_cell_true('undefined') -> 'undefined';
is_cell_true(<<"true">>) -> 'true';
is_cell_true(_) -> 'false'.

%%% Appliers

-spec list(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
list(#{'account_id' := ForAccount}, 'init') ->
    ToList = [{ForAccount, NumberDb} || NumberDb <- knm_util:get_all_number_dbs()],
    {'ok', ToList};
list(_, []) -> 'stop';
list(#{'auth_account_id' := AuthBy}, Todo) ->
    list_assigned_to(AuthBy, Todo).

-spec list_numbers(kz_term:ne_binary(), kz_term:ne_binaries()) -> [kz_csv:row()].
list_numbers(AuthBy, E164s) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    Collection = knm_numbers:get(E164s, Options),
    PNs = knm_pipe:succeeded(Collection),
    Failed = knm_pipe:failed(Collection),
    maps:fold(fun list_bad_rows/3, [], Failed)
        ++ [list_number(PN) || PN <- PNs].

list_bad_rows(E164, 'not_reconcilable', Rows) ->
    %% Numbers that shouldn't be in the system (e.g. '+141510010+14')
    %% Their fields are not queryable but we return the id to show it exists.
    Row = [E164 | lists:duplicate(length(list_output_header()) - 1, 'undefined')],
    [Row|Rows];
list_bad_rows(_E164, _R, Rows) ->
    lager:error("wild number ~s appeared: ~p", [_E164, _R]),
    Rows.

-spec list_number(knm_phone_number:record()) -> map().
list_number(PN) ->
    InboundCNAM = knm_phone_number:feature(PN, ?FEATURE_CNAM_INBOUND),
    OutboundCNAM = knm_phone_number:feature(PN, ?FEATURE_CNAM_OUTBOUND),
    E911 = knm_phone_number:feature(PN, ?FEATURE_E911),
    Prepend = knm_phone_number:feature(PN, ?FEATURE_PREPEND),
    Ringback = knm_phone_number:feature(PN, ?FEATURE_RINGBACK),
    Failover = knm_phone_number:feature(PN, ?FEATURE_FAILOVER),

    #{<<"e164">> => knm_phone_number:number(PN)
     ,<<"account_name">> => account_name(knm_phone_number:assigned_to(PN))
     ,<<"account_id">> => knm_phone_number:assigned_to(PN)
     ,<<"previously_assigned_to">> => knm_phone_number:prev_assigned_to(PN)
     ,<<"state">> => knm_phone_number:state(PN)
     ,<<"created">> => integer_to_binary(knm_phone_number:created(PN))
     ,<<"modified">> => integer_to_binary(knm_phone_number:modified(PN))
     ,<<"used_by">> => knm_phone_number:used_by(PN)
     ,<<"ported_in">> => kz_term:to_binary(knm_phone_number:ported_in(PN))
     ,<<"carrier_module">> => knm_phone_number:module_name(PN)
     ,<<"cnam.inbound">> => kz_term:to_binary(kz_json:is_true(?CNAM_INBOUND_LOOKUP, InboundCNAM))
     ,<<"cnam.outbound">> => quote(kz_json:get_ne_binary_value(?CNAM_DISPLAY_NAME, OutboundCNAM))
     ,<<"e911.locality">> => quote(kz_json:get_ne_binary_value(?E911_CITY, E911))
     ,<<"e911.name">> => quote(kz_json:get_ne_binary_value(?E911_NAME, E911))
     ,<<"e911.region">> => kz_json:get_ne_binary_value(?E911_STATE, E911)
     ,<<"e911.street_address">> => quote(kz_json:get_ne_binary_value(?E911_STREET1, E911))
     ,<<"e911.extended_address">> => quote(kz_json:get_ne_binary_value(?E911_STREET2, E911))
     ,<<"e911.postal_code">> => kz_json:get_ne_binary_value(?E911_ZIP, E911)
     ,<<"prepend.enabled">> => kz_term:to_binary(kz_json:is_true(?PREPEND_ENABLED, Prepend))
     ,<<"prepend.name">> => quote(kz_json:get_ne_binary_value(?PREPEND_NAME, Prepend))
     ,<<"prepend.number">> => kz_json:get_ne_binary_value(?PREPEND_NUMBER, Prepend)
     ,<<"ringback.early">> => kz_json:get_ne_binary_value(?RINGBACK_EARLY, Ringback)
     ,<<"ringback.transfer">> => kz_json:get_ne_binary_value(?RINGBACK_TRANSFER, Ringback)
     ,<<"force_outbound">> => kz_term:to_binary(knm_number:force_outbound_feature(PN))
     ,<<"failover.e164">> => kz_json:get_ne_binary_value(?FAILOVER_E164, Failover)
     ,<<"failover.sip">> => quote(kz_json:get_ne_binary_value(?FAILOVER_SIP, Failover))
     }.

-spec account_name(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
account_name(MaybeAccountId) ->
    case kzd_accounts:fetch_name(MaybeAccountId) of
        'undefined' -> 'undefined';
        Name -> quote(Name)
    end.

-spec quote(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
quote('undefined') -> 'undefined';
quote(Bin) -> <<$\", Bin/binary, $\">>.

-spec list_all(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
list_all(#{'account_id' := Account}, 'init') ->
    ForAccounts = [Account | kapps_util:account_descendants(Account)],
    ToList = [{ForAccount, NumberDb}
              || ForAccount <- ForAccounts,
                 NumberDb <- knm_util:get_all_number_dbs()
             ],
    {'ok', ToList};
list_all(_, []) -> 'stop';
list_all(_, Todo) ->
    list_assigned_to(?KNM_DEFAULT_AUTH_BY, Todo).

-spec find(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
find(#{'auth_account_id' := AuthBy}, _IterValue, Args=#{<<"e164">> := Num}) ->
    handle_result(Args, knm_number:get(Num, [{'auth_by', AuthBy}])).

-spec dump(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump(ExtraArgs, 'init') ->
    init_dump(ExtraArgs);
dump(_, []) -> 'stop';
dump(_, Todo) ->
    dump_next(fun db_and_view_for_dump/1, Todo).

init_dump(ExtraArgs) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    case maps:get('auth_account_id', ExtraArgs) of
        MasterAccountId -> {'ok', knm_util:get_all_number_dbs()};
        _ -> 'stop'
    end.

dump_by_state(State, Todo) ->
    ViewFun = fun (Next) -> db_and_view_for_dump_by_state(State, Next) end,
    dump_next(ViewFun, Todo).

-spec dump_aging(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_aging(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_aging(_, []) -> 'stop';
dump_aging(_, Todo) -> dump_by_state(?NUMBER_STATE_AGING, Todo).

-spec dump_available(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_available(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_available(_, []) -> 'stop';
dump_available(_, Todo) -> dump_by_state(?NUMBER_STATE_AVAILABLE, Todo).

-spec dump_deleted(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_deleted(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_deleted(_, []) -> 'stop';
dump_deleted(_, Todo) -> dump_by_state(?NUMBER_STATE_DELETED, Todo).

-spec dump_discovery(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_discovery(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_discovery(_, []) -> 'stop';
dump_discovery(_, Todo) -> dump_by_state(?NUMBER_STATE_DISCOVERY, Todo).

-spec dump_in_service(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_in_service(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_in_service(_, []) -> 'stop';
dump_in_service(_, Todo) -> dump_by_state(?NUMBER_STATE_IN_SERVICE, Todo).

-spec dump_port_in(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_port_in(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_port_in(_, []) -> 'stop';
dump_port_in(_, Todo) -> dump_by_state(?NUMBER_STATE_PORT_IN, Todo).

-spec dump_port_out(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_port_out(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_port_out(_, []) -> 'stop';
dump_port_out(_, Todo) -> dump_by_state(?NUMBER_STATE_PORT_OUT, Todo).

-spec dump_released(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_released(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_released(_, []) -> 'stop';
dump_released(_, Todo) -> dump_by_state(?NUMBER_STATE_RELEASED, Todo).

-spec dump_reserved(kz_tasks:extra_args(), kz_tasks:iterator()) -> kz_tasks:iterator().
dump_reserved(ExtraArgs, 'init') -> init_dump(ExtraArgs);
dump_reserved(_, []) -> 'stop';
dump_reserved(_, Todo) -> dump_by_state(?NUMBER_STATE_RESERVED, Todo).

-spec import(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
          {kz_tasks:return(), sets:set()}.
import(ExtraArgs, 'init', Args) ->
    kz_datamgr:suppress_change_notice(),
    IterValue = sets:new(),
    import(ExtraArgs, IterValue, Args);
import(#{'account_id' := Account
        ,'auth_account_id' := AuthAccountId
        }
      ,AccountIds
      ,Args=#{<<"e164">> := E164
             ,<<"account_id">> := AccountId0
             ,<<"state">> := State
             ,<<"carrier_module">> := Carrier
             ,<<"ported_in">> := PortedIn
             ,<<"previously_assigned_to">> := _
             ,<<"created">> := _
             ,<<"modified">> := _
             ,<<"used_by">> := _  %%FIXME: decide whether to use this one
             }
      ) ->
    AccountId = select_account_id(AccountId0, Account),
    Options = [{'auth_by', AuthAccountId}
              ,{'batch_run', 'true'}
              ,{'assign_to', AccountId}
              ,{'module_name', import_module_name(AuthAccountId, Carrier)}
              ,{'ported_in', PortedIn =:= <<"true">>}
              ,{'public_fields', public_fields(Args)}
               | import_state(AuthAccountId, State)
              ],
    Row = handle_result(Args, knm_number:create(E164, Options)),
    {Row, sets:add_element(AccountId, AccountIds)}.

public_fields(Args) -> kz_json:from_list(lists:flatten(pub_fields(Args))).
pub_fields(Args=#{<<"cnam.inbound">> := CNAMInbound
                 ,<<"cnam.outbound">> := CNAMOutbound
                 ,<<"e911.locality">> := E911Locality
                 ,<<"e911.name">> := E911Name
                 ,<<"e911.region">> := E911Region
                 ,<<"e911.street_address">> := E911StreetAddress
                 ,<<"e911.extended_address">> := E911ExtendedAddress
                 ,<<"e911.postal_code">> := E911PostalCode
                 ,<<"prepend.enabled">> := PrependEnabled
                 ,<<"prepend.name">> := PrependName
                 ,<<"prepend.number">> := PrependNumber
                 ,<<"ringback.early">> := RingbackEarly
                 ,<<"ringback.transfer">> := RingbackTransfer
                 ,?FEATURE_FORCE_OUTBOUND := ForceOutbound
                 ,<<"failover.e164">> := FailoverE164
                 ,<<"failover.sip">> := FailoverSIP
                 }) ->
    RenameCarrier = maps:get(?FEATURE_RENAME_CARRIER, Args, 'undefined'),
    [props:filter_undefined([{?FEATURE_RENAME_CARRIER, RenameCarrier}])
    ,cnam(props:filter_undefined(
            [{?CNAM_DISPLAY_NAME, CNAMOutbound}
            ,{?CNAM_INBOUND_LOOKUP, is_cell_true(CNAMInbound)}
            ]))
    ,e911(props:filter_empty(
            [{?E911_CITY, E911Locality}
            ,{?E911_NAME, E911Name}
            ,{?E911_STATE, E911Region}
            ,{?E911_STREET1, E911StreetAddress}
            ,{?E911_STREET2, E911ExtendedAddress}
            ,{?E911_ZIP, E911PostalCode}
            ]))
    ,prepend(props:filter_undefined(
               [{?PREPEND_ENABLED, is_cell_true(PrependEnabled)}
               ,{?PREPEND_NAME, PrependName}
               ,{?PREPEND_NUMBER, PrependNumber}
               ]))
    ,ringback(props:filter_empty(
                [{?RINGBACK_EARLY, RingbackEarly}
                ,{?RINGBACK_TRANSFER, RingbackTransfer}
                ]))
    ,props:filter_undefined([{?FEATURE_FORCE_OUTBOUND, is_cell_true(ForceOutbound)}])
    ,failover(props:filter_empty(
                [{?FAILOVER_E164, FailoverE164}
                ,{?FAILOVER_SIP, FailoverSIP}
                ]))
    ,additional_fields_to_json(Args)
    ].

cnam(Props) -> maybe_nest(?FEATURE_CNAM, Props).
e911(Props) -> maybe_nest(?FEATURE_E911, Props).
prepend(Props) -> maybe_nest(?FEATURE_PREPEND, Props).
ringback(Props) -> maybe_nest(?FEATURE_RINGBACK, Props).
failover(Props) -> maybe_nest(?FEATURE_FAILOVER, Props).

-spec maybe_nest(kz_term:ne_binary(), kz_term:proplist()) -> kz_term:proplist().
maybe_nest(_, []) -> [];
maybe_nest(Feature, Props) -> [{Feature, kz_json:from_list(Props)}].

import_module_name(AuthBy, Carrier) ->
    case kzd_accounts:is_superduper_admin(AuthBy)
        andalso Carrier
    of
        'false' -> ?IMPORT_DEFAULTS_TO_CARRIER;
        'undefined' -> ?IMPORT_DEFAULTS_TO_CARRIER;
        _ -> Carrier
    end.

import_state(AuthBy, State) ->
    case kzd_accounts:is_superduper_admin(AuthBy)
        andalso 'undefined' =/= State
    of
        'false' -> [];
        'true' -> [{'state', State}]
    end.

additional_fields_to_json(Args) ->
    F = fun (Field, JObj) ->
                Path = binary:split(Field, <<$.>>, ['global']),
                case maps:get(<<"opaque.", Field/binary>>, Args, 'undefined') of
                    'undefined' -> JObj;
                    Value ->
                        lager:debug("setting public field ~p to ~p", [Path, Value]),
                        kz_json:set_value(Path, Value, JObj)
                end
        end,
    kz_json:to_proplist(
      lists:foldl(F, kz_json:new(), additional_fields(Args))
     ).

additional_fields(Args) ->
    [OpaqueField
     || <<"opaque.", OpaqueField0/binary>> <- maps:keys(Args),
        OpaqueField <- [kz_binary:strip(OpaqueField0)],
        not kz_term:is_empty(OpaqueField)
    ].

select_account_id(?MATCH_ACCOUNT_RAW(_)=AccountId, _) -> AccountId;
select_account_id(_, AccountId) -> AccountId.

-spec assign_to(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
assign_to(#{'auth_account_id' := AuthBy, 'account_id' := Account}
         ,_IterValue
         ,Args=#{<<"e164">> := Num, <<"account_id">> := AccountId0}
         ) ->
    AccountId = select_account_id(AccountId0, Account),
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    handle_result(Args, knm_number:move(Num, AccountId, Options)).

-spec update_merge(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
update_merge(#{'auth_account_id' := AuthBy}
            ,_IterValue
            ,Args=#{<<"e164">> := Num}
            ) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    Updates = [{fun knm_phone_number:update_doc/2, public_fields(Args)}],
    handle_result(Args, knm_number:update(Num, Updates, Options)).

-spec update_overwrite(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
update_overwrite(#{'auth_account_id' := AuthBy}
                ,_IterValue
                ,Args=#{<<"e164">> := Num}
                ) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    Updates = [{fun knm_phone_number:reset_doc/2, public_fields(Args)}],
    handle_result(Args, knm_number:update(Num, Updates, Options)).

-spec release(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
release(#{'auth_account_id' := AuthBy}
       ,_IterValue
       ,Args=#{<<"e164">> := Num}
       ) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    handle_result(Args, knm_number:release(Num, Options)).

-spec reserve(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
reserve(#{'auth_account_id' := AuthBy, 'account_id' := Account}
       ,_IterValue
       ,Args=#{<<"e164">> := Num, <<"account_id">> := AccountId0}
       ) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ,{'assign_to', select_account_id(AccountId0, Account)}
              ],
    handle_result(Args, knm_number:reserve(Num, Options)).

-spec delete(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) -> kz_tasks:return().
delete(#{'auth_account_id' := AuthBy}
      ,_IterValue
      ,Args=#{<<"e164">> := Num}
      ) ->
    Options = [{'auth_by', AuthBy}
              ,{'batch_run', 'true'}
              ],
    handle_result(Args, knm_number:delete(Num, Options)).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_result(kz_tasks:args(), knm_number:return()) -> kz_tasks:return().
handle_result(Args, {'ok', PN}) ->
    format_result(Args, PN);
handle_result(Args, {'dry_run', _Quotes}) ->
    format_result(Args, <<"accept_charges">>);
handle_result(Args, {'error', Reason})
  when is_atom(Reason) ->
    format_result(Args, kz_term:to_binary(Reason));
handle_result(Args, {'error', KNMError}) ->
    Reason = case knm_errors:message(KNMError) of
                 'undefined' -> knm_errors:error(KNMError);
                 R -> R
             end,
    format_result(Args, kz_term:to_binary(Reason)).

-spec format_result(kz_tasks:args(), kz_term:ne_binary() | knm_phone_number:record()) -> kz_csv:mapped_row().
format_result(Args, Reason=?NE_BINARY) ->
    Args#{?OUTPUT_CSV_HEADER_ERROR => Reason};
format_result(_, PN) ->
    Map = list_number(PN),
    Map#{?OUTPUT_CSV_HEADER_ERROR => 'undefined'}.

-type accountid_or_startkey_and_numberdbs() :: [{kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:ne_binary()}].
-spec list_assigned_to(kz_term:ne_binary(), accountid_or_startkey_and_numberdbs()) ->
          {'ok' | 'error'  | [kz_csv:row()], accountid_or_startkey_and_numberdbs()}.
list_assigned_to(AuthBy, [{Next,NumberDb}|Rest]) ->
    ViewOptions = [{'limit', ?DB_DUMP_BULK_SIZE} | view_for_list_assigned(Next)],
    case kz_datamgr:get_result_keys(NumberDb, <<"numbers/assigned_to">>, ViewOptions) of
        {'ok', []} -> {'ok', Rest};
        {'error', _R} ->
            lager:error("could not get ~p's numbers in ~s: ~p", [ViewOptions, NumberDb, _R]),
            {'error', Rest};
        {'ok', Keys} ->
            Rows = list_numbers(AuthBy, [lists:last(Key) || Key <- Keys]),
            {Rows, [{lists:last(Keys),NumberDb}|Rest]}
    end.

-spec view_for_list_assigned(kz_term:ne_binary() | kz_term:ne_binaries()) -> kz_datamgr:view_options().
view_for_list_assigned(?MATCH_ACCOUNT_RAW(AccountId)) ->
    [{'startkey', [AccountId]}
    ,{'endkey', [AccountId, kz_json:new()]}
    ];
view_for_list_assigned(StartKey=[AccountId,_]) ->
    [{'startkey', StartKey}
    ,{'endkey', [AccountId, kz_json:new()]}
    ,{'skip', 1}
    ].

-type startkey_or_numberdb() :: kz_term:ne_binary() | kz_term:ne_binaries().
-spec dump_next(fun((startkey_or_numberdb()) -> {kz_term:ne_binary(), kz_datamgr:view_options()}), startkey_or_numberdb()) ->
          {'ok' | 'error' | [kz_csv:row()], [startkey_or_numberdb()]}.
dump_next(ViewFun, [Next|Rest]) ->
    {NumberDb, MoreViewOptions} = ViewFun(Next),
    ViewOptions = [{limit, ?DB_DUMP_BULK_SIZE} | MoreViewOptions],
    case kz_datamgr:get_result_keys(NumberDb, <<"numbers/status">>, ViewOptions) of
        {'ok', []} -> {'ok', Rest};
        {'error', _R} ->
            lager:error("could not get ~p from ~s: ~p", [ViewOptions, NumberDb, _R]),
            {'error', Rest};
        {'ok', Keys} ->
            Rows = list_numbers(?KNM_DEFAULT_AUTH_BY, [lists:last(Key) || Key <- Keys]),
            {Rows, [lists:last(Keys)|Rest]}
    end.

-spec db_and_view_for_dump(kz_term:ne_binary() | kz_term:ne_binaries()) -> {kz_term:ne_binary(), kz_datamgr:view_options()}.
db_and_view_for_dump(<<NumberDb/binary>>) -> {NumberDb, []};
db_and_view_for_dump(StartKey=[_, _, LastNum]) ->
    ViewOpts = [{'startkey', StartKey}
               ,{'skip', 1}
               ],
    {knm_converters:to_db(LastNum), ViewOpts}.

-spec db_and_view_for_dump_by_state(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> {kz_term:ne_binary(), kz_datamgr:view_options()}.
db_and_view_for_dump_by_state(State, <<NumberDb/binary>>) ->
    ViewOptions = [{'startkey', [State]}
                  ,{'endkey', [State, kz_json:new()]}
                  ],
    {NumberDb, ViewOptions};
db_and_view_for_dump_by_state(State, StartKey=[_ ,_, LastNum]) ->
    ViewOptions = [{'startkey', StartKey}
                  ,{'endkey', [State, kz_json:new()]}
                  ,{'skip', 1}
                  ],
    {knm_converters:to_db(LastNum), ViewOptions}.

%%% End of Module.
