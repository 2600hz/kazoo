%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, Voxter
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_devices).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Verifiers
-export([verify_account_id/1
        ,verify_owner_id/1
        ]).

%% Appliers
-export([import/3
        ,delete/3
        ]).

-include("tasks.hrl").
-include_lib("kazoo_tasks/include/task_fields.hrl").

-define(MATCH_USER_ID(UserId)
       ,<<(UserId):32/binary>>
       ).

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".devices">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).


-define(CATEGORY, "devices").
-define(ACTIONS, [<<"import">>
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
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".account_id">>, ?MODULE, 'verify_account_id'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".owner_id">>, ?MODULE, 'verify_owner_id'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(?NE_BINARY) ->
    result_output_header().

-spec result_output_header() -> kz_tasks:output_header().
result_output_header() ->
    {'replace', output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]}.

-spec output_header() -> kz_tasks:output_header().
output_header() ->
    [<<"id">>
    ,<<"name">>
    ,<<"owner_id">>
    ,<<"sip_username">>
    ,<<"account_id">>
    ].

-spec mandatory_import_fields() -> kz_type:proplist().
mandatory_import_fields() ->
    [<<"name">>
    ,<<"owner_id">>
    ,<<"sip_username">>
    ,<<"sip_password">>
    ,<<"account_id">>
    ].
-spec optional_import_fields() -> kz_type:proplist().
optional_import_fields() ->
    [].

-spec mandatory_delete_fields() -> kz_type:proplist().
mandatory_delete_fields() ->
    [<<"account_id">>
    ,<<"device_id">>
    ].
-spec optional_delete_fields() -> kz_type:proplist().
optional_delete_fields() -> [].

-spec help(kz_json:object()) -> kz_json:object().
help(JObj) -> help(JObj, <<?CATEGORY>>).

-spec help(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category) ->
    lists:foldl(fun(Action, J) -> help(J, Category, Action) end, JObj, ?ACTIONS).

-spec help(kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
help(JObj, <<?CATEGORY>>=Category, Action) ->
    kz_json:set_value([Category, Action], kz_json:from_map(action(Action)), JObj).

%%%=============================================================================
%%% Actions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec action(kz_term:ne_binary()) -> map().
action(<<"import">>) ->
    #{<<"description">> => <<"Bulk-import of devices">>
     ,<<"doc">> => <<"Bulk create devices for the defined account id.\n"
                     "For each device created, return fields:\n"
                     "* `id`: doc id it is assigned to (32 alphanumeric characters).\n"
                     "* `name`: The devices name as supplied\n"
                     "* `owner_id`: The devices owner id\n"
                     "* `sip_username`: The devices sip username as supplied\n"
                     "* `account_id`: account it is assigned to (32 alphanumeric characters).\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => mandatory_import_fields()
     ,<<"optional">> => optional_import_fields()
     ,<<"return_headers">> => output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]
     };

action(<<"delete">>) ->
    #{<<"description">> => <<"Bulk-remove devices">>
     ,<<"doc">> => <<"Forces devices to be deleted from an account\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => mandatory_delete_fields()
     ,<<"optional">> => optional_delete_fields()
     ,<<"return_headers">> => output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]
     }.

%%%=============================================================================
%%% Verifiers
%%% These are called during CSV first upload on all rows to verify the CSV data
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_account_id(kz_term:ne_binary()) -> boolean().
verify_account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
verify_account_id(_) -> 'false'.

-spec verify_owner_id(kz_term:ne_binary()) -> boolean().
verify_owner_id(?MATCH_USER_ID(_)) -> 'true';
verify_owner_id(_) -> 'false'.

%%%=============================================================================
%%% Appliers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Handle an import action
%% @end
%%------------------------------------------------------------------------------
-spec import(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
import(ExtraArgs, 'init', Args) ->
    IterValue = sets:new(),
    import(ExtraArgs, IterValue, Args);
import(#{account_id := Account
        ,auth_account_id := AuthAccountId
        }
      ,AccountIds
      ,Args=#{<<"account_id">> := AccountId0}
      ) ->
    AccountId = select_account_id(AccountId0, Account),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
               'true' ->
                   validate_and_save_device(AccountId, Args);
               'false' ->
                   {'error', <<"Access denied, Auth Account does not have access to account">>}
           end,
    Row = handle_result(Args, Resp, 'import'),
    {Row, sets:add_element(AccountId, AccountIds)}.

-spec delete(kz_tasks:extra_args(), kz_tasks:iterator(), kz_tasks:args()) ->
                    {kz_tasks:return(), sets:set()}.
delete(ExtraArgs, 'init', Args) ->
    IterValue = sets:new(),
    delete(ExtraArgs, IterValue, Args);
delete(#{account_id := Account
        ,auth_account_id := AuthAccountId
        }
      ,AccountIds
      ,Args=#{<<"account_id">> := AccountId0}
      ) ->
    AccountId = select_account_id(AccountId0, Account),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
               'true' ->
                   delete_device(AccountId, Args);
               'false' ->
                   {'error', <<"Access denied, Auth Account does not have access to account">>}
           end,
    Row = handle_result(Args, Resp, 'delete'),
    {Row, sets:add_element(AccountId, AccountIds)}.


%%%=============================================================================
%%% Return value functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process the CSV row result
%% @end
%%------------------------------------------------------------------------------
-spec handle_result(kz_tasks:args(), {'ok', kz_doc:object()} | {'error', atom() | kz_type:ne_binary()}, atom()) -> kz_tasks:return().
handle_result(Args, {'ok', Doc}, Action) ->
    format_result(Args, Doc, Action);
handle_result(Args, {'error', Reason}, Action)
  when is_atom(Reason) ->
    format_result(Args, kz_term:to_binary(Reason), Action);
handle_result(Args, {'error', Reason}, Action) ->
    format_result(Args, Reason, Action).

%%------------------------------------------------------------------------------
%% @doc Format the row response to be returned to the device in the CSV
%% @end
%%------------------------------------------------------------------------------
-spec format_result(kz_tasks:args(), kz_term:ne_binary() | knm_number:knm_number(), atom()) -> kz_csv:mapped_row().
format_result(Args, Reason=?NE_BINARY, Action) ->
    Map = generate_return_values_from_args(Args, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => Reason};
format_result(_Args, Doc, Action) ->
    Map = generate_return_values_from_doc(Doc, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => 'undefined'}.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the supplied csv inputs  (Error case)
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_args(map(), atom()) -> map().
generate_return_values_from_args(Args ,'import') ->
    #{<<"id">> => 'undefined'
     ,<<"name">> => maps:get(<<"name">>, Args)
     ,<<"owner_id">> => maps:get(<<"owner_id">>, Args)
     ,<<"sip_username">> => maps:get(<<"sip_username">>, Args)
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     };
generate_return_values_from_args(Args ,'delete') ->
    #{<<"id">> => maps:get(<<"device_id">>, Args)
     ,<<"name">> => 'undefined'
     ,<<"owner_id">> => 'undefined'
     ,<<"sip_username">> => 'undefined'
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     }.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the device doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_doc(kz_doc:object(), atom()) -> map().
generate_return_values_from_doc(Doc, 'import') ->
    lager:debug("generating import resp row from doc: ~p", [Doc]),
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"name">> => kzd_devices:name(Doc)
     ,<<"owner_id">> => kzd_devices:owner_id(Doc)
     ,<<"sip_username">> => kzd_devices:sip_username(Doc)
     ,<<"account_id">> => kz_doc:account_id(Doc)
     };
generate_return_values_from_doc(Doc, 'delete') ->
    lager:debug("generating delete resp row from doc: ~p", [Doc]),
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"name">> => 'undefined'
     ,<<"owner_id">> => 'undefined'
     ,<<"sip_username">> => 'undefined'
     ,<<"account_id">> => kz_doc:account_id(Doc)
     }.


%%%=============================================================================
%%% Cleanup functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup(kz_term:ne_binary(), any()) -> any().
cleanup(<<"import">>, 'init') ->
    %% Hit if no rows at all succeeded.
    'ok';
%%cleanup(<<"import">>, AccountIds) ->
%%    F = fun (AccountId) ->
%%                lager:debug("reconciling account ~s", [AccountId]),
%%                kz_services:reconcile(AccountId, <<"user">>)
%%        end,
%%    lists:foreach(F, sets:to_list(AccountIds)),
%%    kz_datamgr:enable_change_notice();
cleanup(?NE_BINARY, _) -> 'ok'.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns true if Account1 is authorised to make changes to Account2
%% @end
%%------------------------------------------------------------------------------
-spec is_authorized_account(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_authorized_account(Account1, Account1) -> true;
is_authorized_account(Account1, Account2) -> is_parent_account(Account1, Account2).

%%------------------------------------------------------------------------------
%% @doc Returns true if Account1 is a parent account of Account2
%% @end
%%------------------------------------------------------------------------------
-spec is_parent_account(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_parent_account(<<_/binary>> = Account1, Account2) ->
    {'ok', JObj} = kzd_accounts:fetch(Account2),
    lists:member(Account1, kzd_accounts:tree(JObj)).

%%------------------------------------------------------------------------------
%% @doc Return the first account id if its in the coret format, else return the second
%% @end
%%------------------------------------------------------------------------------
-spec select_account_id(kz_type:ne_binary(), kz_type:ne_binary()) -> kz_type:ne_binary().
select_account_id(?MATCH_ACCOUNT_RAW(_)=AccountId, _) -> AccountId;
select_account_id(_, AccountId) -> AccountId.

%%------------------------------------------------------------------------------
%% @doc Verify the device passes validation and save the device to kazoo
%% @end
%%------------------------------------------------------------------------------
-spec validate_and_save_device(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
validate_and_save_device(AccountId, Args) ->
    case validate_device(AccountId, Args) of
        {'error', Cause} = Error ->
            lager:error("device failed validation: ~p", [Cause]),
            Error;
        'ok' ->
            prepare_and_save_device(AccountId, Args)
    end.

%%------------------------------------------------------------------------------
%% @doc Build User doc and save device to kazoo storage
%% @end
%%------------------------------------------------------------------------------
-spec prepare_and_save_device(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
prepare_and_save_device(AccountId ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Doc = generate_device_doc(AccountId, Args),
    case kz_datamgr:save_doc(AccountDb, Doc) of
        {'error', Reason} ->
            lager:error("failed to save doc, Reason: ~p, Doc: ~p", [Reason, Doc]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _} = Ok ->
            Ok
    end.


%%------------------------------------------------------------------------------
%% @doc Validate the CSV args are valid to create a Device
%% @end
%%------------------------------------------------------------------------------
-spec validate_device(kz_type:ne_binary(), kz_tasks:args()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_device(AccountId, #{<<"sip_username">> := SipUsername}) ->
    AccountRealm = kzd_accounts:fetch_realm(AccountId),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    case  is_sip_creds_unique(AccountDb, AccountRealm, SipUsername) of
        'true' ->
            check_username_length(AccountId, SipUsername);
        'false' -> {'error', <<"sip username is not unique in realm ", AccountRealm/binary>>}
    end.


%%------------------------------------------------------------------------------
%% @doc Check if the device sip creds are unique
%% @end
%%------------------------------------------------------------------------------
-spec is_sip_creds_unique(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_sip_creds_unique(AccountDb, Realm, Username) ->
    is_creds_locally_unique(AccountDb, Username)
        andalso is_creds_global_unique(Realm, Username).

-spec is_creds_locally_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_creds_locally_unique(AccountDb, Username) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Username)}],
    case kz_datamgr:get_results(AccountDb, <<"devices/sip_credentials">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [_JObj]} -> 'false';
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.

-spec is_creds_global_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_creds_global_unique(Realm, Username) ->
    ViewOptions = [{'key', [kz_term:to_lower_binary(Realm)
                           ,kz_term:to_lower_binary(Username)
                           ]
                   }],
    case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [_JObj]} -> 'false';
        {'error', 'not_found'} -> 'true';
        _ -> 'false'
    end.



%%------------------------------------------------------------------------------
%% @doc Build Device doc from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec generate_device_doc(kz_type:ne_binary(), kz_tasks:args()) -> kz_doc:object().
generate_device_doc(AccountId, Args) ->
    Media = kz_json:from_list_recursive([{<<"audio">>,[{<<"codecs">>, [<<"G722">>, <<"G729">>, <<"PCMU">>]}]}]),
    Fax = kz_json:from_list([{<<"option">>, <<"auto">>}]),
    Encryption = kz_json:from_list([{<<"enforce_security">>, <<"false">>},{<<"methods">>, []}]),
    Video = kz_json:from_list([{<<"codecs">>, []}]),
    DocFuns = [{fun kzd_devices:set_name/2, maps:get(<<"name">>, Args)}
              ,{fun kzd_devices:set_owner_id/2, maps:get(<<"owner_id">>, Args)}
              ,{fun kzd_devices:set_sip_username/2, maps:get(<<"sip_username">>, Args)}
              ,{fun kzd_devices:set_sip_password/2, maps:get(<<"sip_password">>, Args)}
              ,{fun kzd_devices:set_sip_invite_format/2, <<"username">>}
              ,{fun kzd_devices:set_sip_expire_seconds/2, 360}
              ,{fun kzd_devices:set_sip_method/2, <<"password">>}
              ,{fun kzd_devices:set_call_forward_require_keypress/2, 'false'}
              ,{fun kzd_devices:set_call_forward_substitute/2, 'false'}
              ,{fun kzd_devices:set_call_forward_direct_calls_only/2, 'false'}
              ,{fun kzd_devices:set_call_forward_enabled/2, 'false'}
              ,{fun kzd_devices:set_call_forward_failover/2, 'false'}
              ,{fun kzd_devices:set_call_forward_ignore_early_media/2, 'true'}
              ,{fun kzd_devices:set_call_forward_keep_caller_id/2, 'true'}
              ,{fun kzd_devices:set_media/2, Media}
              ,{fun kzd_devices:set_call_restriction/2, kz_json:new()}
              ,{fun kzd_devices:set_contact_list/2, kz_json:new()}
              ,{fun kzd_devices:set_enabled/2, 'true'}
              ,{fun kzd_devices:set_exclude_from_queues/2, 'false'}
              ,{fun kzd_devices:set_music_on_hold/2, kz_json:new()}
              ,{fun kzd_devices:set_mwi_unsolicited_updates/2, 'true'}
              ,{fun kzd_devices:set_register_overwrite_notify/2, 'false'}
              ,{fun kzd_devices:set_ringtones/2, kz_json:new()}
              ,{fun kzd_devices:set_suppress_unregister_notifications/2, 'false'}
              ],
    PubDoc = lists:foldl(fun({Fun, Arg}, Doc) -> Fun(Doc, Arg) end, kzd_devices:new(), DocFuns),

    %% Values not exposed in kzd_devices
    OtherPubValues = [{<<"bypass_media">>, <<"auto">>}
                     ,{<<"fax">>, Fax}
                     ,{<<"encryption">>, Encryption}
                     ,{<<"video">>, Video}
                     ],
    UpdatedPubDoc = kz_json:set_values(OtherPubValues, PubDoc),
    add_private_fields(AccountId, UpdatedPubDoc, Args).

%%------------------------------------------------------------------------------
%% @doc Add private doc fields from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec add_private_fields(kt_task:args(), kz_doc:object(), kz_type:ne_binary()) -> kz_doc:object().
add_private_fields(AccountId ,Doc ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    AlphaNumName = normalize_alphanum_name(maps:get(<<"name">>, Args)),
    PvtOptions = [{'type', kzd_devices:type()}
                 ,{'account_id', AccountId}
                 ,{'account_db', AccountDb}
                 ,{'crossbar_doc_vsn', <<"1">>}
                 ],
    UpdatedDoc = kz_doc:update_pvt_parameters(Doc, AccountDb, PvtOptions),
    OtherPvtValues = [{<<"pvt_alphanum_name">>, AlphaNumName}],
    kz_json:set_values(OtherPvtValues, UpdatedDoc).



%%------------------------------------------------------------------------------
%% @doc Validate the sip_username meets min lenght if defined
%% @end
%%------------------------------------------------------------------------------
-spec check_username_length(kz_type:ne_binary(), kz_type:ne_binary()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_username_length(AccountId, Username) ->
    CheckMinLength = kapps_config:get_is_true(<<"device">>, <<"enforce_min_length">>, 'false'),
    check_username_length(AccountId, Username, CheckMinLength).

-spec check_username_length(kz_type:ne_binary(), kz_type:ne_binary(), boolean()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_username_length(_AccountId, _Username, 'false') -> 'ok';
check_username_length(AccountId, Username, 'true') ->
    MinLength = kapps_account_config:get_global(AccountId, <<"device">>, <<"min_device_length">>, 3),
    case byte_size(Username) of
        N when N < MinLength ->
            {'error', <<"Username is not long enough, Min length required: ", MinLength/binary>>};
        _Else ->
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a device from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec delete_device(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
delete_device(AccountId ,#{<<"device_id">> := DeviceId}) ->
    kz_tasks_utils:delete_doc(AccountId, DeviceId, <<"device">>).
%%------------------------------------------------------------------------------
%% @doc Generate alphanum from the device name
%% @end
%%------------------------------------------------------------------------------
-spec normalize_alphanum_name(kz_type:ne_binary()) -> kz_type:ne_binary().
normalize_alphanum_name(<<"undefined">>) ->
    <<"undefined">>;
normalize_alphanum_name(Name) ->
    re:replace(kz_term:to_lower_binary(Name), <<"[^a-z0-9]">>, <<>>, [global, {return, binary}]).

%%% End of Module.
