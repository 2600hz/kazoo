%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2018, Voxter
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_vmboxes).

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

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".vmboxes">>).
-define(DB_DUMP_BULK_SIZE
       ,kapps_config:get_integer(?MOD_CAT, <<"db_page_size">>, 1000)
       ).


-define(CATEGORY, "vmboxes").
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
    ,<<"mailbox">>
    ,<<"name">>
    ,<<"owner_id">>
    ,<<"account_id">>
    ].

-spec mandatory_import_fields() -> kz_type:proplist().
mandatory_import_fields() ->
    [<<"mailbox">>
    ,<<"name">>
    ,<<"pin">>
    ,<<"owner_id">>
    ,<<"account_id">>
    ].
-spec optional_import_fields() -> kz_type:proplist().
optional_import_fields() ->
    [<<"timezone">>
    ,<<"numbers">>
    ,<<"transcribe">>
    ,<<"require_pin">>
    ].

-spec mandatory_delete_fields() -> kz_type:proplist().
mandatory_delete_fields() ->
    [<<"account_id">>
    ,<<"vmbox_id">>
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
    #{<<"description">> => <<"Bulk-import of Voicemail Boxes">>
     ,<<"doc">> => <<"Bulk create Voicemail Boxes for the defined account id.\n"
                     "For each voicemail box created, return fields:\n"
                     "* `id`: doc id it is assigned to (32 alphanumeric characters).\n"
                     "* `mailbox`: The voicemail box number as supplied\n"
                     "* `name`: The voicemail box name as supplied\n"
                     "* `owner_id`: The voicemail box owner id\n"
                     "* `account_id`: account it is assigned to (32 alphanumeric characters).\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => <<"text/csv">>
     ,<<"mandatory">> => mandatory_import_fields()
     ,<<"optional">> => optional_import_fields()
     ,<<"return_headers">> => output_header() ++ [?OUTPUT_CSV_HEADER_ERROR]
     };

action(<<"delete">>) ->
    #{<<"description">> => <<"Bulk-remove voicemail boxes">>
     ,<<"doc">> => <<"Forces voicemail boxes to be deleted from an account\n"
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
                   validate_and_save_vmbox(AccountId, Args);
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
                   delete_vmbox(AccountId, Args);
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
%% @doc Format the row response to be returned to the vmbox in the CSV
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
     ,<<"mailbox">> => maps:get(<<"mailbox">>, Args)
     ,<<"name">> => maps:get(<<"name">>, Args)
     ,<<"owner_id">> => maps:get(<<"owner_id">>, Args)
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     };
generate_return_values_from_args(Args ,'delete') ->
    #{<<"id">> => maps:get(<<"vmbox_id">>, Args)
     ,<<"mailbox">> => 'undefined'
     ,<<"name">> => 'undefined'
     ,<<"owner_id">> => 'undefined'
     ,<<"account_id">> => maps:get(<<"account_id">>, Args)
     }.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the vmbox doc
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_doc(kz_doc:object(), atom()) -> map().
generate_return_values_from_doc(Doc, 'import') ->
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"mailbox">> => kzd_vmboxes:mailbox(Doc)
     ,<<"name">> => kzd_vmboxes:name(Doc)
     ,<<"owner_id">> => kzd_vmboxes:owner_id(Doc)
     ,<<"account_id">> => kz_doc:account_id(Doc)
     };
generate_return_values_from_doc(Doc, 'delete') ->
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"mailbox">> => 'undefined'
     ,<<"name">> => 'undefined'
     ,<<"owner_id">> => 'undefined'
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
%% @doc Verify the vmbox passes validation and save the vmbox to kazoo
%% @end
%%------------------------------------------------------------------------------
-spec validate_and_save_vmbox(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
validate_and_save_vmbox(AccountId, Args) ->
    case validate_vmbox(AccountId, Args) of
        {'error', Cause} = Error ->
            lager:error("vmbox failed validation: ~p", [Cause]),
            Error;
        'ok' ->
            prepare_and_save_vmbox(AccountId, Args)
    end.

%%------------------------------------------------------------------------------
%% @doc Build VMBox doc and save vmbox to kazoo storage
%% @end
%%------------------------------------------------------------------------------
-spec prepare_and_save_vmbox(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
prepare_and_save_vmbox(AccountId ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Doc = generate_vmbox_doc(AccountId, Args),
    case kz_datamgr:save_doc(AccountDb, Doc) of
        {'error', Reason} ->
            lager:error("failed to save doc, Reason: ~p, Doc: ~p", [Reason, Doc]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _} = Ok ->
            Ok
    end.


%%------------------------------------------------------------------------------
%% @doc Validate the CSV args are valid to create a vmbox
%% @end
%%------------------------------------------------------------------------------
-spec validate_vmbox(kz_term:ne_binary(), kz_type:ne_binary()) -> 'ok' | {'error', kz_type:ne_binary()}.
validate_vmbox(AccountId, #{<<"mailbox">> := MailboxNumber}) ->
    case is_mailbox_number_unique(AccountId, MailboxNumber) of
        'true' ->
            check_mailbox_number_length(AccountId, MailboxNumber);
        'false' ->
            {'error', <<"mailbox number is not unique">>}
    end.


%%------------------------------------------------------------------------------
%% @doc Check if the vmbox mailbox number is unique
%% @end
%%------------------------------------------------------------------------------
-spec is_mailbox_number_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_mailbox_number_unique(AccountId, MailboxNumber) ->
    try kz_term:to_integer(MailboxNumber) of
        BoxNum ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            ViewOptions = [{'key', BoxNum}],
            case kz_datamgr:get_results(AccountDb, <<"vmboxes/listing_by_mailbox">>, ViewOptions) of
                {'ok', []} -> 'true';
                {'error', 'not_found'} -> 'true';
                {'ok', [_JObj]} -> 'false';
                _ -> 'false'
            end
    catch
        _:_ -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Build VMBox doc from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec generate_vmbox_doc(kz_type:ne_binary(), kz_tasks:args()) -> kz_doc:object().
generate_vmbox_doc(AccountId, Args) ->
    DocFuns = [{fun kzd_vmboxes:set_name/2, maps:get(<<"name">>, Args, 'undefined')}
              ,{fun kzd_vmboxes:set_mailbox/2, maps:get(<<"mailbox">>, Args, 'undefined')}
              ,{fun kzd_vmboxes:set_owner_id/2, maps:get(<<"owner_id">>, Args, 'undefined')}
              ,{fun kzd_vmboxes:set_timezone/2, maps:get(<<"timezone">>, Args, 'undefined')}
              ,{fun kzd_vmboxes:set_pin/2, maps:get(<<"pin">>, Args, 'undefined')}
              ,{fun kzd_vmboxes:set_require_pin/2, maps:get(<<"require_pin">>, Args, 'false')}
              ,{fun kzd_vmboxes:set_transcribe/2, maps:get(<<"transcribe">>, Args, 'false')}
              ,{fun kzd_vmboxes:set_check_if_owner/2, 'true'}
              ,{fun kzd_vmboxes:set_delete_after_notify/2, 'false'}
              ,{fun kzd_vmboxes:set_is_setup/2, 'false'}
              ,{fun kzd_vmboxes:set_media/2, kz_json:new()}
              ,{fun kzd_vmboxes:set_media_extension/2, <<"mp3">>}
              ,{fun kzd_vmboxes:set_not_configurable/2, 'false'}
              ,{fun kzd_vmboxes:set_notify_email_addresses/2, []}
              ,{fun kzd_vmboxes:set_save_after_notify/2, 'false'}
              ,{fun kzd_vmboxes:set_skip_envelope/2, 'false'}
              ,{fun kzd_vmboxes:set_skip_greeting/2, 'false'}
              ,{fun kzd_vmboxes:set_skip_instructions/2, 'false'}
              ],
    PubDoc = lists:foldl(fun({_, 'undefined'}, Doc) -> Doc;
                            ({Fun, Arg}, Doc) -> Fun(Doc, Arg)
                         end
                        ,kzd_vmboxes:new()
                        ,DocFuns),
    add_private_fields(AccountId, PubDoc, Args).

%%------------------------------------------------------------------------------
%% @doc Add private doc fields from CSV args
%% @end
%%------------------------------------------------------------------------------
-spec add_private_fields(kt_task:args(), kz_doc:object(), kz_type:ne_binary()) -> kz_doc:object().
add_private_fields(AccountId ,Doc ,Args) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    AlphaNumName = normalize_alphanum_name(maps:get(<<"name">>, Args)),
    PvtOptions = [{'type', <<"vmbox">>}
                 ,{'account_id', AccountId}
                 ,{'account_db', AccountDb}
                 ,{'crossbar_doc_vsn', <<"1">>}
                 ],
    UpdatedDoc = kz_doc:update_pvt_parameters(Doc, AccountDb, PvtOptions),
    OtherPvtValues = [{<<"pvt_alphanum_name">>, AlphaNumName}],
    kz_json:set_values(OtherPvtValues, UpdatedDoc).

%%------------------------------------------------------------------------------
%% @doc Validate the mailbox number meets min length if defined
%% @end
%%------------------------------------------------------------------------------
-spec check_mailbox_number_length(kz_type:ne_binary(), kz_type:ne_binary()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_mailbox_number_length(AccountId, MailboxNumber) ->
    CheckMinLength = kapps_config:get_is_true(<<"voicemail">>, <<"enforce_min_length">>, 'false'),
    check_mailbox_number_length(AccountId, MailboxNumber, CheckMinLength).

-spec check_mailbox_number_length(kz_type:ne_binary(), kz_type:ne_binary(), boolean()) -> 'ok' | {'error', kz_type:ne_binary()}.
check_mailbox_number_length(_AccountId, _MailboxNumber, 'false') -> 'ok';
check_mailbox_number_length(AccountId, MailboxNumber, 'true') ->
    MinLength = kapps_account_config:get_global(AccountId, <<"voicemail">>, <<"min_vmbox_length">>, 3),
    case byte_size(MailboxNumber) of
        N when N < MinLength ->
            {'error', <<"mailbox number is not long enough, Min length required: ", MinLength/binary>>};
        _Else ->
            'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a vmbox from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec delete_vmbox(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kz_doc:object()}  | {'error', kz_type:ne_binary()}.
delete_vmbox(AccountId ,#{<<"vmbox_id">> := VMBoxId}) ->
    kz_tasks_utils:delete_doc(AccountId, VMBoxId, <<"vmbox">>).
%%------------------------------------------------------------------------------
%% @doc Generate alphanum from the vmbox name
%% @end
%%------------------------------------------------------------------------------
-spec normalize_alphanum_name(kz_type:ne_binary()) -> kz_type:ne_binary().
normalize_alphanum_name(<<"undefined">>) ->
    <<"undefined">>;
normalize_alphanum_name(Name) ->
    re:replace(kz_term:to_lower_binary(Name), <<"[^a-z0-9]">>, <<>>, [global, {return, binary}]).

%%% End of Module.
