%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-, Voxter
%%% @doc
%%% @author Ben Bradford
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_users).

%% behaviour: tasks_provider

-export([init/0
        ,help/1, help/2, help/3
        ,output_header/1
        ,cleanup/2
        ]).

%% Verifiers
-export([verify_account_id/1
        ,verify_user_id/1
        ,verify_email/1
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

-define(CATEGORY, "users").
-define(ACTIONS, [<<"import">>
                 ,<<"delete">>
                 ]).

-define(ERROR_SEPARATOR, ",").

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
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".email">>, ?MODULE, 'verify_email'),
    _ = tasks_bindings:bind(<<"tasks."?CATEGORY".user_id">>, ?MODULE, 'verify_user_id'),
    tasks_bindings:bind_actions(<<"tasks."?CATEGORY>>, ?MODULE, ?ACTIONS).

%%------------------------------------------------------------------------------
%% @doc Required tasks function.
%% For a given `Action' return a list of the output CSV headers.
%% @end
%%------------------------------------------------------------------------------
-spec output_header(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header(Action=?NE_BINARY) ->
    {'replace', output_header_by_action(Action)}.

%%------------------------------------------------------------------------------
%% @doc Define the response headers for each action.
%% @end
%%------------------------------------------------------------------------------
-spec output_header_by_action(kz_term:ne_binary()) -> kz_tasks:output_header().
output_header_by_action(<<"import">>) ->
    [<<"id">>
    ,<<"username">>
    ,<<"first_name">>
    ,<<"last_name">>
    ,<<"account_id">>
    ,?OUTPUT_CSV_HEADER_ERROR
    ];
output_header_by_action(<<"delete">>) ->
    [<<"id">>
    ,<<"account_id">>
    ,?OUTPUT_CSV_HEADER_ERROR
    ].

%%------------------------------------------------------------------------------
%% @doc The mandatory fields required for import action.
%% Mandatory fields are pulled from doc schema.
%% @end
%%------------------------------------------------------------------------------
-spec mandatory_import_fields() -> kz_type:proplist().
mandatory_import_fields() ->
    {ok, UserSchema} = kz_json_schema:load(<<"users">>),
    kz_json:get_list_value(<<"required">>, UserSchema).

%%------------------------------------------------------------------------------
%% @doc The optional fields that can be set for import action.
%% @end
%%------------------------------------------------------------------------------
-spec optional_import_fields() -> kz_type:proplist().
optional_import_fields() ->
    AllKeys = maps:keys(kzd_users:get_setters()),
    AllKeys -- mandatory_import_fields().

%%------------------------------------------------------------------------------
%% @doc The mandatory fields required for delete action.
%% @end
%%------------------------------------------------------------------------------
-spec mandatory_delete_fields() -> kz_type:proplist().
mandatory_delete_fields() ->
    [<<"account_id">>
    ,<<"user_id">>
    ].

%%------------------------------------------------------------------------------
%% @doc The optional fields required for delete action.
%% @end
%%------------------------------------------------------------------------------
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
    #{<<"description">> => <<"Bulk-import of users">>
     ,<<"doc">> => <<"Bulk create a user for the defined account id.\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => [<<"application/json">>, <<"text/csv">>]
     ,<<"mandatory">> => mandatory_import_fields()
     ,<<"optional">> => optional_import_fields()
     ,<<"return_headers">> => output_header_by_action(<<"import">>)
     };

action(<<"delete">>) ->
    #{<<"description">> => <<"Bulk-remove users">>
     ,<<"doc">> => <<"Forces users to be deleted from an account\n"
                     "Note: account creating the task (or `auth_by` account) must have permissions on the affecting account.\n"
                   >>
     ,<<"expected_content">> => [<<"application/json">>, <<"text/csv">>]
     ,<<"mandatory">> => mandatory_delete_fields()
     ,<<"optional">> => optional_delete_fields()
     ,<<"return_headers">> => output_header_by_action(<<"delete">>)
     }.

%%%=============================================================================
%%% Verifiers
%%% Called during CSV first upload on all rows to verify the CSV data, not the
%%% generated user doc.
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Verify the account id is a valid format.
%% @end
%%------------------------------------------------------------------------------
-spec verify_account_id(kz_term:ne_binary()) -> boolean().
verify_account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
verify_account_id(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Verify the user id is a valid format.
%% @end
%%------------------------------------------------------------------------------
-spec verify_user_id(kz_term:ne_binary()) -> boolean().
verify_user_id(?MATCH_USER_ID(_)) -> 'true';
verify_user_id(_) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Basic check to verify the email is a valid format.
%% @end
%%------------------------------------------------------------------------------
-spec verify_email(kz_term:ne_binary()) -> boolean().
verify_email(Cell) ->
    case binary:split(Cell, <<"@">>) of
        [_User, _Domain] -> 'true';
        _ -> 'false'
    end.

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
import(#{account_id := URLAccountId
        ,auth_account_id := AuthAccountId
        }
      ,AccountIds
      ,Args
      ) ->
    AccountId = maps:get(<<"account_id">>, Args, URLAccountId),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
               'true' ->
                   generate_validate_and_save_new_user(AccountId, Args);
               'false' ->
                   {'error', <<"Access denied, auth account does not have access to account">>}
           end,
    Row = handle_result(Args, Resp, 'import'),
    {Row, sets:add_element(AccountId, AccountIds)}.

%%------------------------------------------------------------------------------
%% @doc Handle a delete action
%% @end
%%------------------------------------------------------------------------------
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
                   delete_user(AccountId, Args);
               'false' ->
                   {'error', <<"Access denied, Auth Account does not have access to account">>}
           end,
    Row = handle_result(Args, Resp, 'delete'),
    {Row, sets:add_element(AccountId, AccountIds)}.

%%%=============================================================================
%%% Return value functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process response and format the doc or errors into the CSV row result.
%% @end
%%------------------------------------------------------------------------------
-spec handle_result(kz_tasks:args(), {'ok', kzd_users:doc()} | {'error', kz_type:ne_binary()}, atom()) -> kz_tasks:return().
handle_result(_Args, {'ok', Doc}, Action) ->
    format_ok_result_to_csv_row(Doc, Action);
handle_result(Args, {'error', Reason}, Action) ->
    format_error_result_to_csv_row(Args, Reason, Action).

%%------------------------------------------------------------------------------
%% @doc Format a successfull (ok) result into a csv row response.
%% @end
%%------------------------------------------------------------------------------
-spec format_ok_result_to_csv_row(kzd_users:doc(), atom()) -> kz_csv:mapped_row().
format_ok_result_to_csv_row(Doc, Action) ->
    Map = generate_return_values_from_doc(Doc, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => 'undefined'}.

%%------------------------------------------------------------------------------
%% @doc Format the error result into a csv row response.
%% @end
%%------------------------------------------------------------------------------
-spec format_error_result_to_csv_row(kz_tasks:args(), kz_term:ne_binary(),  atom()) -> kz_csv:mapped_row().
format_error_result_to_csv_row(Args, Error, Action) ->
    Map = generate_return_values_from_args(Args, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => Error}.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the supplied csv inputs
%% (Error case)
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_args(map(), atom()) -> map().
generate_return_values_from_args(Args ,'import') ->
    #{<<"id">> => 'undefined'
     ,<<"username">> => maps:get(<<"username">>, Args, 'undefined')
     ,<<"first_name">> => maps:get(<<"first_name">>, Args, 'undefined')
     ,<<"last_name">> => maps:get(<<"last_name">>, Args, 'undefined')
     ,<<"account_id">> => maps:get(<<"account_id">>, Args, 'undefined')
     };
generate_return_values_from_args(Args ,'delete') ->
    #{<<"id">> => maps:get(<<"user_id">>, Args, 'undefined')
     ,<<"account_id">> => maps:get(<<"account_id">>, Args, 'undefined')
     }.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the user doc.
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_doc(kzd_users:doc(), atom()) -> map().
generate_return_values_from_doc(Doc, 'import') ->
    lager:debug("generating import resp row from doc: ~p", [Doc]),
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"username">> => kzd_users:username(Doc)
     ,<<"first_name">> => kzd_users:first_name(Doc)
     ,<<"last_name">> => kzd_users:last_name(Doc)
     ,<<"account_id">> => kz_doc:account_id(Doc)
     };
generate_return_values_from_doc(Doc, 'delete') ->
    lager:debug("generating delete resp row from doc: ~p", [Doc]),
    #{<<"id">> => kz_doc:id(Doc)
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
%% @doc Generate a new user from `Args' and verify is passes validation.
%% If validation passes then save the user.
%% @end
%%------------------------------------------------------------------------------
-spec generate_validate_and_save_new_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kzd_users:doc()}  | {'error', kz_type:ne_binary()}.
generate_validate_and_save_new_user(AccountId, Args) ->
    UserDoc = generate_new_user_doc(Args),
    case kzd_users:validate(AccountId, 'undefined', UserDoc) of
        {'true', UpdatedUserDoc} ->
            lager:debug("successfully validated new user object"),
            save_user(AccountId, UpdatedUserDoc);
        {'validation_errors', ValidationErrors} ->
            lager:info("validation errors on new user"),
            {'error', merge_validation_errors(ValidationErrors)};
        {'system_error', Error} ->
            lager:info("system error validating user: ~p", [Error]),
            {'error', Error}
    end.

%%------------------------------------------------------------------------------
%% @doc Save user to kazoo.
%% @end
%%------------------------------------------------------------------------------
-spec save_user(kz_type:ne_binary(), kzd_users:doc()) -> {'ok', kzd_users:doc()}  | {'error', kz_type:ne_binary()}.
save_user(AccountId, UserDoc) ->
    AccountDb = kzs_util:format_account_id(AccountId, 'encoded'),
    case kz_datamgr:save_doc(AccountDb, UserDoc) of
        {'error', Reason} ->
            lager:error("failed to save user, Reason: ~p, Doc: ~p", [Reason, UserDoc]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _User} = Ok ->
            lager:debug("saved user ~p", [kz_doc:id(_User)]),
            Ok
    end.

%%------------------------------------------------------------------------------
%% @doc Convert multiple `kazoo_documents:doc_validation_error()' into a binary
%% string, Each error separated by `?ERROR_SEPARATOR'.
%% @end
%%------------------------------------------------------------------------------
-spec merge_validation_errors(kazoo_documents:doc_validation_errors()) -> kz_term:ne_binary().
merge_validation_errors(ValidationErrors) ->
    ErrorBinaries = lists:map(fun(ValidationError) -> validation_error_to_binary(ValidationError) end
                             ,ValidationErrors),
    kz_term:to_binary(lists:join(?ERROR_SEPARATOR, ErrorBinaries)).

%%------------------------------------------------------------------------------
%% @doc Convert `kazoo_documents:doc_validation_error()' into a binary string.
%% @end
%%------------------------------------------------------------------------------
-spec validation_error_to_binary(kazoo_documents:doc_validation_error()) -> kz_term:ne_binary().
validation_error_to_binary({Path, ErrorCode, JObj}) ->
    PathBin = kz_term:to_binary(lists:join(".", Path)),
    ErrorCodeBin = kz_term:to_binary(ErrorCode),
    MessageBin = kz_json:get_binary_value(<<"message">>, JObj),
    <<"Validation error '", ErrorCodeBin/binary, "' on field '", PathBin/binary, "' : ", MessageBin/binary>>.

%%------------------------------------------------------------------------------
%% @doc Build user doc from default values and set / overwrite values defined
%% in the input Args (CSV row)
%% @end
%%------------------------------------------------------------------------------
-spec generate_new_user_doc(kz_tasks:args()) -> kzd_users:doc().
generate_new_user_doc(Args) ->
    KeyToSetFunMap = kzd_users:get_setters(),
    SetterFun = fun(_Key, 'undefined', JObjAcc) -> JObjAcc;
                   (Key, Value, JObjAcc) ->
                        case maps:get(Key, KeyToSetFunMap, 'undefined') of
                            'undefined' ->
                                JObjAcc;
                            SetterFun ->
                                erlang:apply('kzd_users', SetterFun, [JObjAcc, Value])
                        end
                end,
    maps:fold(SetterFun, kzd_users:new(), Args).

%%------------------------------------------------------------------------------
%% @doc Delete a user from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec delete_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kzd_users:doc()}  | {'error', kz_type:ne_binary()}.
delete_user(AccountId ,#{<<"user_id">> := UserId}) ->
    kz_tasks_utils:delete_doc(AccountId, UserId, <<"user">>).
