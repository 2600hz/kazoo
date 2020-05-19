%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-, Voxter
%%% @doc
%%% @author Ben Bradford
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
%% Optional fields are pulled from the kzd_users setter functions.
%% `account_id' is added to allow users to be created in an account different
%% from the one defined in the url (this valve will overwirte the url value).
%% @end
%%------------------------------------------------------------------------------
-spec optional_import_fields() -> kz_type:proplist().
optional_import_fields() ->
    AllKeys = maps:keys(kzd_users:get_setters()),
    (AllKeys ++ [<<"account_id">>]) -- mandatory_import_fields().

%%------------------------------------------------------------------------------
%% @doc The mandatory fields required for delete action.
%% @end
%%------------------------------------------------------------------------------
-spec mandatory_delete_fields() -> kz_type:proplist().
mandatory_delete_fields() ->
    [<<"user_id">>].

%%------------------------------------------------------------------------------
%% @doc The optional fields required for delete action.
%% `account_id' is added to allow users to be deleted from an account different
%% from the one defined in the url (this valve will overwirte the url value).
%% @end
%%------------------------------------------------------------------------------
-spec optional_delete_fields() -> kz_type:proplist().
optional_delete_fields() ->
    [<<"account_id">>].

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
                     "`account_id` field is added to allow you to overwite, per row (if set), the account id defined in the request url"
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
                     "`account_id` field is added to allow you to overwite, per row (if set), the account id defined in the request url"
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
    AccountId = maybe_override_account_id(URLAccountId, Args),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
               'true' ->
                   generate_validate_and_save_new_user(AccountId, Args);
               'false' ->
                   lager:error("failed to create user, auth account does not have access to account '~p'", [AccountId]),
                   {'error', <<"Access denied, auth account does not have access to account '~p'">>, [AccountId]}
           end,
    Row = handle_result(AccountId, Args, Resp, 'import'),
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
delete(#{account_id := URLAccountId
        ,auth_account_id := AuthAccountId
        }
      ,AccountIds
      ,Args
      ) ->
    AccountId = maybe_override_account_id(URLAccountId, Args),
    Resp = case is_authorized_account(AuthAccountId, AccountId) of
               'true' ->
                   delete_user(AccountId, Args);
               'false' ->
                   lager:error("failed to delete user, auth account does not have access to account '~p'", [AccountId]),
                   {'error', <<"Access denied, auth account does not have access to account '~p'">>, [AccountId]}
           end,
    Row = handle_result(AccountId, Args, Resp, 'delete'),
    {Row, sets:add_element(AccountId, AccountIds)}.

%%%=============================================================================
%%% Return value functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process response and format the doc or errors into the CSV row result.
%% @end
%%------------------------------------------------------------------------------
-spec handle_result(kz_term:ne_binary(), kz_tasks:args(), {'ok', kzd_users:doc()} | {'error', kz_type:ne_binary()}, atom()) ->
          kz_tasks:return().
handle_result(AccountId, _Args, {'ok', Doc}, Action) ->
    format_ok_result_to_csv_row(AccountId, Doc, Action);
handle_result(AccountId, Args, {'error', Reason}, Action) ->
    format_error_result_to_csv_row(AccountId, Args, Reason, Action).

%%------------------------------------------------------------------------------
%% @doc Format a successfull (ok) result into a csv row response.
%% @end
%%------------------------------------------------------------------------------
-spec format_ok_result_to_csv_row(kz_term:ne_binary(), kzd_users:doc(), atom()) -> kz_csv:mapped_row().
format_ok_result_to_csv_row(AccountId, Doc, Action) ->
    Map = generate_return_values_from_doc(AccountId, Doc, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => 'undefined'}.

%%------------------------------------------------------------------------------
%% @doc Format the error result into a csv row response.
%% @end
%%------------------------------------------------------------------------------
-spec format_error_result_to_csv_row(kz_term:ne_binary(), kz_tasks:args(), kz_term:ne_binary(), atom()) -> kz_csv:mapped_row().
format_error_result_to_csv_row(AccountId, Args, Error, Action) ->
    Map = generate_return_values_from_args(AccountId, Args, Action),
    Map#{?OUTPUT_CSV_HEADER_ERROR => Error}.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the supplied csv inputs.
%% (Error case)
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_args(kz_term:ne_binary(), kz_tasks:args(), atom()) -> kz_csv:mapped_row().
generate_return_values_from_args(AccountId, Args ,'import') ->
    #{<<"id">> => 'undefined'
     ,<<"username">> => maps:get(<<"username">>, Args, 'undefined')
     ,<<"first_name">> => maps:get(<<"first_name">>, Args, 'undefined')
     ,<<"last_name">> => maps:get(<<"last_name">>, Args, 'undefined')
     ,<<"account_id">> => AccountId
     };
generate_return_values_from_args(AccountId, Args ,'delete') ->
    #{<<"id">> => maps:get(<<"user_id">>, Args, 'undefined')
     ,<<"account_id">> => AccountId
     }.

%%------------------------------------------------------------------------------
%% @doc Generate the csv return row values from the user doc.
%% @end
%%------------------------------------------------------------------------------
-spec generate_return_values_from_doc(kz_term:ne_binary(), kzd_users:doc(), atom()) -> kz_csv:mapped_row().
generate_return_values_from_doc(AccountId, Doc, 'import') ->
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"username">> => kzd_users:username(Doc)
     ,<<"first_name">> => kzd_users:first_name(Doc)
     ,<<"last_name">> => kzd_users:last_name(Doc)
     ,<<"account_id">> => kz_doc:account_id(Doc, AccountId)
     };
generate_return_values_from_doc(AccountId, Doc, 'delete') ->
    #{<<"id">> => kz_doc:id(Doc)
     ,<<"account_id">> => kz_doc:account_id(Doc, AccountId)
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
%% @doc Maybe override the account id defined in the url with one defined in the
%% row args.
%% If the `account_id' key in the map is `undefined' then use the `URLAccountId'
%% else use the value defined in the args map.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_override_account_id(kz_term:ne_binary(), kz_tasks:args()) -> kz_term:ne_binary().
maybe_override_account_id(URLAccountId, #{<<"account_id">> := 'undefined'}) -> URLAccountId;
maybe_override_account_id(_URLAccountId, #{<<"account_id">> := AccountIdOverride}) -> AccountIdOverride.

%%------------------------------------------------------------------------------
%% @doc Generate a new user from `Args' and verify is passes validation.
%% If validation passes then save the user.
%% @end
%%------------------------------------------------------------------------------
-spec generate_validate_and_save_new_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kzd_users:doc()}  | {'error', kz_type:ne_binary()}.
generate_validate_and_save_new_user(AccountId, Args) ->
    UserDoc = kz_tasks_utils:generate_new_doc_with_kazoo_doc_setters('kzd_users', Args),
    case kzd_users:validate(AccountId, 'undefined', UserDoc) of
        {'true', UpdatedUserDoc} ->
            lager:debug("successfully validated new user object"),
            save_user(AccountId, UpdatedUserDoc);
        {'validation_errors', ValidationErrors} ->
            lager:info("validation errors on new user"),
            {'error', kz_tasks_utils:merge_kazoo_doc_validation_errors(ValidationErrors)};
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
    AccountDb = kzs_util:format_account_db(AccountId),
    case kz_datamgr:save_doc(AccountDb, UserDoc) of
        {'error', Reason} ->
            lager:error("failed to save user, Reason: ~p, Doc: ~p", [Reason, UserDoc]),
            {'error', kz_term:to_binary(Reason)};
        {'ok', _User} = Ok ->
            lager:debug("saved user ~p", [kz_doc:id(_User)]),
            Ok
    end.

%%------------------------------------------------------------------------------
%% @doc Delete a user from kazoo
%% @end
%%------------------------------------------------------------------------------
-spec delete_user(kz_type:ne_binary(), kz_tasks:args()) -> {'ok', kzd_users:doc()}  | {'error', kz_type:ne_binary()}.
delete_user(AccountId ,#{<<"user_id">> := UserId}) ->
    kz_tasks_utils:delete_doc(AccountId, UserId, <<"user">>).
