%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Task document
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kzd_task).

-export([fetch/2]).

-export([account_id/1
        ,action/1
        ,auth_account_id/1
        ,category/1
        ,end_timestamp/1
        ,failure_count/1
        ,file_name/1
        ,id/1
        ,node/1
        ,start_timestamp/1
        ,status/1
        ,success_count/1
        ,total_count/1
        ,type/0
        ]).

-include("kz_documents.hrl").
-include_lib("kazoo_tasks/include/tasks.hrl").
-include_lib("kazoo_tasks/include/task_fields.hrl").

-type doc() :: kz_json:object().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(api_binary(), kz_tasks:task_id() | 'undefined') ->
                   {'ok', kz_json:object()} |
                   {'error', any()}.
fetch('undefined', _) ->
    {'error', 'account_id_undefined'};
fetch(_, 'undefined') ->
    {'error', 'task_id_undefined'};
fetch(Account, Id) ->
    AccountId = kz_util:format_account_db(Account),
    case kz_datamgr:get_single_result(?KZ_TASKS_DB
                                     ,?KZ_TASKS_BY_ACCOUNT
                                     ,[{'key', [AccountId, Id]}]
                                     )
    of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            kz_json:get_value(<<"value">>, JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec id(doc()) -> ne_binary().
id(JObj) ->
    kz_doc:id(JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> <<"task">>.

%% @public
-spec node(doc()) -> ne_binary() | 'undefined'.
node(Doc) ->
    kz_json:get_ne_binary_value(?PVT_WORKER_NODE, Doc).

%% @public
-spec account_id(doc()) -> ne_binary().
account_id(Doc) ->
    kz_json:get_ne_binary_value(?PVT_ACCOUNT_ID, Doc).

%% @public
-spec auth_account_id(doc()) -> ne_binary().
auth_account_id(Doc) ->
    kz_json:get_ne_binary_value(?PVT_AUTH_ACCOUNT_ID, Doc).

%% @public
-spec category(doc()) -> ne_binary().
category(Doc) ->
    kz_json:get_ne_binary_value(?PVT_CATEGORY, Doc).

%% @public
-spec action(doc()) -> ne_binary().
action(Doc) ->
    kz_json:get_ne_binary_value(?PVT_ACTION, Doc).

%% @public
-spec status(doc()) -> ne_binary().
status(Doc) ->
    kz_json:get_ne_binary_value(?PVT_STATUS, Doc).

%% @public
-spec file_name(doc()) -> ne_binary().
file_name(Doc) ->
    kz_json:get_ne_binary_value(?PVT_FILENAME, Doc).

%% @public
-spec start_timestamp(doc()) -> api_seconds().
start_timestamp(Doc) ->
    kz_json:get_integer_value(?PVT_STARTED_AT, Doc).

%% @public
-spec end_timestamp(doc()) -> api_seconds().
end_timestamp(Doc) ->
    kz_json:get_integer_value(?PVT_FINISHED_AT, Doc).

%% @public
-spec total_count(doc()) -> api_pos_integer().
total_count(Doc) ->
    kz_json:get_integer_value(?PVT_TOTAL_ROWS, Doc).

%% @public
-spec failure_count(doc()) -> api_non_neg_integer().
failure_count(Doc) ->
    kz_json:get_integer_value(?PVT_TOTAL_ROWS_FAILED, Doc).

%% @public
-spec success_count(doc()) -> api_non_neg_integer().
success_count(Doc) ->
    kz_json:get_integer_value(?PVT_TOTAL_ROWS_SUCCEEDED, Doc).
