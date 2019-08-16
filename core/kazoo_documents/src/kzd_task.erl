%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc Task document
%%% @author Pierre Fenoll
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_tasks:id()) ->
                   {ok, kz_json:object()} |
                   {error, any()}.
fetch(Account, TaskId) ->
    View = ?KZ_TASKS_BY_ACCOUNT,
    ViewOptions = [{'key', [kz_util:format_account_id(Account), TaskId]}],
    case kz_datamgr:get_single_result(?KZ_TASKS_DB, View, ViewOptions) of
        {'error', _}=E -> E;
        {'ok', JObj} -> kz_json:get_value(<<"value">>, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(doc()) -> kz_term:ne_binary().
id(JObj) ->
    kz_doc:id(JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> <<"task">>.

-spec node(doc()) -> kz_term:ne_binary() | 'undefined'.
node(Doc) ->
    kz_json:get_ne_binary_value(?PVT_WORKER_NODE, Doc).

-spec account_id(doc()) -> kz_term:ne_binary().
account_id(Doc) ->
    kz_json:get_ne_binary_value(?PVT_ACCOUNT_ID, Doc).

-spec auth_account_id(doc()) -> kz_term:ne_binary().
auth_account_id(Doc) ->
    kz_json:get_ne_binary_value(?PVT_AUTH_ACCOUNT_ID, Doc).

-spec category(doc()) -> kz_term:ne_binary().
category(Doc) ->
    kz_json:get_ne_binary_value(?PVT_CATEGORY, Doc).

-spec action(doc()) -> kz_term:ne_binary().
action(Doc) ->
    kz_json:get_ne_binary_value(?PVT_ACTION, Doc).

-spec status(doc()) -> kz_term:ne_binary().
status(Doc) ->
    kz_json:get_ne_binary_value(?PVT_STATUS, Doc).

-spec file_name(doc()) -> kz_term:ne_binary().
file_name(Doc) ->
    kz_json:get_ne_binary_value(?PVT_FILENAME, Doc).

-spec start_timestamp(doc()) -> kz_time:api_seconds().
start_timestamp(Doc) ->
    kz_json:get_integer_value(?PVT_STARTED_AT, Doc).

-spec end_timestamp(doc()) -> kz_time:api_seconds().
end_timestamp(Doc) ->
    kz_json:get_integer_value(?PVT_FINISHED_AT, Doc).

-spec total_count(doc()) -> kz_term:api_pos_integer().
total_count(Doc) ->
    kz_json:get_integer_value(?PVT_TOTAL_ROWS, Doc).

-spec failure_count(doc()) -> kz_term:api_non_neg_integer().
failure_count(Doc) ->
    kz_json:get_integer_value(?PVT_TOTAL_ROWS_FAILED, Doc).

-spec success_count(doc()) -> kz_term:api_non_neg_integer().
success_count(Doc) ->
    kz_json:get_integer_value(?PVT_TOTAL_ROWS_SUCCEEDED, Doc).
