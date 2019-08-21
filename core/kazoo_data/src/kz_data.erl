%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc data adapter behaviour
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_data).

-include("kz_data.hrl").

-type connection() :: server() | any().
-type option() :: {atom(), term()}.
-type options() :: [option()].
-type document() :: kz_json:object().
-type documents() :: kz_json:objects().

-export_type([connection/0
             ,option/0, options/0
             ,document/0
             ,documents/0
             ,db_classification/0
             ]).

-callback new_connection(map()) -> connection().
-callback format_error(any()) -> any().

%% Connection operations
-callback get_db(connection(), kz_term:ne_binary()) -> any().
-callback server_url(connection()) ->
    kz_term:ne_binary() |
    {'error', 'resource_not_available'}.
-callback db_url(connection(), kz_term:ne_binary()) ->
    kz_term:ne_binary() |
    {'error', 'resource_not_available'}.
-callback server_info(connection()) -> any().

%% DB operations
-callback db_create(connection(), kz_term:ne_binary(), options()) -> any().
-callback db_delete(connection(), kz_term:ne_binary()) -> any().
-callback db_view_cleanup(connection(), kz_term:ne_binary()) -> any().
-callback db_info(connection()) -> any().
-callback db_info(connection(), kz_term:ne_binary()) -> any().
-callback db_exists(connection(), kz_term:ne_binary()) ->
    boolean() |
    {'error', 'resource_not_available'}.
-callback db_archive(connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
-callback db_list(connection(), options()) -> any().

%% Document operations
-callback open_doc(connection(), kz_term:ne_binary(), kz_term:ne_binary(), options()) -> any().
-callback lookup_doc_rev(connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
-callback save_doc(connection(), kz_term:ne_binary(), document(), options()) -> any().
-callback save_docs(connection(), kz_term:ne_binary(), documents(), options()) -> any().
-callback del_doc(connection(), kz_term:ne_binary(), document(), options()) -> any().
-callback del_docs(connection(), kz_term:ne_binary(), documents(), options()) -> any().
-callback ensure_saved(connection(), kz_term:ne_binary(), document(), options()) -> any().

%% Attachment-related
-callback fetch_attachment(connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
-callback stream_attachment(connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> any().
-callback put_attachment(connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), options()) -> any().
-callback delete_attachment(connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), options()) -> any().
-callback attachment_url(connection(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), options()) -> any().

%% View-related
-callback design_info(connection(), kz_term:ne_binary(), kz_term:ne_binary()) -> any().
-callback all_design_docs(connection(), kz_term:ne_binary(), options()) -> any().
-callback get_results(connection(), kz_term:ne_binary(), kz_term:ne_binary(), options()) -> any().
-callback get_results_count(connection(), kz_term:ne_binary(), kz_term:ne_binary(), options()) -> any().
-callback all_docs(connection(), kz_term:ne_binary(), options()) -> any().
