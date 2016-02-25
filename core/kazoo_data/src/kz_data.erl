%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kz_data).

-include("kz_data.hrl").

-callback new_connection(ne_binary()) -> {atom(), #{}}.
-callback format_error(_) -> any().

%% Connection operations
-callback get_db(_, _) -> any().
-callback server_url(_) -> any().
-callback db_url(_, _) -> any().
-callback server_info(_) -> any().

%% DB operations
-callback db_create(_, _, _) -> any().
-callback db_delete(_, _) -> any().
-callback db_view_cleanup(_, _) -> any().
-callback db_info(_) -> any().
-callback db_info(_, _) -> any().
-callback db_exists(_, _) -> boolean().
-callback db_archive(_, _, _) -> any().
-callback db_list(_, _) -> any().

%% Document operations
-callback open_doc(_, _, _, _) -> any().
-callback lookup_doc_rev(_, _, _) -> any().
-callback save_doc(_, _, _, _) -> any().
-callback save_docs(_, _, _, _) -> any().
-callback del_doc(_, _, _, _) -> any().
-callback del_docs(_, _, _, _) -> any().
-callback ensure_saved(_, _, _, _) -> any().
-callback copy_doc(_, _, _) -> any().
-callback move_doc(_, _, _) -> any().

%% Attachment-related
-callback fetch_attachment(_, _, _, _) -> any().
-callback stream_attachment(_, _, _, _, _) -> any().
-callback put_attachment(_, _, _, _, _, _) -> any().
-callback delete_attachment(_, _, _, _, _) -> any().

%% View-related
-callback design_info(_, _, _) -> any().
-callback all_design_docs(_, _, _) -> any().
-callback get_results(_, _, _, _) -> any().
-callback get_results_count(_, _, _, _) -> any().
-callback all_docs(_, _, _) -> any().