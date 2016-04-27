%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kz_data).

-include("kz_data.hrl").

-type connection() :: any().
-type options() :: wh_proplist().
-type document() :: wh_json:object().
-type documents() :: [document()].

-export_type([connection/0, options/0, document/0, documents/0]).

-callback new_connection(ne_binary()) -> connection().
-callback format_error(any()) -> any().


%% Connection operations
-callback get_db(connection(), ne_binary()) -> any().
-callback server_url(connection()) -> ne_binary().
-callback db_url(connection(), ne_binary()) -> ne_binary().
-callback server_info(connection()) -> any().

%% DB operations
-callback db_create(connection(), ne_binary(), options()) -> any().
-callback db_delete(connection(), ne_binary()) -> any().
-callback db_view_cleanup(connection(), ne_binary()) -> any().
-callback db_info(connection()) -> any().
-callback db_info(connection(), ne_binary()) -> any().
-callback db_exists(connection(), ne_binary()) -> boolean().
-callback db_archive(connection(), ne_binary(), ne_binary()) -> any().
-callback db_list(connection(), options()) -> any().

%% Document operations
-callback open_doc(connection(), ne_binary(), ne_binary(), options()) -> any().
-callback lookup_doc_rev(connection(), ne_binary(), ne_binary()) -> any().
-callback save_doc(connection(), ne_binary(), document(), options()) -> any().
-callback save_docs(connection(), ne_binary(), documents(), options()) -> any().
-callback del_doc(connection(), ne_binary(), document(), options()) -> any().
-callback del_docs(connection(), ne_binary(), documents(), options()) -> any().
-callback ensure_saved(connection(), ne_binary(), document(), options()) -> any().

%% Attachment-related
-callback fetch_attachment(connection(), ne_binary(), ne_binary(), ne_binary()) -> any().
-callback stream_attachment(connection(), ne_binary(), ne_binary(), ne_binary(), pid()) -> any().
-callback put_attachment(connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), options()) -> any().
-callback delete_attachment(connection(), ne_binary(), ne_binary(), ne_binary(), options()) -> any().
-callback attachment_url(connection(), ne_binary(), ne_binary(), ne_binary(), options()) -> any().

%% View-related
-callback design_info(connection(), ne_binary(), ne_binary()) -> any().
-callback all_design_docs(connection(), ne_binary(), options()) -> any().
-callback get_results(connection(), ne_binary(), ne_binary(), options()) -> any().
-callback get_results_count(connection(), ne_binary(), ne_binary(), options()) -> any().
-callback all_docs(connection(), ne_binary(), options()) -> any().
