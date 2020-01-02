%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fixturedb_doc).

%% Document operations
-export([open_doc/4
        ,lookup_doc_rev/3
        ,save_doc/4
        ,save_docs/4
        ,del_doc/4
        ,del_docs/4
        ,ensure_saved/4
        ]).

-include("kz_fixturedb.hrl").

%%%=============================================================================
%%% Document operations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec open_doc(server_map(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_data:options()) -> doc_resp().
open_doc(Server, DbName, DocId, _Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    case kz_term:is_empty(DocId) of
        'true' -> {'error', 'empty_doc_id'};
        'false' -> kz_fixturedb_util:open_json(Db, DocId)
    end.

-spec lookup_doc_rev(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> {ok, kz_term:ne_binary()} | fixture_error().
lookup_doc_rev(Server, DbName, DocId) ->
    case open_doc(Server, DbName, DocId, []) of
        {'ok', Doc} -> {'ok', kz_doc:revision(Doc)};
        {'error', _}=Error -> Error
    end.

-spec save_doc(server_map(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> doc_resp().
save_doc(Server, DbName, Doc, Options) when not is_list(Doc) ->
    case open_doc(Server, DbName, kz_doc:id(Doc), Options) of
        {'ok', JObj} ->
            DocRev = kz_doc:revision(Doc),
            JObjRev = kz_doc:revision(JObj),
            case {DocRev, JObjRev} of
                {'undefined', _} -> {'ok', kz_fixturedb_util:update_doc(kz_doc:set_revision(Doc, JObjRev))};
                {_, 'undefined'} -> {'ok', kz_fixturedb_util:update_doc(Doc)};
                {DocRev, JObjRev} -> {'ok', kz_fixturedb_util:update_doc(Doc)};
                {_, _} -> {'error', 'conflict'}
            end;
        {'error', _} ->
            case kz_fixturedb_db:db_exists(Server, DbName) of
                'false' -> {'error', 'not_found'};
                'true' -> {'ok', kz_fixturedb_util:update_doc(Doc)}
            end
    end.

-spec save_docs(server_map(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) -> docs_resp().
save_docs(Server, DbName, Docs, Options) ->
    case kz_fixturedb_db:db_exists(Server, DbName) of
        'false' -> {'error', 'not_found'};
        'true' ->
            {'ok', [perform_save_doc(Server, DbName, Doc, Options) || Doc <- Docs]}
    end.

-spec del_doc(server_map(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> {'ok', kz_json:objects()}.
del_doc(Server, DbName, Doc, Options) ->
    del_docs(Server, DbName, [Doc], Options).

-spec del_docs(server_map(), kz_term:ne_binary(), kz_data:documents(), kz_data:options()) -> {'ok', kz_json:objects()}.
del_docs(Server, DbName, Docs, Options) ->
    {'ok', [perform_save_doc(Server, DbName, Doc, Options) || Doc <- Docs]}.

-spec ensure_saved(server_map(), kz_term:ne_binary(), kz_data:document(), kz_data:options()) -> doc_resp().
ensure_saved(Server, DbName, Doc, Options) ->
    case save_doc(Server, DbName, Doc, Options) of
        {'ok', _}=OK -> OK;
        {'error', 'conflict'} ->
            case lookup_doc_rev(Server, DbName, kz_doc:id(Doc)) of
                {'ok', Rev} -> ensure_saved(Server, DbName, kz_doc:set_revision(Doc, Rev), Options);
                {'error', 'not_found'} -> ensure_saved(Server, DbName, kz_doc:delete_revision(Doc), Options)
            end;
        {'error', _}=Error -> Error
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec perform_save_doc(server_map(), kz_term:ne_binary(), kz_json:object(), kz_data:options()) ->
          kz_json:object().
perform_save_doc(Server, DbName, JObj, Options) ->
    prepare_bulk_save_response(save_doc(Server, DbName, JObj, Options), JObj).

-spec prepare_bulk_save_response(doc_resp(), kz_json:object()) -> kz_json:object().
prepare_bulk_save_response({'ok', JObj}, _) ->
    kz_json:from_list(
      [{<<"ok">>, 'true'}
      ,{<<"id">>, kz_doc:id(JObj)}
      ,{<<"rev">>, kz_doc:revision(JObj)}
      ,{<<"accepted">>, 'true'}
      ]);
prepare_bulk_save_response({'error', Error}, JObj) ->
    Reason = kz_term:safe_cast(Error, <<"unknown">>, fun kz_term:to_binary/1),
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(JObj)}
      ,{<<"error">>, Reason}
      ,{<<"reason">>, <<"Document update failed due to error ", Reason/binary>>}
      ]).
