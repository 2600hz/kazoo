%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fixturedb_attachments).

%% Attachment-related
-export([fetch_attachment/4
        ,stream_attachment/5
        ,put_attachment/6
        ,delete_attachment/5
        ,attachment_url/5
        ]).

-include("kz_fixturedb.hrl").

%%%=============================================================================
%%% Attachment-related
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> {ok, binary()} | fixture_error().
fetch_attachment(Server, DbName, DocId, AName) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    kz_fixturedb_util:open_attachment(Db, DocId, AName).

-spec stream_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), pid()) -> {ok, reference()} | fixture_error().
stream_attachment(Server, DbName, DocId, AName, Caller) ->
    AttResult = fetch_attachment(Server, DbName, DocId, AName),
    Ref = erlang:make_ref(),
    kz_process:spawn(fun relay_stream_attachment/3, [Caller, Ref, AttResult]),
    {ok, Ref}.

-spec put_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> doc_resp().
put_attachment(Server, DbName, DocId, AName, Contents, Options) ->
    Doc = kz_fixturedb_doc:open_doc(Server, DbName, DocId, Options),
    prepare_att_doc(Doc, AName, Contents, Options).

-spec delete_attachment(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> docs_resp().
delete_attachment(Server, DbName, DocId, _AName, Options) ->
    Doc = kz_fixturedb_doc:open_doc(Server, DbName, DocId, Options),
    del_att_response(Doc).

-spec attachment_url(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> kz_term:ne_binary().
attachment_url(Server, DbName, DocId, AName, _Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    kz_fixturedb_util:att_path(Db, DocId, AName).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec relay_stream_attachment(pid(), reference(), {ok, reference()} | fixture_error()) -> any().
relay_stream_attachment(Caller, Ref, AttResult) ->
    Caller ! {Ref, AttResult}.

-spec prepare_att_doc(doc_resp(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> doc_resp().
prepare_att_doc({ok, Doc}, AName, Contents, Options) ->
    JObj = kz_fixturedb_util:update_revision(Doc),
    Rev = kz_doc:revision(JObj),
    Att = kz_json:from_list_recursive(
            [{<<"_attachments">>
             ,[{AName, [{<<"content_type">>, props:get_value(content_type, Options, <<"application/octet-stream">>)}
                       ,{<<"revpos">>, Rev}
                       ,{<<"digest">>, <<"md5-", (base64:encode(crypto:hash(md5, Contents)))/binary>>}
                       ,{<<"length">>, erlang:size(Contents)}
                       ,{<<"stub">>, true}
                       ]
               }]
             }
            ]),
    NewAtt = kz_json:merge(kz_json:get_json_value(<<"_attachments">>, JObj, kz_json:new()), Att),
    {ok, kz_json:set_value(<<"_attachments">>, NewAtt, JObj)};
prepare_att_doc({error, _}=Error, _, _, _) ->
    Error.

del_att_response({ok, JObj}) ->
    {ok, kz_json:from_list(
           [{<<"id">>, kz_doc:id(JObj)}
           ,{<<"rev">>, kz_doc:revision(kz_fixturedb_util:update_revision(JObj))}
           ])
    };
del_att_response({error, _}=Error) ->
    Error.
