%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fax_attachment).

-export([save_outbound/4, fetch/3]).


-export([fetch_faxable/2
        ,fetch_pdf/2
        ,fetch_received_pdf/2
        ,fetch_received/2
        ,fetch_original/2
        ,fetch_legacy/2
        ,fetch_url/1
        ]).

-export([store_attachments/3
        ,save_fax_doc/5
        ,store_attachment/5
        ]).

-define(ORIGINAL_FILE_PREFIX, "original_file").
-define(RECEIVED_FILE_PREFIX, "received_file").
-define(FAX_FILENAME, <<"fax_file.tiff">>).
-define(PDF_FILENAME, <<"pdf_file.pdf">>).

-define(RETRY_SAVE_ATTACHMENT_DELAY, 5000).

-define(FAX_CONFIG_CAT, <<"fax">>).


%%%=============================================================================
%%% attachment handling functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Faxes pre-convert attachment documents to tiff/pdf on ingress and save
%% all these formats to the db as attachments.
%%
%% If configured, the fax document for outbound faxes will contain a copy of the
%% post conversion fax tiff file and a pdf representation of this file.
%%
%% @end
%%------------------------------------------------------------------------------
-spec save_outbound(kz_term:ne_binary(), kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) ->
                           {'ok', kz_json:object()} |
                           {'error', any()}.
save_outbound(Db, Doc, 'undefined', _) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_url_document">>, true) of
        'true' ->
            case fetch_url(Doc) of
                {'ok', Content, ContentType} ->
                    save_outbound(Db, Doc, Content, ContentType);
                Error -> Error
            end;
        'false' -> {'ok', Doc}
    end;
save_outbound(Db, Doc, Original, ContentType) ->
    Id = kz_doc:id(Doc),
    Name = <<?ORIGINAL_FILE_PREFIX, ".", (kz_mime:to_extension(ContentType))/binary>>,
    Att = {Original, ContentType, Name},
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_tiff">>, true)
        orelse kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_pdf">>, true) of
        'true' ->
            case convert_to_fax(ContentType, Original, Id) of
                {'ok', Tiff, Props} ->
                    NewDoc = update_fax_props(Doc, Props),
                    NewAtt = [Att|maybe_store_fax_tiff(Tiff)],
                    store_attachments(Db, NewDoc, NewAtt ++ maybe_convert_to_pdf(<<"image/tiff">>, Tiff, Id));
                Error -> Error
            end;
        'false' ->
            store_attachments(Db, Doc, [Att])
    end.

-spec update_fax_props(kz_json:object(), kz_term:proplist())  -> kz_json:object().
update_fax_props(Doc, Props) ->
    kz_json:set_values([{<<"pvt_pages">>, props:get_value(<<"page_count">>, Props, 0)}
                       ,{<<"pvt_size">>, props:get_value(<<"size">>, Props, 0)}
                       ]
                      ,Doc
                      ).

-spec maybe_store_fax_tiff(kz_term:ne_binary()) ->
                                  [{kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}] | [].
maybe_store_fax_tiff(Content) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_tiff">>, true) of
        'true' -> [{Content, <<"image/tiff">>, ?FAX_FILENAME}];
        'false' -> []
    end.

-spec maybe_convert_to_pdf(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                  [{kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}] | [].
maybe_convert_to_pdf(ContentType, Content, Id) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_pdf">>, true) of
        'true' ->
            case convert_to_pdf(ContentType, Content, Id) of
                {'ok', Pdf} -> [{Pdf, <<"application/pdf">>, ?PDF_FILENAME}];
                _Error -> []
            end;
        'false' -> []
    end.

-spec convert_to_fax(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binary(), kz_term:proplist()} |
                            {'error', any()}.
convert_to_fax(FromFormat, File, Id) ->
    Options = [{<<"output_type">>, 'binary'}
              ,{<<"job_id">>, Id}
              ,{<<"read_metadata">>, 'true'}
              ],
    kz_convert:fax(FromFormat, <<"image/tiff">>, File, Options).

-spec convert_to_pdf(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {'ok', kz_term:ne_binary()} |
                            {'error', any()}.
convert_to_pdf(FromFormat, Content, Id) ->
    Options = [{<<"output_type">>, 'binary'}
              ,{<<"job_id">>, Id}
              ],
    kz_convert:fax(FromFormat, <<"application/pdf">>, Content, Options).

%%%=============================================================================
%%% attachment getter functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing sent and received fax documents.
%%
%% Note: Some of the fetch functions will save the document if it is not present in the format
%% requested and storage of the requested format is enabled in the fax app config.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                   {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                   {'error', kz_term:ne_binary()}.
fetch(Format, Db, Doc) ->
    case kz_json:get_binary_value(<<"folder">>, Doc) of
        <<"inbox">> ->
            lager:debug("fetching received fax attachment with format ~s", [Format]),
            fetch_received_format(Format, Db, Doc);
        <<"outbox">> ->
            lager:debug("fetching sent fax attachment with format ~s", [Format]),
            fetch_sent_format(Format, Db, Doc);
        'undefined' -> {'error', <<"no folder defined in doc">>}
    end.

-spec fetch_sent_format(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                               {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                               {'error', kz_term:ne_binary()}.
fetch_sent_format(<<"original">>, Db, Doc) ->
    fetch_original(Db, Doc);
fetch_sent_format(<<"pdf">>, Db, Doc) ->
    fetch_pdf(Db, Doc);
fetch_sent_format(<<"tiff">>, Db, Doc) ->
    fetch_faxable(Db, Doc);
fetch_sent_format(_, _, _) ->
    {'error', <<"invalid format for attachment">>}.

-spec fetch_received_format(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                                   {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                   {'error', kz_term:ne_binary()}.
fetch_received_format(<<"original">>, Db, Doc) ->
    fetch_received(Db, Doc);
fetch_received_format(<<"pdf">>, Db, Doc) ->
    fetch_received_pdf(Db, Doc);
fetch_received_format(<<"tiff">>, Db, Doc) ->
    fetch_received(Db, Doc);
fetch_received_format(_, _, _) ->
    {'error', <<"invalid format for attachment">>}.

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing the received document attachment.
%% @end
%%------------------------------------------------------------------------------
-spec fetch_received(kz_term:ne_binary(), kz_json:object()) ->
                            {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                            {'error', kz_term:ne_binary()}.
fetch_received(Db, Doc) ->
    fetch_received(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_received(kz_term:ne_binary(), kz_json:object(), list()) ->
                            {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                            {'error', kz_term:ne_binary()}.
fetch_received(Db, Doc, [<<?RECEIVED_FILE_PREFIX, _/binary>>=Name|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), Name) of
        {'ok', Content} -> {'ok', Content, <<"image/tiff">>, Doc};
        Error -> Error
    end;
fetch_received(Db, Doc, [_|Attachments]) ->
    fetch_received(Db, Doc, Attachments);
fetch_received(Db, Doc, []) ->
    lager:debug("no received attachments found, fetching legacy attachment"),
    fetch_legacy(Db, Doc).

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing attachments suitable for fax transmission.
%%
%% Saves the document if it is not present and store_fax_tiff is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_faxable(kz_term:ne_binary(), kz_json:object()) ->
                           {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                           {'error', any()}.
fetch_faxable(Db, Doc) ->
    fetch_faxable(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_faxable(kz_term:ne_binary(), kz_json:object(), list()) ->
                           {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                           {'error', kz_term:ne_binary()}.
fetch_faxable(Db, Doc, [?FAX_FILENAME|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), ?FAX_FILENAME) of
        {'ok', Content} -> {'ok', Content, <<"image/tiff">>, Doc};
        Error -> Error
    end;
fetch_faxable(Db, Doc, [_|Attachments]) ->
    fetch_faxable(Db, Doc, Attachments);
fetch_faxable(Db, Doc, []) ->
    lager:debug("no faxable tiff attachments found, fetching original attachment"),
    case fetch_original(Db, Doc) of
        {'ok', Content, ContentType, NewDoc} ->
            case convert_to_fax(ContentType, Content, kz_doc:id(NewDoc)) of
                {'ok', Tiff, Props} ->
                    NewerDoc = update_fax_props(NewDoc, Props),
                    NewestDoc = maybe_save_faxable(Db, NewerDoc, Content),
                    {'ok', Tiff, <<"image/tiff">>, NewestDoc};
                Error -> Error
            end;
        Error -> Error
    end.

-spec maybe_save_faxable(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_save_faxable(Db, Doc, Content) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_tiff">>, true) of
        'true' ->
            case save_fax_doc(Db, Doc, Content, <<"image/tiff">>, ?FAX_FILENAME) of
                {'ok', NewDoc} -> NewDoc;
                _Else ->
                    lager:error("failed to save fax tiff attachment for document ~p", [kz_doc:id(Doc)]),
                    Doc
            end;
        'false' -> Doc
    end.

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing/creating a pdf file of a transmitted fax.
%%
%% Saves the document if it is not present and store_fax_pdf is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_pdf(kz_term:ne_binary(), kz_json:object()) ->
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                       {'error', kz_term:ne_binary()}.
fetch_pdf(Db, Doc) ->
    fetch_pdf(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_pdf(kz_term:ne_binary(), kz_json:object(), list()) ->
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                       {'error', kz_term:ne_binary()}.
fetch_pdf(Db, Doc, [?PDF_FILENAME|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), ?PDF_FILENAME) of
        {'ok', Content} -> {'ok', Content, <<"application/pdf">>, Doc};
        Error -> Error
    end;
fetch_pdf(Db, Doc, [_|Attachments]) ->
    fetch_pdf(Db, Doc, Attachments);
fetch_pdf(Db, Doc, []) ->
    lager:debug("no fax pdf attachments found, fetching faxable attachment"),
    case fetch_faxable(Db, Doc) of
        {'ok', Content, ContentType, NewDoc} ->
            case convert_to_pdf(ContentType, Content, kz_doc:id(NewDoc)) of
                {'ok', Pdf} ->
                    NewerDoc = maybe_save_pdf(Db, Pdf, NewDoc),
                    {'ok', Pdf, <<"application/pdf">>, NewerDoc};
                Error -> Error
            end;
        Error -> Error
    end.

-spec maybe_save_pdf(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                            kz_json:object().
maybe_save_pdf(Db, Pdf, Doc) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_pdf">>, true) of
        'true' ->
            case save_fax_doc(Db, Doc, Pdf, <<"application/pdf">>, ?PDF_FILENAME) of
                {'ok', NewDoc} -> NewDoc;
                _Else ->
                    lager:error("failed to save pdf attachment for document ~p", [kz_doc:id(Doc)]),
                    Doc
            end;
        'false' -> Doc
    end.


%%------------------------------------------------------------------------------
%% @doc Helper function for accessing/creating a pdf file of a received fax
%%
%% Saves the document if it is not present and store_fax_pdf is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_received_pdf(kz_term:ne_binary(), kz_json:object()) ->
                                {'ok', iodata(), kz_term:ne_binary(), kz_json:object()} |
                                kz_datamgr:data_error().
fetch_received_pdf(Db, Doc) ->
    fetch_received_pdf(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_received_pdf(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binaries()) ->
                                {'ok', iodata(), kz_term:ne_binary(), kz_json:object()} |
                                kz_datamgr:data_error().
fetch_received_pdf(Db, Doc, [?PDF_FILENAME|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), ?PDF_FILENAME) of
        {'ok', Content} -> {'ok', Content, <<"application/pdf">>, Doc};
        Error -> Error
    end;
fetch_received_pdf(Db, Doc, [_|Attachments]) ->
    fetch_received_pdf(Db, Doc, Attachments);
fetch_received_pdf(Db, Doc, []) ->
    lager:debug("no fax pdf attachments found, fetching received attachment"),
    case fetch_received(Db, Doc) of
        {'ok', Content, ContentType, NewDoc} ->
            case convert_to_pdf(ContentType, Content, kz_doc:id(NewDoc)) of
                {'ok', Pdf} ->
                    NewerDoc = maybe_save_pdf(Db, Pdf, NewDoc),
                    {'ok', Pdf, <<"application/pdf">>, NewerDoc};
                Error -> Error
            end;
        Error -> Error
    end.


%%------------------------------------------------------------------------------
%% @doc Helper function for accessing/creating original sent attachment.
%% @end
%%------------------------------------------------------------------------------
-spec fetch_original(kz_term:ne_binary(), kz_json:object()) ->
                            {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                            {'error', kz_term:ne_binary()}.
fetch_original(Db, Doc) ->
    fetch_original(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_original(kz_term:ne_binary(), kz_json:object(), list()) ->
                            {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                            {'error', kz_term:ne_binary()}.
fetch_original(Db, Doc, [<<?ORIGINAL_FILE_PREFIX, _/binary>>=Name|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), Name) of
        {'ok', Content} -> {'ok', Content, kz_doc:attachment_content_type(Doc, Name), Doc};
        Error -> Error
    end;
fetch_original(Db, Doc, [_|Attachments]) ->
    fetch_original(Db, Doc, Attachments);
fetch_original(Db, Doc, []) ->
    lager:debug("no original attachments found, fetching received attachment"),
    fetch_legacy(Db, Doc).

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing a legacy attachment.
%%
%% Checks if any attachments are present on the document that are not reserved
%% filenames. If none are found, attempts to fetch an attachment url if document present in the doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_legacy(kz_term:ne_binary(), kz_json:object()) ->
                          {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                          {'error', kz_term:ne_binary()}.
fetch_legacy(Db, Doc) ->
    fetch_legacy(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_legacy(kz_term:ne_binary(), kz_json:object(), list()) ->
                          {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                          {'error', kz_term:ne_binary()}.
fetch_legacy(Db, Doc, [?FAX_FILENAME|Attachments]) ->
    fetch_legacy(Db, Doc, Attachments);
fetch_legacy(Db, Doc, [?PDF_FILENAME|Attachments]) ->
    fetch_legacy(Db, Doc, Attachments);
fetch_legacy(Db, Doc, [<<?ORIGINAL_FILE_PREFIX, _/binary>>|Attachments]) ->
    fetch_legacy(Db, Doc, Attachments);
fetch_legacy(Db, Doc, [<<?RECEIVED_FILE_PREFIX, _/binary>>|Attachments]) ->
    fetch_legacy(Db, Doc, Attachments);
fetch_legacy(Db, Doc, [Name|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), Name) of
        {'ok', Content} ->
            {'ok', Content, kz_doc:attachment_content_type(Doc, Name), Doc};
        Error -> Error
    end;
fetch_legacy(Db, Doc, []) ->
    lager:debug("no legacy attachments found, fetching url attachment"),
    case fetch_url(Doc) of
        {'ok', Content, ContentType} ->
            NewDoc = maybe_store_url_attachment(Db, Doc, Content, ContentType),
            {'ok', Content, ContentType, NewDoc};
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing a url attachment document.
%%
%% Saves the document if it is not present and store_url_doc is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_url(kz_json:object()) ->
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                       {'error', kz_term:ne_binary()}.
fetch_url(Doc) ->
    case kzd_fax:document(Doc, 'undefined') of
        'undefined' ->
            lager:info("no attachment found on doc ~s", [kz_doc:id(Doc)]),
            {'error', <<"no attachment found">>};
        FetchRequest  ->
            fetch_url(kzd_fax:document_url(Doc), FetchRequest)
    end.

-spec fetch_url(kz_term:api_binary(), kz_json:object()) ->
                       {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                       {'error', kz_term:ne_binary()}.
fetch_url('undefined', _) ->
    {'error', <<"attachment not found">>};
fetch_url(Url, FetchRequest) ->
    Method = kz_term:to_atom(kz_json:get_value(<<"method">>, FetchRequest, <<"get">>), 'true'),
    Headers = props:filter_undefined(
                [{"Host", kz_json:get_string_value(<<"host">>, FetchRequest)}
                ,{"Referer", kz_json:get_string_value(<<"referer">>, FetchRequest)}
                ,{"Content-Type", kz_json:get_string_value(<<"content_type">>, FetchRequest, <<"text/plain">>)}
                ]),
    Body = kz_json:get_string_value(<<"content">>, FetchRequest, ""),
    lager:debug("making ~s request to '~s'", [Method, Url]),
    case kz_http:req(Method, Url, Headers, Body) of
        {'ok', 200, RespHeaders, Contents} ->
            DefaultCt = kz_mime:from_filename(Url),
            CT = props:get_value("Content-Type", RespHeaders, DefaultCt),
            ContentType = kz_mime:normalize_content_type(CT),
            {'ok', Contents, ContentType};
        {'ok', Status, _, _} ->
            lager:error("failed to fetch file for job from: ~s, http response: ~b", [Url, Status]);
        {'error', Reason} ->
            lager:error("failed to fetch file from: ~s for job: ~p", [Url, Reason])
    end.

-spec maybe_store_url_attachment(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                                        kz_json:object().
maybe_store_url_attachment(Db, Doc, Content, ContentType) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_url_document">>, true) of
        true ->
            Name = <<?ORIGINAL_FILE_PREFIX, ".", (kz_mime:to_extension(ContentType))/binary>>,
            case save_fax_doc(Db, Doc, Content, ContentType, Name) of
                {'ok', NewDoc} ->
                    NewDoc;
                {'error', Msg} ->
                    lager:info("failed to save url doc with error ~p", [Msg]),
                    Doc
            end;
        false ->
            Doc
    end.

%%%=============================================================================
%%% attachment save functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Common method for the safe saving of attachments.
%%
%% Bigcouch sometimes has issues where it returns a 409 when attaching files
%% it then actually attaches to the document. When this happens it increments the rev
%% without indicating this change in the response. Subsequent saves seem to experience
%% the same issue which can result in looping. To avoid this condition, if a save attempt
%% fails, check the doc for an attachment and return success response if the
%% attachment is found.
%%
%% @end
%%------------------------------------------------------------------------------
-spec store_attachments(kz_term:ne_binary(), kz_json:object(), list()) ->
                               {'ok', kz_json:object()} |
                               {'error', any()}.
store_attachments(Db, Doc, [{Content, CT, Name}|Files]) ->
    case save_fax_doc(Db, Doc, Content, CT, Name) of
        {'ok', NewDoc} ->
            store_attachments(Db, NewDoc, Files);
        Error -> Error
    end;
store_attachments(_, Doc, []) ->
    {'ok', Doc}.

-spec maybe_save_fax_doc(kz_term:ne_binary(), kz_json:object()) ->
                                {'ok', kz_json:object()} |
                                {'error', any()}.
maybe_save_fax_doc(Db, Doc) ->
    case kz_doc:revision(Doc) of
        'undefined' ->
            lager:debug("saving fax doc with id ~s", [kz_doc:id(Doc)]),
            kz_datamgr:save_doc(Db, Doc);
        _ -> {'ok', Doc}
    end.

-spec save_fax_doc(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                          {'ok', kz_json:object()} |
                          {'error', any()}.
save_fax_doc(Db, Doc, Content, CT, Name) ->
    case maybe_save_fax_doc(Db, Doc) of
        {'error', _}=Error -> Error;
        {'ok', NewDoc} -> store_attachment(Db, NewDoc, Content, CT, Name)
    end.

-spec store_attachment(kz_term:ne_binary(), kz_term:api_object(), binary(), kz_term:ne_binary(), kz_term:ne_binary())->
                              {'ok', kz_json:object()} |
                              {'error', kz_term:ne_binary()}.
store_attachment(Db, Doc, Content, CT, Name) ->
    MaxStorageRetry = kapps_config:get_integer(?FAX_CONFIG_CAT, <<"max_storage_retry">>, 5),
    lager:debug("saving fax attachment ~s to ~s", [Name, kz_doc:id(Doc)]),
    store_attachment(Db, Doc, Content, CT, Name, MaxStorageRetry).

-spec store_attachment(kz_term:ne_binary(), kz_term:api_object(), binary(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer())->
                              {'ok', kz_json:object()} |
                              {'error', kz_term:ne_binary()}.
store_attachment(_, Doc, _Content, _CT, _Name, 0) ->
    lager:error("max retry saving attachment ~s on fax id ~s rev ~s"
               ,[_Name, kz_doc:id(Doc), kz_doc:revision(Doc)]
               ),
    {'error', <<"max retry saving attachment">>};
store_attachment(Db, Doc, Content, CT, Name, Count) ->
    DocId = kz_doc:id(Doc),
    _ = attempt_save(Db, Doc, Content, CT, Name),
    case check_fax_attachment(Db, DocId, Name) of
        {'ok', _}=Ok -> Ok;
        {'missing', NewDoc} ->
            lager:warning("missing fax attachment on fax id ~s",[DocId]),
            timer:sleep(?RETRY_SAVE_ATTACHMENT_DELAY),
            store_attachment(Db, NewDoc, Content, CT, Name, Count-1);
        {'error', _R} ->
            lager:debug("error '~p' saving fax attachment on fax id ~s",[_R, DocId]),
            timer:sleep(?RETRY_SAVE_ATTACHMENT_DELAY),
            {'ok', NewDoc} = kz_datamgr:open_doc(Db, DocId),
            store_attachment(Db, NewDoc, Content, CT, Name, Count-1)
    end.

-spec attempt_save(kz_term:ne_binary(), kz_json:object(), binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                          {'ok', kz_json:object()} |
                          kz_datamgr:data_error().
attempt_save(Db, Doc, Content, CT, Name) ->
    Opts = [{'content_type', CT}
           ],
    kz_datamgr:put_attachment(Db, kz_doc:id(Doc), Name, Content, Opts).

-spec check_fax_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary())->
                                  {'ok', kz_json:object()} |
                                  {'missing', kz_json:object()} |
                                  {'error', any()}.
check_fax_attachment(Db, DocId, Name) ->
    case kz_datamgr:open_doc(Db, DocId) of
        {'ok', Doc} ->
            case kz_doc:attachment(Doc, Name) of
                'undefined' -> {'missing', Doc};
                _Else -> {'ok', Doc}
            end;
        {'error', _}=E -> E
    end.
