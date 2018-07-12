%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc Fax document manipulation
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_fax).

-export([new/0
        ,type/0
        ,owner_id/1, owner_id/2
        ,faxbox_id/1, faxbox_id/2
        ,timezone/1, timezone/2
        ,retries/1, retries/2
        ,attempts/1, attempts/2
        ,from_number/1, from_number/2
        ,from_name/1, from_name/2
        ,to_number/1, to_number/2
        ,to_name/1, to_name/2
        ,identity_number/1, identity_number/2
        ,identity_name/1, identity_name/2
        ,folder/1, folder/2
        ,document/1, document_url/1
        ,notifications/1
        ,rx_result/1, tx_result/1, result/1
        ,job_node/1, job_node/2
        ,job_status/1, job_status/2
        ,size/1, size/2
        ,pages/1, pages/2
        ,retry_after/1, retry_after/2
        ]
       ).

-export([save_outbound_fax/4]).

-export([fetch_faxable_attachment/2, fetch_faxable_attachment/3
        ,fetch_pdf_attachment/2, fetch_pdf_attachment/3
        ,fetch_original_attachment/2, fetch_original_attachment/3
        ,fetch_legacy_attachment/2, fetch_legacy_attachment/3
        ,fetch_attachment_url/1
        ]).

-export([save_fax_attachments/3
        ,save_fax_doc/5
        ,save_fax_attachment/5
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(KEY_FAXBOX_ID, <<"faxbox_id">>).
-define(KEY_OWNER_ID, <<"owner_id">>).
-define(KEY_TIMEZONE, <<"fax_timezone">>).
-define(KEY_RETRIES, <<"retries">>).
-define(KEY_RETRY_AFTER, <<"retry_after">>).
-define(KEY_ATTEMPTS, <<"attempts">>).
-define(KEY_FOLDER, <<"folder">>).
-define(KEY_NOTIFICATIONS, <<"notifications">>).
-define(KEY_RX_RESULT, <<"rx_result">>).
-define(KEY_TX_RESULT, <<"tx_result">>).
-define(KEY_PAGES, <<"pvt_pages">>).
-define(KEY_SIZE, <<"pvt_size">>).
-define(KEY_FROM_NAME, <<"from_name">>).
-define(KEY_FROM_NUMBER, <<"from_number">>).
-define(KEY_TO_NAME, <<"to_name">>).
-define(KEY_TO_NUMBER, <<"to_number">>).
-define(KEY_IDENTITY_NAME, <<"fax_identity_name">>).
-define(KEY_IDENTITY_NUMBER, <<"fax_identity_number">>).
-define(KEY_JOB_NODE, <<"pvt_job_node">>).
-define(KEY_JOB_STATUS, <<"pvt_job_status">>).
-define(KEY_DOCUMENT, <<"document">>).
-define(KEY_DOCUMENT_URL, [<<"document">>, <<"url">>]).

-define(PVT_TYPE, <<"fax">>).

-define(ORIGINAL_FILE_PREFIX, "original_file").
-define(FAX_FILENAME, <<"fax_file.tiff">>).
-define(PDF_FILENAME, <<"pdf_file.pdf">>).

-define(RETRY_SAVE_ATTACHMENT_DELAY, 5000).

-define(FAX_CONFIG_CAT, <<"fax">>).

-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec owner_id(doc()) -> kz_term:api_binary().
owner_id(FaxDoc) ->
    owner_id(FaxDoc, 'undefined').

-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(FaxDoc, Default) ->
    kz_json:get_value(?KEY_OWNER_ID, FaxDoc, Default).

-spec faxbox_id(doc()) -> kz_term:api_binary().
faxbox_id(FaxDoc) ->
    faxbox_id(FaxDoc, 'undefined').

-spec faxbox_id(doc(), Default) -> kz_term:ne_binary() | Default.
faxbox_id(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FAXBOX_ID, FaxDoc, Default).

-spec timezone(doc()) -> kz_term:api_binary().
timezone(FaxDoc) ->
    timezone(FaxDoc, 'undefined').

-spec timezone(doc(), Default) -> kz_term:ne_binary() | Default.
timezone(FaxDoc, Default) ->
    kz_json:get_value(?KEY_TIMEZONE, FaxDoc, Default).

-spec retries(doc()) -> kz_term:api_integer().
retries(FaxDoc) ->
    retries(FaxDoc, 'undefined').

-spec retries(doc(), Default) -> integer() | Default.
retries(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_RETRIES, FaxDoc, Default).

-spec attempts(doc()) -> kz_term:api_integer().
attempts(FaxDoc) ->
    attempts(FaxDoc, 'undefined').

-spec attempts(doc(), Default) -> integer() | Default.
attempts(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_ATTEMPTS, FaxDoc, Default).

-spec from_number(doc()) -> kz_term:api_binary().
from_number(FaxDoc) ->
    from_number(FaxDoc, 'undefined').

-spec from_number(doc(), Default) -> kz_term:api_binary() | Default.
from_number(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FROM_NUMBER, FaxDoc, Default).

-spec to_number(doc()) -> kz_term:api_binary().
to_number(FaxDoc) ->
    to_number(FaxDoc, 'undefined').

-spec to_number(doc(), Default) -> kz_term:api_binary() | Default.
to_number(FaxDoc, Default) ->
    kz_json:get_value(?KEY_TO_NUMBER, FaxDoc, Default).

-spec from_name(doc()) -> kz_term:api_binary().
from_name(FaxDoc) ->
    from_name(FaxDoc, 'undefined').

-spec from_name(doc(), Default) -> kz_term:api_binary() | Default.
from_name(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FROM_NAME, FaxDoc, Default).

-spec to_name(doc()) -> kz_term:api_binary().
to_name(FaxDoc) ->
    to_name(FaxDoc, 'undefined').

-spec to_name(doc(), Default) -> kz_term:api_binary() | Default.
to_name(FaxDoc, Default) ->
    kz_json:get_value(?KEY_TO_NAME, FaxDoc, Default).

-spec notifications(doc()) -> doc().
notifications(FaxDoc) ->
    kz_json:get_value(?KEY_NOTIFICATIONS, FaxDoc, kz_json:new()).

-spec folder(doc()) -> kz_term:api_binary().
folder(FaxDoc) ->
    folder(FaxDoc, 'undefined').

-spec folder(doc(), Default) -> kz_term:api_binary() | Default.
folder(FaxDoc, Default) ->
    kz_json:get_value(?KEY_FOLDER, FaxDoc, Default).

-spec document(doc()) -> doc().
document(FaxDoc) ->
    kz_json:get_value(?KEY_DOCUMENT, FaxDoc, kz_json:new()).

-spec document_url(doc()) -> doc() | 'undefined'.
document_url(FaxDoc) ->
    kz_json:get_value(?KEY_DOCUMENT_URL, FaxDoc).

-spec identity_name(doc()) -> kz_term:api_binary().
identity_name(FaxDoc) ->
    identity_name(FaxDoc, 'undefined').

-spec identity_name(doc(), Default) -> kz_term:api_binary() | Default.
identity_name(FaxDoc, Default) ->
    kz_json:get_value(?KEY_IDENTITY_NAME, FaxDoc, Default).

-spec identity_number(doc()) -> kz_term:api_binary().
identity_number(FaxDoc) ->
    identity_number(FaxDoc, 'undefined').

-spec identity_number(doc(), Default) -> kz_term:api_binary() | Default.
identity_number(FaxDoc, Default) ->
    kz_json:get_value(?KEY_IDENTITY_NUMBER, FaxDoc, Default).

-spec tx_result(doc()) -> doc().
tx_result(FaxDoc) ->
    kz_json:get_value(?KEY_TX_RESULT, FaxDoc, kz_json:new()).

-spec rx_result(doc()) -> doc().
rx_result(FaxDoc) ->
    kz_json:get_value(?KEY_RX_RESULT, FaxDoc, kz_json:new()).

-spec result(doc()) -> doc().
result(FaxDoc) ->
    kz_json:get_first_defined([?KEY_RX_RESULT, ?KEY_TX_RESULT], FaxDoc, kz_json:new()).

-spec job_node(doc()) -> kz_term:api_binary().
job_node(FaxDoc) ->
    job_node(FaxDoc, 'undefined').

-spec job_node(doc(), Default) -> kz_term:api_binary() | Default.
job_node(FaxDoc, Default) ->
    kz_json:get_value(?KEY_JOB_NODE, FaxDoc, Default).

-spec job_status(doc()) -> kz_term:api_binary().
job_status(FaxDoc) ->
    job_status(FaxDoc, 'undefined').

-spec job_status(doc(), Default) -> kz_term:api_binary() | Default.
job_status(FaxDoc, Default) ->
    kz_json:get_value(?KEY_JOB_STATUS, FaxDoc, Default).

-spec size(doc()) -> kz_term:api_integer().
size(FaxDoc) ->
    size(FaxDoc, 'undefined').

-spec size(doc(), Default) -> integer() | Default.
size(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_SIZE, FaxDoc, Default).

-spec pages(doc()) -> kz_term:api_integer().
pages(FaxDoc) ->
    pages(FaxDoc, 'undefined').

-spec pages(doc(), Default) -> integer() | Default.
pages(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_PAGES, FaxDoc, Default).

-spec retry_after(doc()) -> kz_term:api_integer().
retry_after(FaxDoc) ->
    retry_after(FaxDoc, 'undefined').

-spec retry_after(doc(), Default) -> integer() | Default.
retry_after(FaxDoc, Default) ->
    kz_json:get_integer_value(?KEY_RETRY_AFTER, FaxDoc, Default).

%%%=============================================================================
%%% attachment handling functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc faxes pre-convert attachment documents to tiff/pdf on ingress and save
%% all these formats to the db as attachments
%%
%% If configured, the fax document for outbound faxes will contain a copy of the
%% post conversion fax tiff file and a pdf representation of this file.
%%
%% @end
%%------------------------------------------------------------------------------
-spec save_outbound_fax(kz_term:ne_binary(), kz_json:object(), kz_term:api_binary(), kz_term:api_binary()) ->
                               {'ok', kz_json:object()} |
                               {'error', any()}.
save_outbound_fax(Db, Doc, 'undefined', _) ->
    case fetch_attachment_url(Doc) of
        {'ok', Content, ContentType} ->
            case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_url_document">>, true) of
                'true' -> save_outbound_fax(Db, Doc, Content, ContentType);
                'false' -> {'ok', Doc}
            end;
        Error -> Error
    end;
save_outbound_fax(Db, Doc, Original, ContentType) ->
    Id = kz_doc:id(Doc),
    Name = <<?ORIGINAL_FILE_PREFIX, (kz_mime:to_extension(ContentType))/binary>>,
    Att = [{Original, ContentType, Name}],
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_tiff">>, true) of
        'true' ->
            case convert_to_fax(Id, Original, ContentType) of
                {'ok', Tiff, Props} ->
                    NewDoc = update_fax_props(Doc, Props),
                    Att1 = Att ++ [{Tiff, ContentType, ?FAX_FILENAME}],
                    save_fax_attachments(Db, NewDoc, Att1 ++ [maybe_convert_to_pdf(Tiff, kz_doc:id(NewDoc))]);
                'noop' ->
                    save_fax_attachments(Db, Doc, Att);
                Error -> Error
            end;
        'false' ->
            save_fax_attachments(Db, Doc, Att)
    end.

-spec update_fax_props(kz_json:object(), kz_term:proplist()) -> kz_json:object().
update_fax_props(Doc, Props) ->
    kz_json:set_values([{<<"pvt_pages">>, props:get_value(<<"page_count">>, Props, 0)}
                       ,{<<"pvt_size">>, props:get_value(<<"size">>, Props, 0)}
                       ]
                      ,Doc
                      ).

-spec maybe_convert_to_pdf(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                  {kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} | 'noop'.
maybe_convert_to_pdf(Content, Id) ->
    case kapps_config:get_is_true(?FAX_CONFIG_CAT, <<"store_fax_pdf">>, true) of
        'true' ->
            case convert_fax_to_pdf(Content, Id) of
                {'ok', Pdf} -> {Pdf, <<"application/pdf">>, ?PDF_FILENAME};
                _Error -> 'noop'
            end;
        'false' -> 'noop'
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

-spec convert_fax_to_pdf(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                {'ok', kz_term:ne_binary()} |
                                {'error', any()}.
convert_fax_to_pdf(Content, Id) ->
    Options = [{<<"output_type">>, 'binary'}
              ,{<<"job_id">>, Id}
              ],
    kz_convert:fax(<<"image/tiff">>, <<"application/pdf">>, Content, Options).

%%%=============================================================================
%%% attachment getter functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Helper function for accessing attachments suitable for faxing.
%%
%% Saves the document if it is not present and store_fax_tiff is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_faxable_attachment(kz_term:ne_binary(), kz_json:object()) ->
                                      {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                      {'error', any()}.
fetch_faxable_attachment(Db, Doc) ->
    fetch_faxable_attachment(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_faxable_attachment(kz_term:ne_binary(), kz_json:object(), list()) ->
                                      {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                      {'error', kz_term:ne_binary()}.
fetch_faxable_attachment(Db, Doc, [?FAX_FILENAME|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), ?FAX_FILENAME) of
        {'ok', Content} -> {'ok', Content, <<"image/tiff">>, Doc};
        Error -> Error
    end;
fetch_faxable_attachment(Db, Doc, [_|Attachments]) ->
    fetch_faxable_attachment(Db, Doc, Attachments);
fetch_faxable_attachment(Db, Doc, []) ->
    case fetch_original_attachment(Db, Doc) of
        {'ok', Content, ContentType, Doc} ->
            case convert_to_fax(ContentType, Content, kz_doc:id(Doc)) of
                {'ok', Tiff, Props} ->
                    NewDoc = update_fax_props(Doc, Props),
                    NewerDoc = maybe_save_faxable(Db, NewDoc, Content),
                    {'ok', Tiff, <<"image/tiff">>, NewerDoc};
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
%% @doc helper function for accessing/creating pdf attachment suitable for email/api response
%%
%% Saves the document if it is not present and store_fax_pdf is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_pdf_attachment(kz_term:ne_binary(), kz_json:object()) ->
                                  {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                  {'error', kz_term:ne_binary()}.
fetch_pdf_attachment(Db, Doc) ->
    fetch_pdf_attachment(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_pdf_attachment(kz_term:ne_binary(), kz_json:object(), list()) ->
                                  {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                  {'error', kz_term:ne_binary()}.
fetch_pdf_attachment(Db, Doc, [?PDF_FILENAME|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), ?PDF_FILENAME) of
        {'ok', Content} -> {'ok', Content, <<"application/pdf">>, Doc};
        Error -> Error
    end;
fetch_pdf_attachment(Db, Doc, [_|Attachments]) ->
    fetch_pdf_attachment(Db, Doc, Attachments);
fetch_pdf_attachment(Db, Doc, []) ->
    case fetch_faxable_attachment(Db, Doc) of
        {'ok', Content, _, NewDoc} ->
            case convert_fax_to_pdf(Content, kz_doc:id(NewDoc)) of
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
%% @doc helper function for accessing/creating original attachment
%% @end
%%------------------------------------------------------------------------------
-spec fetch_original_attachment(kz_term:ne_binary(), kz_json:object()) ->
                                       {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                       {'error', kz_term:ne_binary()}.
fetch_original_attachment(Db, Doc) ->
    fetch_original_attachment(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_original_attachment(kz_term:ne_binary(), kz_json:object(), list()) ->
                                       {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                       {'error', kz_term:ne_binary()}.
fetch_original_attachment(Db, Doc, [{<<?ORIGINAL_FILE_PREFIX, _/binary>>=Name}|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), Name) of
        {'ok', Content} -> {'ok', Content, kz_doc:attachment_content_type(Doc, Name), Doc};
        Error -> Error
    end;
fetch_original_attachment(Db, Doc, [_|Attachments]) ->
    fetch_original_attachment(Db, Doc, Attachments);
fetch_original_attachment(Db, Doc, []) ->
    case fetch_legacy_attachment(Db, Doc) of
        {'ok', Content, ContentType, NewDoc} ->
            {'ok', Content, ContentType, NewDoc};
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc helper function for accessing a legacy attachment
%%
%% Checks if any attachments are present on the document. If none our found
%% attempts to fetch an attachment url if present on the doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_legacy_attachment(kz_term:ne_binary(), kz_json:object()) ->
                                     {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                     {'error', kz_term:ne_binary()}.
fetch_legacy_attachment(Db, Doc) ->
    fetch_legacy_attachment(Db, Doc, kz_doc:attachment_names(Doc)).

-spec fetch_legacy_attachment(kz_term:ne_binary(), kz_json:object(), list()) ->
                                     {'ok', kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()} |
                                     {'error', kz_term:ne_binary()}.
fetch_legacy_attachment(Db, Doc, [Name|_]) ->
    case kz_datamgr:fetch_attachment(Db, kz_doc:id(Doc), Name) of
        {'ok', Content} ->
            {'ok', Content, kz_doc:attachment_content_type(Doc, Name), Doc};
        Error -> Error
    end;
fetch_legacy_attachment(Db, Doc, []) ->
    case fetch_attachment_url(Doc) of
        {'ok', Content, ContentType} ->
            NewDoc = maybe_store_url_attachment(Db, Doc, Content, ContentType),
            {'ok', Content, ContentType, NewDoc};
        Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc helper function for accessing a url attachment document
%%
%% Saves the document if it is not present and store_url_doc is true.
%%
%% @end
%%------------------------------------------------------------------------------
-spec fetch_attachment_url(kz_json:object()) ->
                                  {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                                  {'error', kz_term:ne_binary()}.
fetch_attachment_url(Doc) ->
    case kz_json:get_value(<<"document">>, Doc) of
        'undefined' ->
            lager:info("no attachment found on doc ~s", [kz_doc:id(Doc)]),
            {'error', <<"no attachment found">>};
        FetchRequest  ->
            Url = kz_json:get_string_value(<<"url">>, FetchRequest),
            fetch_attachment_url(Url, FetchRequest)
    end.

-spec fetch_attachment_url(kz_term:api_binary(), kz_json:object()) ->
                                  {'ok', kz_term:ne_binary(), kz_term:ne_binary()} |
                                  {'error', kz_term:ne_binary()}.
fetch_attachment_url('undefined', _) ->
    {'error', <<"attachment not found">>};
fetch_attachment_url(Url, FetchRequest) ->
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
            case save_outbound_fax(Db, Doc, Content, ContentType) of
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
%% @doc common method for the safe saving of attachments
%%
%% Bigcouch sometimes has issues where it returns a 409 when attaching files
%% it then actually attaches the file. When this happens it increments the rev
%% without indicating this in the response. To avoid this condition. If a save
%% fails, check the doc for an attachment and return success response if the
%% attachment is found.
%%
%% @end
%%------------------------------------------------------------------------------
-spec save_fax_attachments(kz_term:ne_binary(), kz_json:object(), list()) ->
                                  {'ok', kz_json:object()} |
                                  {'error', any()}.
save_fax_attachments(Db, Doc, [{Content, CT, Name}|Files]) ->
    case save_fax_doc(Db, Doc, Content, CT, Name) of
        {'ok', NewDoc} ->
            save_fax_attachments(Db, NewDoc, Files);
        Error -> Error
    end;
save_fax_attachments(Db, Doc, ['noop'|Files]) ->
    save_fax_attachments(Db, Doc, Files);
save_fax_attachments(_, Doc, []) ->
    {'ok', Doc}.

-spec maybe_save_fax_doc(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
maybe_save_fax_doc(Db, Doc) ->
    case kz_doc:revision(Doc) of
        'undefined' ->
            lager:debug("saving fax doc with id ~s and rev ~s", [kz_doc:id(Doc), kz_doc:revision(Doc)]),
            case kz_datamgr:save_doc(Db, Doc) of
                {'ok', NewDoc} -> NewDoc;
                Error -> Error
            end;
        _ -> Doc
    end.

-spec save_fax_doc(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
                          {'ok', kz_json:object()} |
                          {'error', any()}.
save_fax_doc(Db, Doc, Content, CT, Name) ->
    case maybe_save_fax_doc(Db, Doc) of
        {'error', _}=Error -> Error;
        NewDoc -> save_fax_attachment(Db, NewDoc, Content, CT, Name)
    end.

-spec save_fax_attachment(kz_term:ne_binary(), kz_term:api_object(), binary(), kz_term:ne_binary(), kz_term:ne_binary())->
                                 {'ok', kz_json:object()} |
                                 {'error', kz_term:ne_binary()}.
save_fax_attachment(Db, Doc, Content, CT, Name) ->
    MaxStorageRetry = kapps_config:get_integer(?FAX_CONFIG_CAT, <<"max_storage_retry">>, 5),
    lager:debug("saving fax attachment ~s to ~s", [Name, kz_doc:id(Doc)]),
    save_fax_attachment(Db, Doc, Content, CT, Name, MaxStorageRetry).

-spec save_fax_attachment(kz_term:ne_binary(), kz_term:api_object(), binary(), kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer())->
                                 {'ok', kz_json:object()} |
                                 {'error', kz_term:ne_binary()}.
save_fax_attachment(_, Doc, _Content, _CT, _Name, 0) ->
    lager:error("max retry saving attachment ~s on fax id ~s rev ~s"
               ,[_Name, kz_doc:id(Doc), kz_doc:revision(Doc)]
               ),
    {'error', <<"max retry saving attachment">>};
save_fax_attachment(Db, Doc, Content, CT, Name, Count) ->
    DocId = kz_doc:id(Doc),
    _ = attempt_save(Db, Doc, Content, CT, Name),
    case check_fax_attachment(Db, DocId, Name) of
        {'ok', _}=Ok -> Ok;
        {'missing', NewDoc} ->
            lager:warning("missing fax attachment on fax id ~s",[DocId]),
            timer:sleep(?RETRY_SAVE_ATTACHMENT_DELAY),
            save_fax_attachment(Db, NewDoc, Content, CT, Name, Count-1);
        {'error', _R} ->
            lager:debug("error '~p' saving fax attachment on fax id ~s",[_R, DocId]),
            timer:sleep(?RETRY_SAVE_ATTACHMENT_DELAY),
            {'ok', NewDoc} = kz_datamgr:open_doc(Db, DocId),
            save_fax_attachment(Db, NewDoc, Content, CT, Name, Count-1)
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


