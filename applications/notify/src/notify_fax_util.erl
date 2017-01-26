%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_fax_util).

-export([get_attachment/2, get_attachment/3]).

-include("notify.hrl").

-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"">>).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(kz_proplist(), string()) -> ne_binary().
get_file_name(Props, Ext) ->
    Fax = props:get_value(<<"fax">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Fax), props:get_value(<<"caller_id_number">>, Fax)} of
                   {'undefined', 'undefined'} -> <<"Unknown">>;
                   {'undefined', Num} -> kz_term:to_binary(Num);
                   {Name, _} -> kz_term:to_binary(Name)
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Fax, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", kz_util:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(kz_term:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_attachment(ne_binary(), kz_proplist()) ->
                            {ne_binary(), ne_binary(), ne_binary()} |
                            {'error', any()}.
get_attachment(Category, Props) ->
    UseDb = props:get_value(<<"account_db">>, Props, ?KZ_FAXES_DB),
    get_attachment(UseDb, Category, Props).

-spec get_attachment(ne_binary(), ne_binary(), kz_proplist()) ->
                            {ne_binary(), ne_binary(), ne_binary()} |
                            {'error', any()}.
get_attachment(UseDb, Category, Props) ->
    Fax   = props:get_value(<<"fax">>, Props),
    FaxId = props:get_first_defined([<<"fax_jobid">>, <<"fax_id">>], Fax),

    {'ok', AttachmentBin, ContentType} = raw_attachment_binary(UseDb, FaxId),

    case kapps_config:get_binary(Category, <<"attachment_format">>, <<"pdf">>) of
        <<"pdf">> -> convert_to_pdf(AttachmentBin, Props, ContentType);
        _Else -> convert_to_tiff(AttachmentBin, Props, ContentType)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec raw_attachment_binary(ne_binary(), ne_binary()) ->
                                   {'ok', ne_binary(), ne_binary()}.
-spec raw_attachment_binary(ne_binary(), ne_binary(), non_neg_integer()) ->
                                   {'ok', ne_binary(), ne_binary()}.
raw_attachment_binary(Db, FaxId) ->
    raw_attachment_binary(Db, FaxId, 2).

raw_attachment_binary(Db, FaxId, Retries) when Retries > 0 ->
    lager:debug("get raw attachment ~s / ~s", [Db, FaxId]),

    case kz_datamgr:open_doc(Db, FaxId) of
        {'error','not_found'} when Db =/= ?KZ_FAXES_DB ->
            raw_attachment_binary(?KZ_FAXES_DB, FaxId, Retries);
        {'ok', FaxJObj} ->
            case kz_doc:attachment_names(FaxJObj) of
                [AttachmentId | _] ->
                    ContentType = kz_doc:attachment_content_type(FaxJObj, AttachmentId, <<"image/tiff">>),
                    {'ok', AttachmentBin} = kz_datamgr:fetch_attachment(Db, FaxId, AttachmentId),
                    {'ok', AttachmentBin, ContentType};
                [] ->
                    lager:debug("failed to find the attachment, retrying ~b more times", [Retries]),
                    timer:sleep(?MILLISECONDS_IN_MINUTE * 5),
                    raw_attachment_binary(Db, FaxId, Retries)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_to_tiff(ne_binary(), kz_proplist(), ne_binary()) ->
                             {ne_binary(), ne_binary(), ne_binary()}.
convert_to_tiff(AttachmentBin, Props, _ContentType) ->
    {<<"image/tiff">>, get_file_name(Props, "tiff"), AttachmentBin}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_to_pdf(ne_binary(), kz_proplist(), ne_binary()) ->
                            {ne_binary(), ne_binary(), ne_binary()} |
                            {'error', any()}.
convert_to_pdf(AttachmentBin, Props, <<"application/pdf">>) ->
    {<<"application/pdf">>, get_file_name(Props, "pdf"), AttachmentBin};
convert_to_pdf(AttachmentBin, Props, _ContentType) ->
    TiffFile = tmp_file_name(<<"tiff">>),
    PDFFile = tmp_file_name(<<"pdf">>),
    kz_util:write_file(TiffFile, AttachmentBin),
    ConvertCmd = kapps_config:get_binary(<<"notify.fax">>, <<"tiff_to_pdf_conversion_command">>, ?TIFF_TO_PDF_CMD),
    Cmd = io_lib:format(ConvertCmd, [PDFFile, TiffFile]),
    lager:debug("running command: ~s", [Cmd]),
    _ = os:cmd(Cmd),
    kz_util:delete_file(TiffFile),
    case file:read_file(PDFFile) of
        {'ok', PDFBin} ->
            kz_util:delete_file(PDFFile),
            {<<"application/pdf">>, get_file_name(Props, "pdf"), PDFBin};
        {'error', _R}=E ->
            lager:debug("unable to convert tiff: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec tmp_file_name(ne_binary()) -> string().
tmp_file_name(Ext) ->
    kz_term:to_list(<<"/tmp/", (kz_binary:rand_hex(10))/binary, "_notify_fax.", Ext/binary>>).
