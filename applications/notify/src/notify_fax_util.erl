%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_fax_util).

-export([get_attachment/2]).

-include("notify.hrl").

-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"">>).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% create a friendly file name
%% @end
%%--------------------------------------------------------------------
-spec get_file_name(wh_proplist(), string()) -> ne_binary().
get_file_name(Props, Ext) ->
    Fax = props:get_value(<<"fax">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Fax), props:get_value(<<"caller_id_number">>, Fax)} of
                   {'undefined', 'undefined'} -> <<"Unknown">>;
                   {'undefined', Num} -> wh_util:to_binary(Num);
                   {Name, _} -> wh_util:to_binary(Name)
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Fax, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(wh_util:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_attachment(ne_binary(), wh_proplist()) ->
                            {ne_binary(), ne_binary(), ne_binary()} |
                            {'error', any()}.
get_attachment(Category, Props) ->
    {'ok', AttachmentBin, ContentType} = raw_attachment_binary(Props),
    case whapps_config:get_binary(Category, <<"attachment_format">>, <<"pdf">>) of
        <<"pdf">> -> convert_to_pdf(AttachmentBin, Props, ContentType);
        _Else -> convert_to_tiff(AttachmentBin, Props, ContentType)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec raw_attachment_binary(wh_proplist()) ->
                                   {'ok', ne_binary(), ne_binary()}.
-spec raw_attachment_binary(ne_binary(), ne_binary()) ->
                                   {'ok', ne_binary(), ne_binary()}.
-spec raw_attachment_binary(ne_binary(), ne_binary(), non_neg_integer()) ->
                                   {'ok', ne_binary(), ne_binary()}.
raw_attachment_binary(Props) ->
    Fax = props:get_value(<<"fax">>, Props),
    FaxId = props:get_first_defined([<<"fax_jobid">>, <<"fax_id">>], Fax),
    Db = props:get_value(<<"account_db">>, Props, ?WH_FAXES_DB),
    lager:debug("raw attachment ~s / ~s", [Db, FaxId]),
    raw_attachment_binary(Db, FaxId).

raw_attachment_binary(Db, FaxId) ->
    raw_attachment_binary(Db, FaxId, 2).

raw_attachment_binary(Db, FaxId, Retries) when Retries > 0 ->
    case couch_mgr:open_doc(Db, FaxId) of
        {'error','not_found'} when Db =/= ?WH_FAXES_DB ->
            raw_attachment_binary(?WH_FAXES_DB, FaxId, Retries);
        {'ok', FaxJObj} ->
            case wh_doc:attachment_names(FaxJObj) of
                [AttachmentId | _] ->
                    ContentType = case wh_doc:attachment_content_type(FaxJObj, AttachmentId) of
                                      'undefined' -> <<"image/tiff">>;
                                      CT -> CT
                                  end,
                    {'ok', AttachmentBin} = couch_mgr:fetch_attachment(Db, FaxId, AttachmentId),
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
-spec convert_to_tiff(ne_binary(), wh_proplist(), ne_binary()) ->
                             {ne_binary(), ne_binary(), ne_binary()}.
convert_to_tiff(AttachmentBin, Props, _ContentType) ->
    {<<"image/tiff">>, get_file_name(Props, "tiff"), AttachmentBin}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_to_pdf(ne_binary(), wh_proplist(), ne_binary()) ->
                            {ne_binary(), ne_binary(), ne_binary()} |
                            {'error', any()}.
convert_to_pdf(AttachmentBin, Props, <<"application/pdf">>) ->
    {<<"application/pdf">>, get_file_name(Props, "pdf"), AttachmentBin};
convert_to_pdf(AttachmentBin, Props, _ContentType) ->
    TiffFile = tmp_file_name(<<"tiff">>),
    PDFFile = tmp_file_name(<<"pdf">>),
    wh_util:write_file(TiffFile, AttachmentBin),
    ConvertCmd = whapps_config:get_binary(<<"notify.fax">>, <<"tiff_to_pdf_conversion_command">>, ?TIFF_TO_PDF_CMD),
    Cmd = io_lib:format(ConvertCmd, [PDFFile, TiffFile]),
    lager:debug("running command: ~s", [Cmd]),
    _ = os:cmd(Cmd),
    wh_util:delete_file(TiffFile),
    case file:read_file(PDFFile) of
        {'ok', PDFBin} ->
            wh_util:delete_file(PDFFile),
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
    wh_util:to_list(<<"/tmp/", (wh_util:rand_hex_binary(10))/binary, "_notify_fax.", Ext/binary>>).
