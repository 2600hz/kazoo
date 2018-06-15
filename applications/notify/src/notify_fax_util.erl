%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_fax_util).

-export([get_attachment/2, get_attachment/3]).

-include("notify.hrl").

-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"">>).


%%------------------------------------------------------------------------------
%% @doc create a friendly file name
%% @end
%%------------------------------------------------------------------------------
-spec get_file_name(kz_term:proplist(), string()) -> kz_term:ne_binary().
get_file_name(Props, Ext) ->
    Fax = props:get_value(<<"fax">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Fax), props:get_value(<<"caller_id_number">>, Fax)} of
                   {'undefined', 'undefined'} -> <<"Unknown">>;
                   {'undefined', Num} -> kz_term:to_binary(Num);
                   {Name, _} -> kz_term:to_binary(Name)
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Fax, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", kz_time:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(kz_term:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_attachment(kz_term:ne_binary(), kz_term:proplist()) ->
                            {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()} |
                            {'error', any()}.
get_attachment(Category, Props) ->
    UseDb = props:get_value(<<"account_db">>, Props, ?KZ_FAXES_DB),
    get_attachment(UseDb, Category, Props).

-spec get_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                            {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()} |
                            {'error', any()}.
get_attachment(UseDb, Category, Props) ->
    Fax   = props:get_value(<<"fax">>, Props),
    FaxId = props:get_first_defined([<<"fax_jobid">>, <<"fax_id">>], Fax),

    {'ok', AttachmentBin, ContentType} = raw_attachment_binary(UseDb, FaxId),

    case kapps_config:get_binary(Category, <<"attachment_format">>, <<"pdf">>) of
        <<"pdf">> ->
            case kz_convert:fax(ContentType, <<"application/pdf">>, AttachmentBin, [{<<"output_type">>, 'binary'}]) of
                {'ok', Content} -> {ContentType, get_file_name(Props, "pdf"), Content};
                {'error', _ } -> 'error'
            end;
        _Else -> {ContentType, get_file_name(Props, "pdf"), AttachmentBin}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec raw_attachment_binary(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_term:ne_binary(), kz_term:ne_binary()}.
raw_attachment_binary(Db, FaxId) ->
    raw_attachment_binary(Db, FaxId, 2).

-spec raw_attachment_binary(kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) ->
                                   {'ok', kz_term:ne_binary(), kz_term:ne_binary()}.
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

