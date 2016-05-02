%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_util).

-export([convert/3
         ,get_fax_doc/1
         ,get_attachments/2
        ]).

-include("teletype.hrl").

-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).
-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"">>).

convert(FromFormat, FromFormat, Bin) ->
    {'ok', Bin};
convert(FromFormat0, ToFormat0, Bin) ->
    FromFormat = valid_format(FromFormat0),
    ToFormat = valid_format(ToFormat0),
    Filename = kz_util:rand_hex_binary(8),
    FromFile = <<"/tmp/", Filename/binary, ".", FromFormat/binary>>,
    ToFile = <<"/tmp/", Filename/binary, ".", ToFormat/binary>>,
    'ok' = file:write_file(FromFile, Bin),
    ConvertCmd = kapps_config:get_binary(?FAX_CONFIG_CAT, <<"tiff_to_pdf_conversion_command">>, ?TIFF_TO_PDF_CMD),
    Cmd = io_lib:format(ConvertCmd, [ToFile, FromFile]),
    lager:debug("running conversion command: ~s", [Cmd]),
    Response = case os:cmd(Cmd) of
                   "success" ->
                       case file:read_file(ToFile) of
                           {'ok', PDF} ->
                               lager:debug("convert file ~s to ~s succeeded", [FromFile, ToFile]),
                               {'ok', PDF};
                           {'error', _R}=E ->
                               lager:debug("unable to read converted file ~s : ~p", [ToFile, _R]),
                               E
                       end;
                   Else ->
                       lager:debug("could not convert file ~s : ~p", [FromFile, Else]),
                       {'error', Else}
               end,
    _ = kz_util:delete_file(FromFile),
    _ = kz_util:delete_file(ToFile),
    Response.

valid_format(<<"tiff">>) -> <<"tif">>;
valid_format(Format) -> Format.

-spec get_fax_doc(kz_json:object()) -> kz_json:object().
-spec get_fax_doc(kz_json:object(), boolean()) -> kz_json:object().
get_fax_doc(DataJObj) ->
    get_fax_doc(DataJObj, teletype_util:is_preview(DataJObj)).

get_fax_doc(DataJObj, 'true') ->
    FaxId = kz_json:get_value(<<"fax_id">>, DataJObj),
    case teletype_util:open_doc(<<"fax">>, FaxId, DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> kz_json:new()
    end;
get_fax_doc(DataJObj, 'false') ->
    FaxId = kz_json:get_value(<<"fax_id">>, DataJObj),
    case teletype_util:open_doc(<<"fax">>, FaxId, DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> get_fax_doc_from_modb(DataJObj, FaxId)
    end.

-spec get_fax_doc_from_modb(kz_json:object(), ne_binary()) -> kz_json:object().
get_fax_doc_from_modb(DataJObj, FaxId) ->
    AccountId = teletype_util:find_account_id(DataJObj),
    case kazoo_modb:open_doc(AccountId, {<<"fax">>, FaxId}) of
        {'ok', FaxJObj} -> FaxJObj;
        {'error', _E} ->
            lager:debug("failed to find fax ~s: ~p", [FaxId, _E]),
            teletype_util:send_update(DataJObj, <<"failed">>, <<"Fax-ID was invalid">>),
            throw({'error', 'no_fax_id'})
    end.

-spec get_attachments(kz_json:object(), kz_proplist()) -> attachments().
-spec maybe_get_attachments(kz_json:object(), kz_proplist(), boolean()) -> attachments().
get_attachments(DataJObj, Macros) ->
    maybe_get_attachments(DataJObj, Macros, teletype_util:is_preview(DataJObj)).

maybe_get_attachments(_DataJObj, _Macros, 'true') ->
    lager:debug("this is a preview, no attachments"),
    [];
maybe_get_attachments(DataJObj, Macros, 'false') ->
    FaxMacros = props:get_value(<<"fax">>, Macros),
    FaxId = props:get_first_defined([<<"id">>, <<"fax_jobid">>, <<"fax_id">>], FaxMacros),
    Db = fax_db(DataJObj),
    lager:debug("accessing fax at ~s / ~s", [Db, FaxId]),
    case get_attachment_binary(Db, FaxId) of
        {'error', 'no_attachment'} -> [];
        {'ok', ContentType, Bin} ->
            maybe_convert_attachment(Macros, ContentType, Bin)
    end.

-spec maybe_convert_attachment(kz_proplist(), ne_binary(), binary()) -> attachments().
maybe_convert_attachment(Macros, ContentType, Bin) ->
    ToFormat = kapps_config:get(?FAX_CONFIG_CAT, <<"attachment_format">>, <<"pdf">>),
    FromFormat = from_format_from_content_type(ContentType),
    lager:debug("converting from ~s to ~s", [FromFormat, ToFormat]),

    case ?MODULE:convert(FromFormat, ToFormat, Bin) of
        {'ok', Converted} ->
            Filename = get_file_name(Macros, ToFormat),
            lager:debug("adding attachment as ~s", [Filename]),
            [{content_type_from_extension(Filename), Filename, Converted}];
        {'error', Reason} ->
            lager:debug("error converting atachment with reason : ~p", [Reason]),
            []
    end.


-spec from_format_from_content_type(ne_binary()) -> ne_binary().
from_format_from_content_type(<<"application/pdf">>) ->
    <<"pdf">>;
from_format_from_content_type(<<"image/tiff">>) ->
    <<"tif">>;
from_format_from_content_type(ContentType) ->
    [_Type, SubType] = binary:split(ContentType, <<"/">>),
    SubType.

-spec content_type_from_extension(ne_binary()) -> ne_binary().
content_type_from_extension(Ext) ->
    {Type, SubType, _} = cow_mimetypes:all(Ext),
    <<Type/binary, "/", SubType/binary>>.

-spec get_file_name(kz_proplist(), ne_binary()) -> ne_binary().
get_file_name(Macros, Ext) ->
    CallerIdMacros = props:get_value(<<"caller_id">>, Macros),
    CallerID =
        case {props:get_value(<<"name">>, CallerIdMacros)
              ,props:get_value(<<"number">>, CallerIdMacros)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> kz_util:to_binary(Num);
            {Name, _} -> kz_util:to_binary(Name)
        end,
    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", kz_util:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(kz_util:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

-spec get_attachment_binary(ne_binary(), api_binary()) ->
                                   {'ok', ne_binary(), binary()} |
                                   {'error', 'no_attachment'}.
get_attachment_binary(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, {<<"fax">>, Id}) of
        {'error', 'not_found'} when Db =/= ?KZ_FAXES_DB ->
            get_attachment_binary(?KZ_FAXES_DB, Id);
        {'error', 'not_found'} ->
            lager:debug("no attachment binary to send"),
            {'error', 'no_attachment'};
        {'ok', FaxJObj} ->
            case kz_doc:attachment(FaxJObj) of
                'undefined' ->
                    {'error', 'no_attachment'};
                _AttachmentJObj ->
                    get_attachment_binary(Db, Id, FaxJObj)
            end
    end.

-spec get_attachment_binary(ne_binary(), ne_binary(), kz_json:object()) ->
                                   {'ok', ne_binary(), binary()} |
                                   {'error', 'no_attachment'}.
get_attachment_binary(Db, Id, FaxJObj) ->
    [AttachmentName] = kz_doc:attachment_names(FaxJObj),

    case kz_datamgr:fetch_attachment(Db, {<<"fax">>, Id}, AttachmentName) of
        {'ok', Bin} ->
            get_attachment(kz_doc:attachment_content_type(FaxJObj, AttachmentName), Bin);
        {'error', _E} ->
            lager:debug("failed to fetch attachment ~s: ~p", [AttachmentName, _E]),
            {'error', 'no_attachment'}
    end.

-spec get_attachment(api_binary(), binary()) -> {'ok', ne_binary(), binary()}.
get_attachment('undefined', Bin) ->
    get_attachment_binary(<<"image/tiff">>, Bin);
get_attachment(ContentType, Bin) ->
    {'ok', ContentType, Bin}.

-spec fax_db(kz_json:object()) -> ne_binary().
fax_db(DataJObj) ->
    case teletype_util:find_account_db(DataJObj) of
        'undefined' -> ?KZ_FAXES_DB;
        Db -> Db
    end.
