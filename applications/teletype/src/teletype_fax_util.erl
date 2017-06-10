%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_util).

-export([add_data/1, maybe_add_document_data/2
        ,convert/3
        ,get_fax_doc/1, maybe_get_fax_doc/1
        ,get_attachments/2
        ,to_email_addresses/2
        ]).

-include("teletype.hrl").

-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).
-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"">>).

-spec add_data(kz_json:object()) -> kz_json:object().
add_data(DataJObj) ->
    FaxBoxJObj = get_faxbox_doc(DataJObj),
    Values =
        props:filter_empty(
          [{<<"error">>, error_data(DataJObj)}
          ,{<<"faxbox">>, FaxBoxJObj}
          ,{<<"owner">>, get_owner_doc(DataJObj, FaxBoxJObj)}
          ,{<<"fax">>, kz_doc:public_fields(maybe_get_fax_doc(DataJObj))}
          ]),
    kz_json:set_values(Values, DataJObj).

-spec maybe_add_document_data(kz_proplist(), attachments()) -> kz_proplist().
maybe_add_document_data(Macros, []) -> Macros;
maybe_add_document_data(Macros, [{ContentType, Filename, Bin}]) ->
    Values =
        props:filter_undefined(
          [{<<"media">>, Filename}
          ,{<<"document_type">>, kz_mime:to_extension(ContentType)}
          ,{<<"document_size">>, erlang:size(Bin)}
          ]),
    Fax = props:set_values(Values, props:get_value(<<"fax">>, Macros, [])),
    props:set_value(<<"fax">>, Fax, Macros).


-spec convert(ne_binary(), ne_binary(), binary()) -> {ok, binary()} |
                                                     {error, atom() | string()}.
convert(FromFormat, FromFormat, Bin) ->
    {'ok', Bin};
convert(FromFormat0, ToFormat0, Bin) ->
    lager:debug("converting from ~s to ~s", [FromFormat0, ToFormat0]),
    FromFormat = valid_format(FromFormat0),
    ToFormat = valid_format(ToFormat0),
    Filename = kz_binary:rand_hex(8),
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
    case teletype_util:open_doc(<<"fax">>, 'undefined', DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> kz_json:new()
    end;
get_fax_doc(DataJObj, 'false') ->
    FaxId     = kz_json:get_value(<<"fax_id">>, DataJObj),
    AccountDb = kapi_notifications:account_db(DataJObj),

    case kz_datamgr:open_cache_doc(AccountDb, {kzd_fax:type(), FaxId}) of
        {'ok', JObj} ->
            JObj;
        {'error', _E} ->
            lager:debug("failed to find fax ~s: ~p", [FaxId, _E]),
            teletype_util:send_update(DataJObj, <<"failed">>, <<"Fax-ID was invalid">>),
            throw({'error', 'no_fax_id'})
    end.

-spec maybe_get_fax_doc(kz_json:object()) -> kz_json:object().
maybe_get_fax_doc(DataJObj) ->
    try get_fax_doc(DataJObj) catch _:_ -> kz_json:new() end.

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
    ToFormat = kapps_config:get_ne_binary(?FAX_CONFIG_CAT, <<"attachment_format">>, <<"pdf">>),
    FromFormat = from_format_from_content_type(ContentType),

    case convert(FromFormat, ToFormat, Bin) of
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
            {'undefined', Num} -> kz_term:to_binary(Num);
            {Name, _} -> kz_term:to_binary(Name)
        end,
    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", kz_time:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(kz_term:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

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
    case kapi_notifications:account_db(DataJObj) of
        'undefined' -> ?KZ_FAXES_DB;
        Db -> Db
    end.

-spec to_email_addresses(kz_json:object(), ne_binary()) -> api_binaries().
to_email_addresses(DataJObj, ModConfigCat) ->
    to_email_addresses(DataJObj
                      ,ModConfigCat
                      ,kz_json:get_first_defined([[<<"to">>, <<"email_addresses">>]
                                                 ,[<<"fax">>, <<"email">>, <<"send_to">>]
                                                 ,[<<"fax">>, <<"notifications">>, <<"email">>, <<"send_to">>]
                                                 ,[<<"fax_notifications">>, <<"email">>, <<"send_to">>]
                                                 ,[<<"notifications">>, <<"email">>, <<"send_to">>]
                                                 ,[<<"owner">>, <<"email">>]
                                                 ,[<<"owner">>, <<"username">>]
                                                 ]
                                                ,DataJObj
                                                )
                      ).

-spec to_email_addresses(kz_json:object(), ne_binary(), ne_binary() | api_binaries()) -> api_binaries().
to_email_addresses(_DataJObj, _ModConfigCat, <<_/binary>> = Email) ->
    [Email];
to_email_addresses(_DataJObj, _ModConfigCat, [_|_] = Emails) ->
    Emails;
to_email_addresses(DataJObj, ModConfigCat, _) ->
    case teletype_util:find_account_rep_email(DataJObj) of
        'undefined' ->
            lager:debug("failed to find account rep email, using defaults"),
            default_to_addresses(ModConfigCat);
        Emails ->
            lager:debug("using ~p for To", [Emails]),
            Emails
    end.

-spec default_to_addresses(ne_binary()) -> api_binaries().
default_to_addresses(ModConfigCat) ->
    case kapps_config:get_ne_binary_or_ne_binaries(ModConfigCat, <<"default_to">>) of
        'undefined' -> 'undefined';
        <<_/binary>> = Email -> [Email];
        [_|_]=Emails -> Emails
    end.

-spec get_faxbox_doc(kz_json:object()) -> kz_json:object().
get_faxbox_doc(DataJObj) ->
    case teletype_util:open_doc(<<"faxbox">>, kz_json:get_value(<<"faxbox_id">>, DataJObj), DataJObj) of
        {'ok', J} -> J;
        {'error', _} -> kz_json:new()
    end.

-spec get_owner_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
get_owner_doc(DataJObj, FaxBoxJObj) ->
    OwnerId = kzd_fax_box:owner_id(FaxBoxJObj, kz_json:get_value(<<"owner_id">>, DataJObj)),
    case teletype_util:open_doc(<<"user">>, OwnerId, DataJObj) of
        {'ok', J} -> J;
        {'error', _} -> kz_json:new()
    end.

-spec error_data(kz_json:object()) -> kz_json:object().
error_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:from_list(
              [{<<"call_info">>, kz_json:get_value(<<"fax_error">>, DataJObj)}
              ,{<<"fax_info">>, kz_json:get_value([<<"fax_info">>, <<"fax_result_text">>], DataJObj)}
              ]);
        'true'->
            kz_json:from_list(
              [{<<"call_info">>, <<"CALL_INFO">>}
              ,{<<"fax_info">>, <<"FAX_INFO">>}
              ])
    end.
