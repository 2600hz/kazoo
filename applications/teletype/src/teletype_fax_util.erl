%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_fax_util).

-export([add_data/1
        ,add_attachments/3
        ,to_email_addresses/2
        ]).

-include("teletype.hrl").

-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).
-define(TIFF_TO_PDF_CMD, <<"tiff2pdf -o ~s ~s &> /dev/null && echo -n \"success\"">>).

-spec add_data(kz_json:object()) -> kz_json:object().
add_data(DataJObj) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    FaxDoc = maybe_get_fax_doc(DataJObj, IsPreview),
    FaxBoxJObj = get_faxbox_doc(DataJObj, FaxDoc),
    Values =
        props:filter_empty(
          [{<<"error">>, error_data(DataJObj, IsPreview)}
          ,{<<"faxbox">>, FaxBoxJObj}
          ,{<<"owner">>, get_owner_doc(DataJObj, FaxBoxJObj)}
          ,{<<"fax_doc">>, FaxDoc}
          ,{<<"timezone">>, find_timezone(DataJObj, FaxBoxJObj)}
          ]),
    kz_json:set_values(Values, DataJObj).

-spec add_attachments(kz_json:object(), kz_proplist(), boolean()) -> {kz_proplist(), attachments()}.
add_attachments(DataJObj, Macros, ShouldTerminate) ->
    IsPreview = teletype_util:is_preview(DataJObj),
    FaxDoc = kz_json:get_value(<<"fax_doc">>, DataJObj),
    case kz_json:is_json_object(FaxDoc)
        andalso not kz_json:is_empty(FaxDoc)
        andalso maybe_fetch_attachments(FaxDoc, Macros, IsPreview)
    of
        'false' -> maybe_terminate(Macros, ShouldTerminate, IsPreview);
        [] -> maybe_terminate(Macros, ShouldTerminate, IsPreview);
        Attachments -> {add_document_data(FaxDoc, Macros, Attachments), Attachments}
    end.

-spec maybe_terminate(kz_proplist(), boolean(), boolean()) -> {kz_proplist(), attachments()}.
maybe_terminate(Macros, _, 'true') ->
    lager:debug("this is a preview, no attachments"),
    {Macros, []};
maybe_terminate(_, 'true', 'false') ->
    lager:debug("No attachments were found for this fax"),
    throw({'error', 'no_attachment'});
maybe_terminate(Macros, 'false', 'false') ->
    lager:debug("No attachments were found for this fax"),
    {Macros, []}.

-spec add_document_data(kz_json:object(), kz_proplist(), attachments()) -> kz_proplist().
add_document_data(FaxDoc, Macros, [{ContentType, Filename, Bin}]) ->
    FaxDocProps = kz_json:to_proplist(kz_doc:public_fields(FaxDoc)),
    Values =
        props:filter_undefined(
          [{<<"media">>, Filename}
          ,{<<"document_type">>, kz_mime:to_extension(ContentType)}
          ,{<<"document_size">>, erlang:size(Bin)}
           | FaxDocProps
          ]),
    FaxMacros = props:set_values(Values, props:get_value(<<"fax">>, Macros, [])),
    props:set_value(<<"fax">>, FaxMacros, Macros).

-spec to_email_addresses(kz_json:object(), ne_binary()) -> api_binaries().
to_email_addresses(DataJObj, ModConfigCat) ->
    Paths = [[<<"to">>, <<"email_addresses">>]
            ,[<<"fax">>, <<"email">>, <<"send_to">>]
            ,[<<"fax">>, <<"notifications">>, <<"email">>, <<"send_to">>]
            ,[<<"fax_notifications">>, <<"email">>, <<"send_to">>]
            ,[<<"notifications">>, <<"email">>, <<"send_to">>]
            ,[<<"owner">>, <<"email">>]
            ,[<<"owner">>, <<"username">>]
            ],
    Emails = kz_json:get_first_defined(Paths, DataJObj),
    to_email_addresses(DataJObj, ModConfigCat, Emails).

-spec to_email_addresses(kz_json:object(), ne_binary(), ne_binary() | api_binaries()) -> api_binaries().
to_email_addresses(_, _, ?NE_BINARY=Email) ->
    [Email];
to_email_addresses(_, _, Emails)
  when is_list(Emails)
       andalso length(Emails) >= 1 ->
    Emails;
to_email_addresses(DataJObj, ModConfigCat, _) ->
    Emails = teletype_util:find_account_rep_email(DataJObj),
    maybe_using_default_to_addresses(Emails, ModConfigCat).

-spec maybe_using_default_to_addresses(api_binaries(), ne_binary()) -> api_binaries().
maybe_using_default_to_addresses('undefined', ModConfigCat) ->
    lager:debug("failed to find account rep email, using defaults"),
    case kapps_config:get_ne_binary_or_ne_binaries(ModConfigCat, <<"default_to">>) of
        'undefined' -> 'undefined';
        ?NE_BINARY=Email -> [Email];
        Emails when is_list(Emails) -> Emails
    end;
maybe_using_default_to_addresses(Emails, _) ->
    lager:debug("using ~p for To", [Emails]),
    Emails.

%%%===================================================================
%%% Build data functions
%%%===================================================================

-spec get_faxbox_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
get_faxbox_doc(DataJObj, FaxDoc) ->
    BoxId = kz_json:find(<<"faxbox_id">>, [DataJObj, FaxDoc]),
    case kz_term:is_ne_binary(BoxId)
        andalso teletype_util:open_doc(<<"faxbox">>, BoxId, DataJObj)
    of
        'false' -> kz_json:new();
        {'ok', J} -> J;
        {'error', _} -> kz_json:new()
    end.

-spec error_data(kz_json:object(), boolean()) -> kz_json:object().
error_data(DataJObj, 'false') ->
    kz_json:from_list(
      [{<<"call_info">>, kz_json:get_value(<<"fax_error">>, DataJObj)}
      ,{<<"fax_info">>, kz_json:get_value([<<"fax_info">>, <<"fax_result_text">>], DataJObj)}
      ]);
error_data(_, 'true') ->
    kz_json:from_list(
      [{<<"call_info">>, <<"CALL_INFO">>}
      ,{<<"fax_info">>, <<"FAX_INFO">>}
      ]).

-spec get_owner_doc(kz_json:object(), kz_json:object()) -> kz_json:object().
get_owner_doc(DataJObj, FaxBoxJObj) ->
    OwnerId = kzd_fax_box:owner_id(FaxBoxJObj, kz_json:get_value(<<"owner_id">>, DataJObj)),
    case teletype_util:open_doc(<<"user">>, OwnerId, DataJObj) of
        {'ok', J} -> J;
        {'error', _} -> kz_json:new()
    end.

-spec maybe_get_fax_doc(kz_json:object(), boolean()) -> kz_json:object().
maybe_get_fax_doc(DataJObj, 'true') ->
    case teletype_util:open_doc(<<"fax">>, 'undefined', DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> kz_json:new()
    end;
maybe_get_fax_doc(DataJObj, 'false') ->
    FaxId = kz_json:get_ne_binary_value(<<"fax_id">>, DataJObj),
    case get_fax_doc(fax_db(DataJObj, FaxId), FaxId) of
        {'ok', JObj} -> JObj;
        {'error', _} -> kz_json:new()
    end.

-spec get_fax_doc(ne_binary(), api_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
get_fax_doc(_, 'undefined') ->
    lager:debug("undefined fax_id"),
    {'error', 'not_found'};
get_fax_doc(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, {kzd_fax:type(), Id}) of
        {'ok', _}=OK -> OK;
        {'error', 'not_found'} when Db =/= ?KZ_FAXES_DB ->
            get_fax_doc(?KZ_FAXES_DB, Id);
        {'error', _Reason}=Error ->
            lager:debug("failed to open fax ~s/~s document: ~p", [Db, Id, _Reason]),
            Error
    end.

-spec find_timezone(api_ne_binary() | kz_json:object(), kz_json:object()) -> ne_binary().
find_timezone('undefined', FaxBoxJObj) ->
    kzd_fax_box:timezone(FaxBoxJObj);
find_timezone(Timezone, _FaxBoxJObj) when is_binary(Timezone) ->
    Timezone;
find_timezone(DataJObj, FaxBoxJObj) ->
    Paths = [<<"fax_timezone">>
            ,[<<"fax_info">>, <<"fax_timezone">>]
            ,[<<"fax">>, <<"tx_result">>, <<"timezone">>]
            ,[<<"fax">>, <<"rx_result">>, <<"timezone">>]
            ],
    find_timezone(kz_json:get_first_defined(Paths, DataJObj), FaxBoxJObj).

%%%===================================================================
%%% Attachment Utilites
%%%===================================================================

-spec fax_db(kz_json:object(), api_ne_binary()) -> ne_binary().
fax_db(DataJObj, FaxId) ->
    case kapi_notifications:account_db(DataJObj) of
        'undefined' ->
            maybe_get_fax_db_from_id(kz_json:get_ne_binary_value(<<"account_id">>, DataJObj), FaxId);
        Db -> maybe_get_fax_db_from_id(Db, FaxId)
    end.

-spec maybe_get_fax_db_from_id(api_ne_binary(), api_ne_binary()) -> ne_binary().
maybe_get_fax_db_from_id('undefined', _) -> ?KZ_FAXES_DB;
maybe_get_fax_db_from_id(?MATCH_MODB_SUFFIX_ENCODED(_, _, _)=Db, _) -> Db;
maybe_get_fax_db_from_id(Db, ?MATCH_MODB_PREFIX(Year, Month, _)) -> kazoo_modb:get_modb(kz_util:format_account_id(Db), Year, Month);
maybe_get_fax_db_from_id(Db, _) -> Db.

-spec maybe_fetch_attachments(kz_json:object(), kz_proplist(), boolean()) -> attachments().
maybe_fetch_attachments(_, _, 'true') ->
    [];
maybe_fetch_attachments(FaxJObj, Macros, 'false') ->
    FaxId = kz_doc:id(FaxJObj),
    Db = kz_doc:account_db(FaxJObj),
    [AttachmentName] = kz_doc:attachment_names(FaxJObj),

    lager:debug("accessing fax attachment ~s at ~s / ~s", [AttachmentName, Db, FaxId]),

    case kz_datamgr:fetch_attachment(Db, {kzd_fax:type(), FaxId}, AttachmentName) of
        {'ok', Bin} ->
            ContentType = kz_doc:attachment_content_type(FaxJObj, AttachmentName),
            maybe_convert_attachment(Macros, ContentType, Bin);
        {'error', _E} ->
            lager:debug("failed to fetch attachment ~s: ~p", [AttachmentName, _E]),
            []
    end.

-spec maybe_convert_attachment(kz_proplist(), api_binary(), binary()) -> attachments().
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
from_format_from_content_type(<<"application/pdf">>) -> <<"pdf">>;
from_format_from_content_type(<<"image/tiff">>) -> <<"tif">>;
from_format_from_content_type('undefined') -> <<"tif">>;
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

-spec convert(ne_binary(), ne_binary(), binary()) -> {ok, binary()} | {error, atom() | string()}.
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

    Response = run_convert_command(Cmd, FromFile, ToFile),
    _ = kz_util:delete_file(FromFile),
    _ = kz_util:delete_file(ToFile),
    Response.

-spec run_convert_command(string(), ne_binary(), ne_binary()) -> {ok, binary()} | {error, atom() | string()}.
run_convert_command(Cmd, FromFile, ToFile) ->
    lager:debug("running conversion command: ~s", [Cmd]),
    case os:cmd(Cmd) of
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
    end.

-spec valid_format(ne_binary()) -> ne_binary().
valid_format(<<"tiff">>) -> <<"tif">>;
valid_format(Format) -> Format.
