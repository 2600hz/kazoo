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

-include("../teletype.hrl").

-define(FAX_CONFIG_CAT, <<(?NOTIFY_CONFIG_CAT)/binary, ".fax">>).

port_options(FromFile, ToFormat) ->
    Args = [FromFile
            ,<<ToFormat/binary, ":-">>
           ],
    ['binary'
     ,'stderr_to_stdout'
     ,'stream'
     ,'eof'
     ,{'args', [wh_util:to_list(Arg) || Arg <- Args]}
    ].

convert(FromFormat, FromFormat, Bin) ->
    {'ok', Bin};
convert(FromFormat0, ToFormat0, Bin) ->
    OldFlag = process_flag('trap_exit', 'true'),

    FromFormat = valid_format(FromFormat0),
    ToFormat = valid_format(ToFormat0),

    FromFile = save_filename(FromFormat, Bin),

    PortOptions = port_options(FromFile, ToFormat),

    Response =
        try open_port({'spawn_executable', "/usr/bin/convert"}, PortOptions) of
            Port -> convert(Port, Bin)
        catch
            _E:_R ->
                lager:debug("failed to open port with '~p': ~s: ~p", [PortOptions, _E, _R]),
                {'error', 'port_failure'}
        end,
    process_flag('trap_exit', OldFlag),
    'ok' = file:delete(FromFile),
    Response.

save_filename(Ext, Bin) ->
    Filename = <<"/tmp/", (wh_util:rand_hex_binary(4))/binary, ".", Ext/binary>>,
    'ok' = file:write_file(Filename, Bin),
    Filename.

valid_format(<<"tiff">>) -> <<"tif">>;
valid_format(Format) -> Format.

convert(Port, Bin) ->
    try erlang:port_command(Port, Bin) of
        'true' -> wait_for_results(Port)
    catch
        _E:_R ->
            lager:debug("failed to send data to port: ~s: ~p", [_E, _R]),
            catch erlang:port_close(Port),
            {'error', 'command_failure'}
    end.

wait_for_results(Port) ->
    wait_for_results(Port, []).
wait_for_results(Port, Acc) ->
    receive
        {Port, {'data', Msg}} ->
            wait_for_results(Port, [Acc | [Msg]]);
        {Port, {'exit_status', 0}} ->
            lager:debug("port exited successfully"),
            {'ok', iolist_to_binary(Acc)};
        {Port, 'eof'} ->
            lager:debug("recv EOF from port"),
            {'ok', iolist_to_binary(Acc)};
        {Port, {'exit_status', _Status}} ->
            lager:debug("port exit status: ~p", [_Status]),
            {'error', iolist_to_binary(Acc)};
        {'EXIT', Port, 'epipe'} ->
            {'ok', iolist_to_binary(Acc)};
        {'EXIT', Port, Reason} ->
            lager:debug("port exited: ~p", [Reason]),
            {'error', iolist_to_binary(Acc)};
        _Msg ->
            lager:debug("recv msg ~p", [_Msg]),
            wait_for_results(Port, Acc)
    after
        ?MILLISECONDS_IN_MINUTE ->
            lager:debug("port timed out: ~p", [Acc]),
            lager:debug("port info: ~p", [erlang:port_info(Port)]),
            catch erlang:port_close(Port),
            {'error', iolist_to_binary(Acc)}
    end.

-spec get_fax_doc(wh_json:object()) -> wh_json:object().
-spec get_fax_doc(wh_json:object(), boolean()) -> wh_json:object().
get_fax_doc(DataJObj) ->
    get_fax_doc(DataJObj, teletype_util:is_preview(DataJObj)).

get_fax_doc(DataJObj, 'true') ->
    FaxId = wh_json:get_value(<<"fax_id">>, DataJObj),
    case teletype_util:open_doc(<<"fax">>, FaxId, DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> wh_json:new()
    end;
get_fax_doc(DataJObj, 'false') ->
    FaxId = wh_json:get_value(<<"fax_id">>, DataJObj),
    case teletype_util:open_doc(<<"fax">>, FaxId, DataJObj) of
        {'ok', JObj} -> JObj;
        {'error', _E} -> get_fax_doc_from_modb(DataJObj, FaxId)
    end.

-spec get_fax_doc_from_modb(wh_json:object(), ne_binary()) -> wh_json:object().
get_fax_doc_from_modb(DataJObj, FaxId) ->
    AccountId = teletype_util:find_account_id(DataJObj),
    case kazoo_modb:open_doc(AccountId, FaxId) of
        {'ok', FaxJObj} -> FaxJObj;
        {'error', _E} ->
            lager:debug("failed to find fax ~s: ~p", [FaxId, _E]),
            teletype_util:send_update(DataJObj, <<"failed">>, <<"Fax-ID was invalid">>),
            throw({'error', 'no_fax_id'})
    end.

-spec get_attachments(wh_json:object(), wh_proplist()) -> attachments().
-spec maybe_get_attachments(wh_json:object(), wh_proplist(), boolean()) -> attachments().
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

-spec maybe_convert_attachment(wh_proplist(), ne_binary(), binary()) -> attachments().
maybe_convert_attachment(Macros, ContentType, Bin) ->
    ToFormat = whapps_config:get(?FAX_CONFIG_CAT, <<"attachment_format">>, <<"pdf">>),
    FromFormat = from_format_from_content_type(ContentType),
    lager:debug("converting from ~s to ~s", [FromFormat, ToFormat]),

    case teletype_fax_util:convert(FromFormat, ToFormat, Bin) of
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

-spec get_file_name(wh_proplist(), ne_binary()) -> ne_binary().
get_file_name(Macros, Ext) ->
    CallerIdMacros = props:get_value(<<"caller_id">>, Macros),
    CallerID =
        case {props:get_value(<<"name">>, CallerIdMacros)
              ,props:get_value(<<"number">>, CallerIdMacros)
             }
        of
            {'undefined', 'undefined'} -> <<"Unknown">>;
            {'undefined', Num} -> wh_util:to_binary(Num);
            {Name, _} -> wh_util:to_binary(Name)
        end,
    LocalDateTime = props:get_value([<<"date_called">>, <<"local">>], Macros, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", wh_util:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(wh_util:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

-spec get_attachment_binary(ne_binary(), api_binary()) ->
                                   {'ok', ne_binary(), binary()} |
                                   {'error', 'no_attachment'}.
get_attachment_binary(Db, Id) ->
    case couch_mgr:open_cache_doc(Db, Id) of
        {'error', 'not_found'} when Db =/= ?WH_FAXES_DB ->
            get_attachment_binary(?WH_FAXES_DB, Id);
        {'error', 'not_found'} ->
            lager:debug("no attachment binary to send"),
            {'error', 'no_attachment'};
        {'ok', FaxJObj} ->
            case wh_doc:attachment(FaxJObj) of
                'undefined' ->
                    {'error', 'no_attachment'};
                _AttachmentJObj ->
                    get_attachment_binary(Db, Id, FaxJObj)
            end
    end.

-spec get_attachment_binary(ne_binary(), ne_binary(), wh_json:object()) ->
                                   {'ok', ne_binary(), binary()} |
                                   {'error', 'no_attachment'}.
get_attachment_binary(Db, Id, FaxJObj) ->
    [AttachmentName] = wh_doc:attachment_names(FaxJObj),

    case couch_mgr:fetch_attachment(Db, Id, AttachmentName) of
        {'ok', Bin} ->
            get_attachment(wh_doc:attachment_content_type(FaxJObj, AttachmentName), Bin);
        {'error', _E} ->
            lager:debug("failed to fetch attachment ~s: ~p", [AttachmentName, _E]),
            {'error', 'no_attachment'}
    end.

-spec get_attachment(api_binary(), binary()) -> {'ok', ne_binary(), binary()}.
get_attachment('undefined', Bin) ->
    get_attachment_binary(<<"image/tiff">>, Bin);
get_attachment(ContentType, Bin) ->
    {'ok', ContentType, Bin}.

-spec fax_db(wh_json:object()) -> ne_binary().
fax_db(DataJObj) ->
    case teletype_util:find_account_db(DataJObj) of
        'undefined' -> ?WH_FAXES_DB;
        Db -> Db
    end.
