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
