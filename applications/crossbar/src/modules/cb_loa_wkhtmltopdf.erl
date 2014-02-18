%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%% Builds an LOA doc using wkhtmltopdf (https://code.google.com/p/wkhtmltopdf/)
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_loa_wkhtmltopdf).

-export([generate_loa/2]).

-include("../crossbar.hrl").
-define(MY_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".port_requests">>).
-define(HTML_TO_PDF, <<"/usr/bin/wkhtmltopdf --quiet $html$ $pdf$">>).

generate_loa(Context, PortRequest) ->
    AccountId = cb_context:account_id(Context),

    ResellerId = wh_services:find_reseller_id(AccountId),
    ResellerDoc = cb_context:account_doc(cb_context:set_account_id(Context, ResellerId)),

    AccountDoc = cb_context:account_doc(Context),

    Numbers = [wnm_util:pretty_print(N) || N <- wh_json:get_keys(<<"numbers">>, PortRequest)],

    QRCode = create_qr_code(cb_context:account_id(Context), wh_json:get_first_defined([<<"_id">>, <<"id">>], PortRequest)),

    generate_loa_from_template(Context
                               ,props:filter_undefined(
                                  [{<<"reseller">>, wh_json:to_proplist(ResellerDoc)}
                                   ,{<<"account">>, wh_json:to_proplist(AccountDoc)}
                                   ,{<<"numbers">>, Numbers}
                                   ,{<<"bill">>, wh_json:to_proplist(wh_json:get_value(<<"bill">>, PortRequest, wh_json:new()))}
                                   ,{<<"request">>, wh_json:to_proplist(PortRequest)}
                                   ,{<<"qr_code">>, QRCode}
                                  ])
                               ,ResellerId
                               ,wh_json:get_value(<<"carrier">>, PortRequest)
                              ).

-spec generate_loa_from_template(cb_context:context(), wh_proplist(), ne_binary(), api_binary()) ->
                                        cb_context:context().
generate_loa_from_template(Context, TemplateData, ResellerId, Carrier) ->
    Template = cb_port_requests:find_template(ResellerId, Carrier),

    Renderer = wh_util:to_atom(<<ResellerId/binary, "_loa">>, 'true'),
    {'ok', Renderer} = erlydtl:compile_template(Template, Renderer),
    {'ok', LOA} = Renderer:render(TemplateData),

    code:purge(Renderer),
    code:delete(Renderer),

    Prefix = <<ResellerId/binary, "-", (cb_context:req_id(Context))/binary>>,
    HTMLFile = filename:join([<<"/tmp">>, <<Prefix/binary, "-loa.html">>]),
    PDFFile = filename:join([<<"/tmp">>, <<Prefix/binary, "-loa.pdf">>]),

    'ok' = file:write_file(HTMLFile, LOA),

    RawCmd = whapps_config:get(?MY_CONFIG_CAT, <<"html2pdf">>, ?HTML_TO_PDF),

    Cmd = lists:foldl(fun cmd_fold/2
                      ,RawCmd
                      ,[{<<"$pdf$">>, PDFFile}
                        ,{<<"$html$">>, HTMLFile}
                       ]
                     ),
    lager:debug("exec ~s", [Cmd]),
    case os:cmd(wh_util:to_list(Cmd)) of
        [] ->
            {'ok', PDF} = file:read_file(PDFFile),
            cb_context:set_resp_status(
              cb_context:set_resp_data(Context, PDF)
              ,'success'
             );
        _E ->
            lager:debug("failed to exec ~s: ~s", [Cmd, _E]),
            cb_context:set_resp_status(Context, 'error')
    end.

-spec cmd_fold({ne_binary(), ne_binary()}, ne_binary()) -> ne_binary().
cmd_fold({Search, Replace}, Subject) ->
    binary:replace(Subject, Search, Replace).

-spec create_qr_code(api_binary(), api_binary()) -> wh_proplist() | 'undefined'.
create_qr_code('undefined', _) -> 'undefined';
create_qr_code(_, 'undefined') -> 'undefined';
create_qr_code(AccountId, PortRequestId) ->
    lager:debug("create qr code for ~s - ~s", [AccountId, PortRequestId]),
    CHL = <<AccountId/binary, "-", PortRequestId/binary>>,
    Url = <<"https://chart.googleapis.com/chart?chs=300x300&cht=qr&chl=", CHL/binary, "&choe=UTF-8">>,

    case ibrowse:send_req(wh_util:to_list(Url)
                          ,[]
                          ,'get'
                          ,[]
                          ,[{'response', 'binary'}]
                         )
    of
        {'ok', "200", _RespHeaders, RespBody} ->
            lager:debug("generated QR code from ~s: ~s", [Url, RespBody]),
            [{<<"image">>, base64:encode(RespBody)}];
        _E ->
            lager:debug("failed to generate QR code: ~p", [_E]),
            'undefined'
    end.
