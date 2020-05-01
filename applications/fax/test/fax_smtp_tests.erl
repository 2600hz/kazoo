%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(fax_smtp_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FROM, <<"kazoo@2600hz.com">>).
-define(DATE, <<"Fri, 21 Feb 2020 11:06:41 -0600">>).
-define(SUBJECT, <<"Test attachment unpacking and decoding">>).

-define(TEXT, <<"Please find the attached PDF">>).

decode_email_test() ->
    BaseDir = code:lib_dir('fax', 'test'),
    {'ok', PDF} = file:read_file(filename:join([BaseDir, "pdf.pdf"])),
    Encoded = base64:encode(PDF),

    Boundary = kz_binary:rand_hex(4),

    Email = create_email(Encoded, Boundary),

    {T, S, Hs, Ps, Body} = fax_smtp:decode_data(Email),

    ?assertEqual(<<"multipart">>, T),
    ?assertEqual(<<"mixed">>, S),

    ?assertEqual(?FROM, props:get_value(<<"From">>, Hs)),
    ?assertEqual(?DATE, props:get_value(<<"Date">>, Hs)),
    ?assertEqual(?SUBJECT, props:get_value(<<"Subject">>, Hs)),

    ?assertEqual(Boundary, props:get_value([<<"content-type-params">>, <<"boundary">>], Ps)),

    ?assertEqual(2, length(Body)),

    [{PTType, PTSubType
     ,_PTHeaders, _PTParameters
     ,PTBody
     }
    ,{PDFType, PDFSubType
     ,_PDFHeaders, _PDFParameters
     ,PDFBody
     }
    ] = Body,

    ?assertEqual(<<"text">>, PTType),
    ?assertEqual(<<"plain">>, PTSubType),
    ?assertEqual(?TEXT, PTBody),

    ?assertEqual(<<"application">>, PDFType),
    ?assertEqual(<<"pdf">>, PDFSubType),
    ?assertEqual(PDF, PDFBody).

create_email(Attachment, Boundary) ->
    Email = ["From: ", ?FROM, "\r\n"
            ,"Date: ", ?DATE, "\r\n"
            ,"Subject: ", ?SUBJECT, "\r\n"
            ,"Content-type: multipart/mixed; boundary=\"", Boundary, "\"\r\n"
            ,"\r\n"
            ,"--", Boundary, "\r\n"
            ,"Content-Type: text/plain; charset=utf-8\r\n"
            ,"Content-Disposition: inline\r\n"
            ,"\r\n"
            ,?TEXT, "\r\n"
            ,"--", Boundary, "\r\n"
            ,"Content-Type: application/pdf; charset=utf-8\r\n"
            ,"Content-Disposition: attachment; filename=\"pdf.pdf\"\r\n"
            ,"Content-Transfer-Encoding: base64\r\n"
            ,"\r\n"
            ,Attachment, "\r\n\r\n"
            ,"--", Boundary, "--\r\n"
            ],
    iolist_to_binary(Email).
