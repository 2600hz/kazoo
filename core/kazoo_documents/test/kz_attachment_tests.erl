%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Account document
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_attachment_tests).

-include_lib("eunit/include/eunit.hrl").

decode_plain_text_test_() ->
    Data = <<"foobar">>,
    Base64Data = base64:encode(Data),
    InlineData = <<"data:text/plain;base64,", Base64Data/binary>>,

    [?_assertEqual({'undefined', Data}, kz_attachment:decode_base64(Base64Data))
    ,?_assertEqual({<<"text/plain">>, Data}, kz_attachment:decode_base64(InlineData))
    ].

decode_image_png_test_() ->
    Path = filename:join(code:priv_dir('kazoo_fixturedb'), "media_files/tiny-red-dot.png"),
    {'ok', Data} = file:read_file(Path),
    Base64Data = base64:encode(Data),
    InlineData = <<"data:image/png;base64,", Base64Data/binary>>,

    [?_assertEqual({'undefined', Data}, kz_attachment:decode_base64(Base64Data))
    ,?_assertEqual({<<"image/png">>, Data}, kz_attachment:decode_base64(InlineData))
    ].

decode_application_pdf_test_() ->
    Path = filename:join(code:priv_dir('kazoo_fixturedb'), "media_files/i.am.a.pdf"),
    {'ok', Data} = file:read_file(Path),

    Base64Data = base64:encode(Data),
    InlineData = <<"data:application/pdf;base64,", Base64Data/binary>>,

    [?_assertEqual({'undefined', Data}, kz_attachment:decode_base64(Base64Data))
    ,?_assertEqual({<<"application/pdf">>, Data}, kz_attachment:decode_base64(InlineData))
    ].
