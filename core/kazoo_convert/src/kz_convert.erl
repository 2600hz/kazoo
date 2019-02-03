%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_convert).

-export([audio/3, audio/4
        ,fax/3, fax/4
        ]).

-include_lib("kazoo_convert/include/kz_convert.hrl").

-type content() :: binary()|{'file', filename:name()}.
-export_type([content/0]).

%% @equiv audio(FromFormat, ToFormat, Content, [])
-spec audio(kz_term:api_ne_binary(), kz_term:api_ne_binary(), content()) ->
                   gen_kz_converter:converted().
audio(FromFormat, ToFormat, Content) ->
    audio(FromFormat, ToFormat, Content, []).

%%------------------------------------------------------------------------------
%% @doc A uniform interface for conversion of auido related files.
%%
%% The configured converter module is loaded from system_config/kazoo_convert via
%% the parameter `audio_converter'. The default audio_converter is `kz_audio_converter'. The
%% behaviour for converter modules is defined in `gen_kz_converter'.
%%
%% Arguments Description:
%% <ul>
%% <li><strong>From:</strong> is a mimetype binary that specifies the format of
%% the Content passed in to convert.</li>
%% <li><strong>To:</strong> is a mimetype binary that specifies the format the
%% Content is to be converted.</li>
%% <li><strong>Content:</strong> content can be file path to the source file or
%% a binary containing the contents of the file to be converted.</li>
%% <li><strong>Options:</strong> a proplist of options for the underlying fax_converter.</li>
%% </ul>
%%
%% @end
%%------------------------------------------------------------------------------
-spec audio(kz_term:api_binary(), kz_term:api_binary(), content(), kz_term:proplist()) ->
                   gen_kz_converter:converted().
audio('undefined', _ToFormat, _Content, _Options) ->
    {'error', <<"undefined from format">>};
audio(_FromFormat, 'undefined', _Content, _Options) ->
    {'error', <<"undefined to format">>};
audio(_FromFormat, _ToFormat, <<>>, _Options) ->
    {'error', <<"empty content">>};
audio(_FromFormat, _ToFormat, {'file', <<>>}, _Options) ->
    {'error', <<"empty filename">>};
audio(FromFormat, ToFormat, Content, Options) ->
    Conversion = kapps_config:get_ne_binary(?CONFIG_CAT, <<"audio_converter">>, <<"audio_converter">>),
    Module = convert_to_module(Conversion),
    Module:convert(FromFormat, ToFormat, Content, props:insert_value(<<"tmp_dir">>, ?TMP_DIR, Options)).

%% @equiv fax(FromFormat, ToFormat, Content, [])
-spec fax(kz_term:api_ne_binary(), kz_term:api_ne_binary(), content()) ->
                 gen_kz_converter:converted().
fax(FromFormat, ToFormat, Content) ->
    fax(FromFormat, ToFormat, Content, []).

%%------------------------------------------------------------------------------
%% @doc A uniform interface for conversion of fax related files.
%%
%% The configured converter module is loaded from system_config/kazoo_convert via
%% the parameter `fax_converter'. The default fax_converter is `kz_fax_converter'. The
%% behaviour for converter modules is defined in `gen_kz_converter'.
%%
%% Arguments Description:
%% <ul>
%% <li><strong>From:</strong> is a mimetype binary that specifies the format of
%% the Content passed in to convert.</li>
%% <li><strong>To:</strong> is a mimetype binary that specifies the format the
%% Content is to be converted.</li>
%% <li><strong>Content:</strong> content can be file path to the source file or
%% a binary containing the contents of the file to be converted.</li>
%% <li><strong>Options:</strong> a proplist of options for the underlying fax_converter.</li>
%% </ul>
%%
%% @end
%%------------------------------------------------------------------------------
-spec fax(kz_term:api_binary(), kz_term:api_binary(), content(), kz_term:proplist()) ->
                 gen_kz_converter:converted().
fax('undefined', _ToFormat, _Content, _Options) ->
    {'error', <<"undefined from format">>};
fax(_FromFormat, 'undefined', _Content, _Options) ->
    {'error', <<"undefined to format">>};
fax(_FromFormat, _ToFormat, <<>>, _Options) ->
    {'error', <<"empty content">>};
fax(_FromFormat, _ToFormat, {'file', <<>>}, _Options) ->
    {'error', <<"empty filename">>};
fax(FromFormat, ToFormat, Content, Options) ->
    Conversion = kapps_config:get_ne_binary(?CONFIG_CAT, <<"fax_converter">>, <<"fax_converter">>),
    Module = convert_to_module(Conversion),
    Module:convert(FromFormat, ToFormat, Content, props:insert_value(<<"tmp_dir">>, ?TMP_DIR, Options)).

-spec convert_to_module(kz_term:ne_binary()) -> atom().
convert_to_module(Conversion) ->
    kz_term:to_atom(<<"kz_", Conversion/binary>>, 'true').
