%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kz_attachment).

-export([decode_base64/1]).

-include("kz_documents.hrl").

%%--------------------------------------------------------------------
%% @Public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_base64(ne_binary()) -> {api_binary(), ne_binary()}.
decode_base64(Base64) ->
    case binary:split(Base64, <<",">>) of
        %% http://tools.ietf.org/html/rfc4648
        [Bin] ->
            {'undefined', corrected_base64_decode(Bin)};
        %% http://tools.ietf.org/rfc/rfc2397.txt
        [<<"data:", CT/binary>>, Bin] ->
            {get_content_type(CT), corrected_base64_decode(Bin)};
        [_SplitLeft, _SplitRight] ->
            {'undefined', corrected_base64_decode(Base64)}
    end.

-spec get_content_type(ne_binary()) -> api_binary().
get_content_type(MediaType) ->
    get_content_type(MediaType, binary:part(MediaType, byte_size(MediaType), -6)).

get_content_type(MediaType, <<"base64">>) ->
    get_content_type(binary:part(MediaType, 0, byte_size(MediaType) - 7), 'undefined');
get_content_type(MediaType, _) ->
    case cowboy_http:nonempty_list(MediaType, fun cowboy_http:media_range/2) of
        [{{Type, SubType, _Options}, _, _}] ->
            wh_util:join_binary([Type, SubType], <<"/">>);
        {'error', 'badarg'} ->
            lager:warning("failed to extract content type from ~s", [MediaType]),
            'undefined'
    end.

-spec corrected_base64_decode(ne_binary()) -> ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 3 ->
    base64:decode(<<Base64/binary, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 2 ->
    base64:decode(<<Base64/binary, "==">>);
corrected_base64_decode(Base64) ->
    base64:decode(Base64).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_plain_text_test() ->
    Data = <<"foobar">>,
    Base64Data = base64:encode(Data),
    InlineData = <<"data:text/plain;base64,", Base64Data/binary>>,

    ?assertEqual({'undefined', Data}, decode_base64(Base64Data)),
    ?assertEqual({<<"text/plain">>, Data}, decode_base64(InlineData)).

decode_image_png_test() ->
    Data = <<137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,5,0,0,0,5,8,6,0,0,0,141,
             111,38,229,0,0,0,28,73,68,65,84,8,215,99,248,255,255,63,195,127,6,32,5,195,
             32,18,132,208,49,241,130,88,205,4,0,14,245,53,203,209,142,14,31,0,0,0,0,73,
             69,78,68,174,66,96,130
           >>,
    Base64Data = base64:encode(Data),
    InlineData = <<"data:image/png;base64,", Base64Data/binary>>,

    ?assertEqual({'undefined', Data}, decode_base64(Base64Data)),
    ?assertEqual({<<"image/png">>, Data}, decode_base64(InlineData)).

-endif.
