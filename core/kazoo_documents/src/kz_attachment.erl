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
-spec decode_base64(ne_binary()) -> {maybe(binary()), ne_binary()}.
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

-spec get_content_type(ne_binary()) -> maybe(binary()).
get_content_type(MediaType) ->
    get_content_type(MediaType, kz_util:truncate_left_binary(MediaType, 6)).

get_content_type(MediaType, <<"base64">>) ->
    get_content_type(kz_util:truncate_right_binary(MediaType, byte_size(MediaType) - 7), 'undefined');
get_content_type(MediaType, _) ->
    case cowboy_http:nonempty_list(MediaType, fun cowboy_http:media_range/2) of
        [{{Type, SubType, _Options}, _, _}] ->
            kz_util:join_binary([Type, SubType], <<"/">>);
        {'error', 'badarg'} -> 'undefined'
    end.

-spec corrected_base64_decode(ne_binary()) -> ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 3 ->
    base64:decode(<<Base64/binary, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 2 ->
    base64:decode(<<Base64/binary, "==">>);
corrected_base64_decode(Base64) ->
    base64:decode(Base64).
