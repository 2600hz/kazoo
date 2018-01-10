%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz
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
-spec decode_base64(kz_term:ne_binary()) -> {kz_term:api_binary(), kz_term:ne_binary()}.
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

-spec get_content_type(kz_term:ne_binary()) -> kz_term:api_binary().
get_content_type(MediaType) ->
    get_content_type(MediaType, kz_binary:truncate_left(MediaType, 6)).

get_content_type(MediaType, <<"base64">>) ->
    get_content_type(kz_binary:truncate_right(MediaType, byte_size(MediaType) - 7), 'undefined');
get_content_type(MediaType, _) ->
    try cow_http_hd:parse_accept(MediaType) of
        [{{Type, SubType, _Options}, _, _}] ->
            kz_binary:join([Type, SubType], <<"/">>)
    catch
        _E:_R ->
            lager:debug("failed to parse ~p: ~s: ~p", [MediaType, _E, _R]),
            {'error', 'badarg'}
    end.

-spec corrected_base64_decode(kz_term:ne_binary()) -> kz_term:ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 =:= 3 ->
    base64:decode(<<Base64/binary, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 =:= 2 ->
    base64:decode(<<Base64/binary, "==">>);
corrected_base64_decode(Base64) ->
    base64:decode(Base64).
