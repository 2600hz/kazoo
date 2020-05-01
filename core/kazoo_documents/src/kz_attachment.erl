%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Account document
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_attachment).

-export([decode_base64/1]).

-include("kz_documents.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec decode_base64(kz_term:ne_binary()) -> {kz_term:api_binary() | {'error', 'badarg'}, kz_term:ne_binary()}.
decode_base64(Base64) ->
    case binary:split(Base64, <<",">>) of
        %% http://tools.ietf.org/html/rfc4648
        [Bin] ->
            {'undefined', corrected_base64_decode(strip_base64(Bin))};
        %% http://tools.ietf.org/rfc/rfc2397.txt
        [<<"data:", CT/binary>>, Bin] ->
            {get_content_type(CT), corrected_base64_decode(strip_base64(Bin))};
        [_SplitLeft, _SplitRight] ->
            {'undefined', corrected_base64_decode(strip_base64(Base64))}
    end.

-spec get_content_type(kz_term:ne_binary()) -> kz_term:api_binary() | {'error', 'badarg'}.
get_content_type(CT) ->
    %% stripping white-spaces for lazy developers
    MediaType = binary:replace(CT, <<$\s>>, <<>>, [global]),
    get_content_type(MediaType, kz_binary:truncate_left(MediaType, 6)).

get_content_type(MediaType, <<"base64">>) ->
    get_content_type(kz_binary:truncate_right(MediaType, byte_size(MediaType) - 7), 'undefined');
get_content_type(MediaType, _) ->
    try cow_http_hd:parse_accept(MediaType) of
        [{{Type, SubType, _Options}, _, _}] ->
            kz_binary:join([Type, SubType], <<"/">>)
    catch
        _E:_R ->
            lager:debug("failed to parse content-type ~p: ~s: ~p", [MediaType, _E, _R]),
            {'error', 'badarg'}
    end.

-spec strip_base64(kz_term:ne_binary()) -> kz_term:ne_binary().
strip_base64(Base64) ->
    %% Why Cowboy is not doing this?
    binary:replace(Base64, [<<$\s>>, <<$\r>>, <<$\n>>], <<>>, [global]).

-spec corrected_base64_decode(kz_term:ne_binary()) -> kz_term:ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 =:= 3 ->
    base64:decode(<<Base64/binary, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 =:= 2 ->
    base64:decode(<<Base64/binary, "==">>);
corrected_base64_decode(Base64) ->
    base64:decode(Base64).
