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
            lager:debug("not split on ','"),
            {'undefined', corrected_base64_decode(Bin)};
        %% http://tools.ietf.org/rfc/rfc2397.txt
        [<<"data:", CT/binary>>, Bin] ->
            {ContentType, _Opts} = mochiweb_util:parse_header(wh_util:to_list(CT)),

            {wh_util:to_binary(ContentType), corrected_base64_decode(Bin)};
        [_SplitLeft, _SplitRight] ->
            lager:debug("split unexpectedly: ~p/~p", [byte_size(_SplitLeft), byte_size(_SplitRight)]),
            lager:debug("l: ~s", [binary:part(_SplitLeft, byte_size(_SplitLeft), -20)]),
            lager:debug("r: ~s", [binary:part(_SplitRight, byte_size(_SplitRight), -10)]),
            {'undefined', corrected_base64_decode(Base64)}
    end.

-spec corrected_base64_decode(ne_binary()) -> ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 3 ->
    base64:mime_decode(<<Base64/binary, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 2 ->
    base64:mime_decode(<<Base64/binary, "==">>);
corrected_base64_decode(Base64) ->
    base64:mime_decode(Base64).


