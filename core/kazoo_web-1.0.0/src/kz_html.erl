%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc HML helper functions for Kazoo
%%%
%%% @contributors
%%%     Mark Magnusson
%%%-------------------------------------------------------------------
-module(kz_html).

-export([
    escape/1
]).

%%--------------------------------------------------------------------
%% @doc Escapes an HTML string
%%--------------------------------------------------------------------
-spec escape(binary()) -> binary().
escape(Source) ->
    escape(Source, <<>>).

-spec escape(binary(), binary()) -> binary().
escape(<<>>, Acc) ->
    Acc;

escape(<<$<, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, "&lt;">>);

escape(<<$>, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, "&gt;">>);

escape(<<$&, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, "&amp;">>);

escape(<<H, R/binary>>, Acc) ->
    escape(R, <<Acc/binary, H>>).

