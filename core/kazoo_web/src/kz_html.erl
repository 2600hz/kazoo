%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc HTML helper functions for Kazoo
%%% @author Mark Magnusson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_html).

-export([escape/1]).

%%------------------------------------------------------------------------------
%% @doc Escapes an HTML string.
%% @end
%%------------------------------------------------------------------------------
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
