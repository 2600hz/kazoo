%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(media_util).

-export([content_type_of/1]).

content_type_of(<<"wav">>) -> <<"audio/x-wav">>;
content_type_of(<<"mp3">>) -> <<"audio/mp3">>.
