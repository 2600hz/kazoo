%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_speech_util).

-export([tmp_file_name/1]).

-include("kazoo_speech.hrl").

-spec tmp_file_name(kz_term:ne_binary()) -> string().
tmp_file_name(Ext) ->
    Prefix = kz_binary:rand_hex(10),
    Name = filename:join([?TMP_PATH
                         ,<<Prefix/binary, "_voicemail.", Ext/binary>>
                         ]),
    kz_term:to_list(Name).
