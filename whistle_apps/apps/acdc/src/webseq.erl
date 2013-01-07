%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Log messages in a way to make importing to WebSequenceDiagrams.com
%%% easier
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(webseq).

-export([evt/3
         ,title/1
         ,note/3
        ]).

-include_lib("whistle/include/wh_types.hrl").

-type who() :: pid() | ne_binary().

-spec title/1 :: (ne_binary()) -> 'ok'.
title(Title) ->
    lager:critical("title ~s", [what(Title)]).

-spec evt/3 :: (who(), who(), ne_binary()) -> 'ok'.
evt(From, To, Desc) ->
    lager:critical("~s->~s: ~s", [who(From), who(To), what(Desc)]).

-spec note/3 :: (who(), 'right' | 'left', ne_binary()) -> 'ok'.
note(Who, Dir, Note) ->
    lager:critical("note ~s of ~s: ~s", [Dir, who(Who), what(Note)]).

who(P) when is_pid(P) -> pid_to_list(P);
who(B) when is_binary(B) -> B.

what(B) when is_binary(B) -> B;
what(IO) when is_list(IO) -> iolist_to_binary(IO).
