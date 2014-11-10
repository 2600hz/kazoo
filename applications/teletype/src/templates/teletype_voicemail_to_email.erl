%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_voicemail_to_email).

-export([init/0
         ,handle/2
        ]).

-spec init() -> 'ok'.
init() ->
    'ok'.

-spec handle(wh_json:object(), wh_proplist()) -> 'ok'.
handle(JObj, _Props) ->
    'ok'.
