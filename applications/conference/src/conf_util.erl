%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_util).

-export([cache/2
         ,retrieve/1
        ]).

-include("conference.hrl").

-spec cache(whapps_call:call(), whapps_conference:conference()) -> 'ok'.
cache(Call, Conference) ->
    wh_cache:store_local(?CONFERENCE_CACHE, whapps_call:call_id(Call), {Call, Conference}).

-spec retrieve(ne_binary()) ->
                      {'error', 'not_found'} |
                      {'ok', {whapps_call:call(), whapps_conference:conference()}}.
retrieve(CallId) ->
    wh_cache:peek_local(?CONFERENCE_CACHE, CallId).
