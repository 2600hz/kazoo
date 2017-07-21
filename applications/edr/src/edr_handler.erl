%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_handler).

-include("edr.hrl").

-export([handle_req/2]).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props)->
    Timestamp = kz_json:get_value(<<"Timestamp">>, JObj),
    Tags = kz_json:get_value(<<"Tags">>, JObj),
    edr_utils:distribute_event(Timestamp, Tags).
