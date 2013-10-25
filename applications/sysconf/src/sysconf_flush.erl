%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(sysconf_flush).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

init() ->
    ok.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = wapi_sysconf:flush_req_v(ApiJObj),

    Category = wh_json:get_value(<<"Category">>, ApiJObj),
    Node = wh_json:get_value(<<"Node">>, ApiJObj),
    case wh_json:get_value(<<"Key">>, ApiJObj) of
        'undefined' ->
            lager:debug("flushing ~s entirely", [Category]),
            whapps_config:flush(Category);
        Key ->
            lager:debug("flushing ~s[~s.~s]", [Category, Node, Key]),
            whapps_config:flush(Category, Key, Node)
    end.
