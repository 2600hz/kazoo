%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Handle requests from WhApps for the blacklist
%%% @end
%%% Created : 29 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(blacklist_req).

-export([init/0, handle_req/2]).

-include("dth.hrl").

init() ->
    ok.

handle_req(JObj, Props) ->
    true = dth_api:blacklist_req_v(JObj),
    Cache = props:get_value(cache, Props),

    {ok, Blacklist} = wh_cache:fetch_local(Cache, dth_util:blacklist_cache_key()),
    {ok, JSON} = dth_api:blacklist_resp([{<<"Accounts">>, Blacklist}
					 | whistle_api:default_headers(<<>>, <<"dth">>, <<"blacklist_resp">>, ?APP_NAME, ?APP_VERSION)
					]),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    amqp_util:targeted_response(RespQ, JSON, <<"application/json">>).
