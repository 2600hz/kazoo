%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @author James Aimonetti <james@2600hz.org>
%%% @doc
%%% Handle requests from WhApps for the blacklist
%%% @end
%%% Created : 29 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_blacklist_req).

-export([init/0, handle_req/2]).

-include("dth.hrl").

-spec init() -> 'ok'.
init() ->
    ok.

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = dth_api:blacklist_req_v(JObj),

    {'ok', Blacklist} = kz_cache:fetch_local(?CACHE_NAME, dth_util:blacklist_cache_key()),
    {'ok', JSON} = dth_api:blacklist_resp([{<<"Accounts">>, Blacklist}
                                           | kz_api:default_headers(<<>>, <<"dth">>, <<"blacklist_resp">>, ?APP_NAME, ?APP_VERSION)
                                          ]),
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    amqp_util:targeted_publish(RespQ, JSON, <<"application/json">>).
