%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(sysconf_flush).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = kapi_sysconf:flush_req_v(ApiJObj),
    kz_util:put_callid(ApiJObj),

    Category = kz_json:get_value(<<"Category">>, ApiJObj),
    Node = kz_json:get_value(<<"Node">>, ApiJObj),

    case kz_json:get_value(<<"Key">>, ApiJObj) of
        'undefined' ->
            lager:debug("flushing ~s entirely", [Category]),
            kapps_config:flush(Category);
        Key ->
            lager:debug("flushing ~s[~s.~s]", [Category, Node, Key]),
            kapps_config:flush(Category, Key, Node)
    end.
