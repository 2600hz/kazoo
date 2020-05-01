%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle requests to read configuration data
%%% Support nested keys a la kz_json, with a #
%%% as a separator i.e key#subkey#subsubkey
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(sysconf_get).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = kapi_sysconf:get_req_v(ApiJObj),
    kz_log:put_callid(ApiJObj),

    Category = kz_json:get_binary_value(<<"Category">>, ApiJObj),
    Key = kz_json:get_value(<<"Key">>, ApiJObj),
    Default = kz_json:get_value(<<"Default">>, ApiJObj),
    Node = kz_api:node(ApiJObj),

    lager:debug("received sysconf get for ~s:~p from ~s", [Category, Key, Node]),

    Value = get_value(Category, Key, Default, Node),
    RespQ = kz_api:server_id(ApiJObj),

    lager:debug("sending reply for ~s.~s(~s): ~p"
               ,[Category, format_key(Key), Node, Value]
               ),
    Resp = [{<<"Category">>, Category}
           ,{<<"Key">>, Key}
           ,{<<"Value">>, maybe_fix_undefined(Value)}
           ,{<<"Msg-ID">>, kz_api:msg_id(ApiJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    maybe_publish_resp(RespQ, Resp).

maybe_publish_resp('undefined', _Resp) -> 'ok';
maybe_publish_resp(RespQ, Resp) -> kapi_sysconf:publish_get_resp(RespQ, Resp).

-spec format_key(kz_term:ne_binary() | kz_term:ne_binaries()) -> kz_term:ne_binary().
format_key(Key)
  when is_binary(Key) -> Key;
format_key(Keys)
  when is_list(Keys) ->
    kz_binary:join(Keys, <<".">>).

-spec get_value(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries(), any(), kz_term:ne_binary()) -> any().
get_value(Category, Key, Default, Node) ->
    kapps_config:get_current(Category, Key, Default, Node).

-spec maybe_fix_undefined(any()) -> any().
maybe_fix_undefined('undefined') -> <<"undefined">>;
maybe_fix_undefined([]) -> <<"undefined">>;
maybe_fix_undefined(Value) -> Value.
