%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Handle requests to read configuration data
%%% Support nested keys a la kz_json, with a #
%%% as a separator i.e key#subkey#subsubkey
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(sysconf_get).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    'true' = kapi_sysconf:get_req_v(ApiJObj),
    kz_util:put_callid(ApiJObj),

    Category = kz_json:get_binary_value(<<"Category">>, ApiJObj),
    Key = kz_json:get_value(<<"Key">>, ApiJObj),
    Default = kz_json:get_value(<<"Default">>, ApiJObj),
    Node = kz_json:get_binary_value(<<"Node">>, ApiJObj),

    lager:debug("received sysconf get for ~s:~s from ~s", [Category, Key, Node]),

    Value = get_value(Category, Key, Default, Node),
    RespQ = kz_json:get_value(<<"Server-ID">>, ApiJObj),

    lager:debug("sending reply for ~s.~s(~s): ~p"
               ,[Category, format_key(Key), Node, Value]
               ),
    Resp = [{<<"Category">>, Category}
           ,{<<"Key">>, Key}
           ,{<<"Value">>, maybe_fix_undefined(Value)}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, ApiJObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_sysconf:publish_get_resp(RespQ, Resp).

-spec format_key(ne_binary() | ne_binaries()) -> ne_binary().
format_key(Key)
  when is_binary(Key) -> Key;
format_key(Keys)
  when is_list(Keys) ->
    kz_util:join_binary(Keys, <<".">>).

-spec get_value(ne_binary(), ne_binary() | ne_binaries(), any(), ne_binary()) -> any().
get_value(_, <<"acls">>, _, Node) ->
    sysconf_acls:build(Node);
get_value(_, <<"gateways">>, _, Node) ->
    sysconf_gateways:build(Node);
get_value(Category, Key, Default, Node) ->
    kapps_config:get_current(Category, Key, Default, Node).

-spec maybe_fix_undefined(any()) -> any().
maybe_fix_undefined('undefined') -> <<"undefined">>;
maybe_fix_undefined(Value) -> Value.
