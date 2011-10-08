%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 6 Oct 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_subscribe_query).

-export([init/0, handle_req/2]).

-include("notify.hrl").
-include_lib("whistle/include/wh_types.hrl").

-spec init/0 :: () -> 'ok'.
init() ->
    ok.

-spec handle_req/2 :: (JObj, Props) -> no_return() when
      JObj :: json_object(),
      Props :: proplist().
handle_req(JObj, _Props) ->
    true = wh_api:presence_subscrs_query_v(JObj),

    User = wh_json:get_value(<<"User">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    Fields = wh_json:get_value(<<"Fields">>, JObj),

    case notify_util:lookup_subscribers(User, Realm) of
        [] ->
            ok;
        Subscribers when is_list(Fields), Fields =/= [] ->
            send_response(notify_util:filter_subscribers(Subscribers, Fields), JObj);
        Subscribers ->
            send_response(Subscribers, JObj)
    end.

send_response(Subscribers, JObj) ->
    Response = [{<<"Subscribers">>, Subscribers}
                ,{<<"Event">>, wh_json:get_value(<<"Event">>, JObj)}
                | wh_api:default_headers(<<>>, <<"presence">>, <<"subscribers_query_resp">>, ?APP_NAME, ?APP_VERSION)
               ],
    {ok, Payload} = wh_api:presence_subscrs_query_resp([ KV || {_, V}=KV <- Response, V =/= undefined ]),
    amqp_util:callmgr_publish(Payload, <<"application/json">>, ?KEY_PRESENCE_IN).
