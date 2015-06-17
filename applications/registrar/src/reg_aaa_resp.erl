%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% Handle aaa_*_resp messages
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(reg_aaa_resp).

-export([init/0, handle_req/2]).

-include("reg.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    lager:debug("received response: ~p", [JObj]),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    AuthUser = registrar_shared_listener:get_auth_user(MsgId),
    EventCategory = wh_json:get_value(<<"Event-Category">>, AuthUser#auth_user.request),
    JObj1 = wh_json:set_value(<<"Event-Category">>, EventCategory, JObj),
    registrar_shared_listener:remove_auth_user({MsgId, JObj1}),
    case wh_json:get_value(<<"AAA-Result">>, JObj1) of
        <<"accept">> ->
            lager:debug("AAA registration - accepted", [JObj]),
            reg_authn_req:send_auth_resp(registrar_shared_listener:get_auth_user(MsgId), JObj1);
        _ ->
            lager:debug("AAA registration - denied", [JObj]),
            reg_authn_req:send_auth_error(JObj1)
    end.
