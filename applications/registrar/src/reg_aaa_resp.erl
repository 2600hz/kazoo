%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% Handle aaa_*_resp messages
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(reg_aaa_resp).

-export([init/0, handle_req/2, send_aaa_request/1]).

-include("reg.hrl").

send_aaa_request({'authn', #auth_user{request = Request} = AuthUser}) ->
    lager:debug([{'trace', 'true'}], "AuthUser=~p~n", [AuthUser]),
    'true' = wapi_authn:req_v(Request),
    MsgId = wh_json:get_value(<<"Msg-ID">>, Request),
    registrar_shared_listener:insert_auth_user({MsgId, AuthUser}),
    % TODO: is it critical to replace Event-Name and Event-Category?
    % It seems that no, but it whould be better to check it
    Request1 = wh_json:set_values([{<<"Response-Queue">>, registrar_shared_listener:get_queue_name()},
                                   {<<"Event-Category">>, <<"aaa">>},
                                   {<<"Event-Name">>, <<"aaa_authn_req">>}
                                  ], Request),
    wapi_aaa:publish_req(Request1).

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    lager:debug([{'trace', 'true'}], "JObj=~p~nProps=~p~n", [JObj, Props]),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    registrar_shared_listener:remove_auth_user({MsgId, JObj}),
    case wh_json:get_value(<<"AAA-Result">>, JObj) of
        <<"ok">> ->
            reg_authn_req:send_auth_resp(registrar_shared_listener:get_auth_user(MsgId), JObj);
        <<"error">> ->
            reg_authn_req:send_auth_error(JObj);
        'undefined' ->
            reg_authn_req:send_auth_error(JObj)
    end.
