%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc ACDc Eavesdrop.
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_eavesdrop).

-include("acdc.hrl").

-export([start/3]).

-spec start(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
start(MCall, AcctId, AgentCallId) ->
    Prop = [{<<"Eavesdrop-Mode">>, <<"listen">>}
           ,{<<"Account-ID">>, AcctId}
           ,{<<"Endpoint-ID">>, <<"5381e0c5caa8d34eec06e0f75d0b4189">>}
           ,{<<"Eavesdrop-Call-ID">>, AgentCallId}
           ,{<<"Outbound-Caller-ID-Name">>, kapps_call:caller_id_name(MCall)}
           ,{<<"Outbound-Caller-ID-Number">>, kapps_call:caller_id_number(MCall)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    eavesdrop_req(Prop).

eavesdrop_req(Prop) ->
    lager:debug("sending eavs ~p", [Prop]),
    case kz_amqp_worker:call(Prop
                            ,fun kapi_resource:publish_eavesdrop_req/1
                            ,fun kapi_resource:eavesdrop_resp_v/1
                            ,2000
                            )
    of
        {'ok', Resp} -> lager:debug("ok: ~p", [Resp]);
        {'error', 'timeout'} -> lager:debug("err: timeout");
        {'error', E} -> lager:debug("err: ~p", [E])
    end.
