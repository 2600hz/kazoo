%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_media_mgr).

-export([request_media_url/1, request_media_url/2]).

-include("kazoo_proper.hrl").

-spec request_media_url(kz_term:ne_binary()) -> kz_amqp_worker:request_return().
request_media_url(MediaName) ->
    request_media_url(MediaName, <<"new">>).

-spec request_media_url(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_amqp_worker:request_return().
request_media_url(MediaName, Type) ->
    MsgProps = props:filter_undefined(
                 [{<<"Media-Name">>, MediaName}
                 ,{<<"Stream-Type">>, Type}
                 ,{<<"Call-ID">>, kz_binary:rand_hex(8)}
                 ,{<<"Msg-ID">>, kz_binary:rand_hex(8)}
                  | kz_api:default_headers(<<"media">>, <<"media_req">>, ?APP_NAME, ?APP_VERSION)
                 ]),
    kz_amqp_worker:call_collect(MsgProps
                               ,fun kapi_media:publish_req/1
                               ,{'media_mgr', fun kapi_media:resp_v/1}
                               ).
