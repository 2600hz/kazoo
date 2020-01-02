%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_maintenance).

-include("doodle.hrl").

-export([send_outbound_sms/2, send_outbound_sms/3]).

-export([flush/0]).

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?CACHE_NAME).


-define(DEFAULT_FROM,
        kapps_config:get_ne_binary(?CONFIG_CAT, <<"default_test_from_number">>, <<"15552220001">>)).

-spec send_outbound_sms(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_outbound_sms(To, Msg) ->
    send_outbound_sms(To, ?DEFAULT_FROM, Msg).

-spec send_outbound_sms(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
send_outbound_sms(To, From, Msg) ->
    Payload = [{<<"Message-ID">>, kz_binary:rand_hex(16)}
              ,{<<"System-ID">>, kz_util:node_name()}
              ,{<<"From">>, From}
              ,{<<"To">>, kz_term:to_binary(To)}
              ,{<<"Body">>, Msg}
               | kz_api:default_headers(<<"sms">>, <<"outbound">>, ?APP_NAME, ?APP_VERSION)
              ],
    kz_amqp_worker:cast(Payload, fun kapi_im:publish_outbound/1).
