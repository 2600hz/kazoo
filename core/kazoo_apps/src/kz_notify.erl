%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_notify).

-export([generic_alert/2]).
-export([system_alert/2, system_alert/3, system_alert/4]).
-export([detailed_alert/3, detailed_alert/4, detailed_alert/5]).

-include("kazoo_apps.hrl").

-spec system_alert(atom() | string() | binary(), [any()]) -> 'ok'.
system_alert(Format, Args) ->
    Msg = io_lib:format(Format, Args),
    system_alert(Msg, Msg, []).

-spec system_alert(string() | kz_term:ne_binary(), string() | kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
system_alert(Subject, Msg, Headers)
  when not is_binary(Subject);
       not is_binary(Msg) ->
    system_alert(kz_term:to_binary(Subject), kz_term:to_binary(Msg), Headers);
system_alert(Subject, Msg, Headers) ->
    Notify= [{<<"Message">>, Msg}
            ,{<<"Subject">>, <<"System Alert: ", Subject/binary>>}
             | Headers ++ kz_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).

-spec system_alert(kz_term:ne_binary(), string() | kz_term:ne_binary(), [any()], kz_term:proplist()) -> 'ok'.
system_alert(Subject, Format, Args, Headers) ->
    Msg = io_lib:format(Format, Args),
    system_alert(Subject, Msg, Headers).

-spec detailed_alert(string(), list(), kz_term:proplist()) -> 'ok'.
detailed_alert(Format, Args, Props) ->
    Msg = io_lib:format(Format, Args),
    detailed_alert(Msg, Msg, [{<<"Format">>, kz_term:to_binary(Format)} | Props], []).

-spec detailed_alert(string() | kz_term:ne_binary(), string() | kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
detailed_alert(Subject, Msg, Props, Headers)
  when not is_binary(Subject);
       not is_binary(Msg) ->
    detailed_alert(kz_term:to_binary(Subject), kz_term:to_binary(Msg), Props, Headers);
detailed_alert(Subject, Msg, Props, Headers) ->
    Notify = [{<<"Message">>, Msg}
             ,{<<"Subject">>, <<"System Alert: ", Subject/binary>>}
             ,{<<"Details">>, kz_json:from_list_recursive(Props)}
              | Headers ++ kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).

-spec detailed_alert(string() | kz_term:ne_binary(), string() | kz_term:ne_binary(), [any()], kz_term:proplist(), kz_term:proplist()) -> 'ok'.
detailed_alert(Subject, Format, Args, Props, Headers) ->
    Msg = io_lib:format(Format, Args),
    detailed_alert(Subject, Msg, [{<<"Format">>, kz_term:to_binary(Format)} | Props], Headers).

-spec generic_alert(atom() | string() | binary(), atom() | string() | binary()) -> 'ok'.
generic_alert(Subject, Msg) ->
    Notify= [{<<"Message">>, kz_term:to_binary(Msg)}
            ,{<<"Subject">>, <<"System Alert: ", (kz_term:to_binary(Subject))/binary>>}
             | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).
