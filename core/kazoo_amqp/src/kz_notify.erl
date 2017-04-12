%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_notify).

-export([low_balance/2]).
-export([generic_alert/2]).
-export([first_call/1]).
-export([first_registration/1]).
-export([system_alert/2, system_alert/3, system_alert/4]).
-export([detailed_alert/3, detailed_alert/4, detailed_alert/5]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(APP_NAME, <<"kazoo">>).
-define(APP_VERSION, <<"4.0.0">>).

-spec low_balance(ne_binary(), float() | integer() | ne_binary()) -> 'ok'.
low_balance(AccountId, Credit) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Current-Balance">>, Credit}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_low_balance/1).

-spec first_call(ne_binary()) -> 'ok'.
first_call(AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Occurrence">>, <<"call">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_first_occurrence/1).

-spec first_registration(ne_binary()) -> 'ok'.
first_registration(AccountId) ->
    Req = [{<<"Account-ID">>, AccountId}
          ,{<<"Occurrence">>, <<"registration">>}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_first_occurrence/1).

-spec system_alert(atom() | string() | binary(), [any()]) -> 'ok'.
system_alert(Format, Args) ->
    Msg = io_lib:format(Format, Args),
    system_alert(Msg, Msg, []).

-spec system_alert(string() | ne_binary(), string() | ne_binary(), kz_proplist()) -> 'ok'.
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

-spec system_alert(ne_binary(), string() | ne_binary(), [any()], kz_proplist()) -> 'ok'.
system_alert(Subject, Format, Args, Headers) ->
    Msg = io_lib:format(Format, Args),
    system_alert(Subject, Msg, Headers).

-spec detailed_alert(string(), list(), kz_proplist()) -> 'ok'.
detailed_alert(Format, Args, Props) ->
    Msg = io_lib:format(Format, Args),
    detailed_alert(Msg, Msg, [{<<"Format">>, kz_term:to_binary(Format)} | Props], []).

-spec detailed_alert(string() | ne_binary(), string() | ne_binary(), kz_proplist(), kz_proplist()) -> 'ok'.
detailed_alert(Subject, Msg, Props, Headers)
  when not is_binary(Subject);
       not is_binary(Msg) ->
    detailed_alert(kz_term:to_binary(Subject), kz_term:to_binary(Msg), Props, Headers);
detailed_alert(Subject, Msg, Props, Headers) ->
    Notify = [{<<"Message">>, Msg}
             ,{<<"Subject">>, <<"System Alert: ", Subject/binary>>}
             ,{<<"Details">>, kz_json:from_list(Props)}
              | Headers ++ kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).

-spec detailed_alert(string() | ne_binary(), string() | ne_binary(), [any()], kz_proplist(), kz_proplist()) -> 'ok'.
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
