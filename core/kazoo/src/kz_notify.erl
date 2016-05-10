%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_notify).

-export([cnam_request/1, cnam_request/2]).
-export([port_request/1, port_request/2]).
-export([deregister/1, deregister/2, deregister/3]).
-export([low_balance/2]).
-export([new_account/2]).
-export([pwd_recovery/2]).
-export([abnormal_hangup/2]).
-export([first_call/1]).
-export([first_registration/1]).
-export([transaction/2, transaction/3]).
-export([system_alert/2]).
-export([detailed_alert/3]).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-define(APP_NAME, <<"kazoo">>).
-define(APP_VERSION, <<"4.0.0">>).

-spec cnam_request(kz_json:object()) -> 'ok'.
-spec cnam_request(kz_json:object(), kz_json:object()) -> 'ok'.

cnam_request(PhoneNumber) ->
    AccountId = kz_json:get_ne_value(<<"pvt_assigned_to">>, PhoneNumber),
    case kz_account:fetch(AccountId) of
        {'ok', Account} -> cnam_request(Account, PhoneNumber);
        {'error', Reason} ->
            Number = kz_doc:id(PhoneNumber),
            Subject = io_lib:format("unable to open account ~s for cnam update on ~s", [AccountId, Number]),
            Body = io_lib:format("Failed to open account doc ~s for cnam update on ~s.~nReason: ~p", [AccountId, Number, Reason]),
            generic_alert(Subject, Body)
    end.

cnam_request(PhoneNumber, Account) ->
    Notify = [{<<"Account">>, Account}
              ,{<<"Phone-Number">>, PhoneNumber}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_cnam_request/1).

-spec port_request(kz_json:object()) -> 'ok'.
-spec port_request(kz_json:object(), kz_json:object()) -> 'ok'.

port_request(PhoneNumber) ->
    AccountId = kz_json:get_ne_value(<<"pvt_assigned_to">>, PhoneNumber),
    case kz_account:fetch(AccountId) of
        {'ok', Account} -> cnam_request(Account, PhoneNumber);
        {'error', Reason} ->
            Number = kz_doc:id(PhoneNumber),
            Subject = io_lib:format("unable to open account ~s for port request of ~s", [AccountId, Number]),
            Body = io_lib:format("Failed to open account doc ~s for port request of ~s.~nReason: ~p", [AccountId, Number, Reason]),
            generic_alert(Subject, Body)
    end.

port_request(PhoneNumber, Account) ->
    Notify = [{<<"Account">>, Account}
              ,{<<"Phone-Number">>, PhoneNumber}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_cnam_request/1).

-spec deregister(kz_json:object()) -> 'ok'.
-spec deregister(kz_json:object(), kz_json:object()) -> 'ok'.
-spec deregister(kz_json:object(), kz_json:object(), kz_json:object()) -> 'ok'.

deregister(LastReg) ->
    AuthorizingId = kz_json:get_value(<<"Authorizing-ID">>, LastReg),
    AccountDb = kz_json:get_value(<<"Account-DB">>, LastReg),
    case kz_datamgr:open_cache_doc(AccountDb, AuthorizingId) of
        {'ok', Endpoint} -> deregister(LastReg, Endpoint);
        {'error', _R} ->
            lager:info("unable to lookup endpoint ~s in database ~s for deregister notice: ~p", [AuthorizingId, AccountDb, _R])
    end.

deregister(LastReg, Endpoint) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, LastReg),
    case kz_account:fetch(AccountId) of
        {'ok', Account} -> deregister(LastReg, Endpoint, Account);
        {'error', _R} ->
            lager:info("unable to open account ~s deregister notice: ~p", [AccountId, _R])
    end.

deregister(LastReg, Endpoint, Account) ->
    Notify = [{<<"Account">>, Account}
              ,{<<"Endpoint">>, Endpoint}
              ,{<<"Last-Registration">>, LastReg}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_deregister/1).

-spec low_balance(ne_binary(), float() | integer() | ne_binary()) -> 'ok'.
low_balance(AccountId, Credit) ->
    Req = [{<<"Account-ID">>, AccountId}
           ,{<<"Current-Balance">>, Credit}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kz_amqp_worker:cast(Req, fun kapi_notifications:publish_low_balance/1).

new_account(_User, _Account) ->
    'ok'.

pwd_recovery(_User, _Account) ->
    'ok'.

abnormal_hangup(_CDR, _Account) ->
    'ok'.

first_call(_Account) ->
    'ok'.

first_registration(_Account) ->
    'ok'.

-spec transaction(ne_binary(), kz_json:object()) -> 'ok'.
-spec transaction(ne_binary(), kz_json:object(), api_object()) -> 'ok'.

transaction(Account, Transaction) ->
    transaction(Account, Transaction, 'undefined').

transaction(Account, Transaction, ServicePlan) ->
    Notify = props:filter_undefined(
               [{<<"Account-ID">>, kz_account:format_id(Account, 'raw')}
                ,{<<"Transaction">>, Transaction}
                ,{<<"Service-Plan">>, ServicePlan}
                | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
               ]),
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_transaction/1).

-spec system_alert(atom() | string() | binary(), [any()]) -> 'ok'.
system_alert(Format, Args) ->
    Msg = io_lib:format(Format, Args),
    Notify= [{<<"Message">>, kz_term:to_binary(Msg)}
             ,{<<"Subject">>, <<"KAZOO: ", (kz_term:to_binary(Msg))/binary>>}
             | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).

-spec detailed_alert(string(), list(), kz_proplist()) -> 'ok'.
detailed_alert(Format, Args, Props) ->
    Msg = io_lib:format(Format, Args),
    Notify = [{<<"Message">>, kz_term:to_binary(Msg)}
              ,{<<"Subject">>, <<"KAZOO: ", (kz_term:to_binary(Msg))/binary>>}
              ,{<<"Details">>,
                kz_json:from_list(
                  %% Include Format to help parse JSON data sent
                  [{<<"Format">>, kz_term:to_binary(Format)}
                   | Props
                  ])
               }
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).

-spec generic_alert(atom() | string() | binary(), atom() | string() | binary()) -> 'ok'.
generic_alert(Subject, Msg) ->
    Notify= [{<<"Message">>, kz_term:to_binary(Msg)}
             ,{<<"Subject">>, <<"KAZOO: ", (kz_term:to_binary(Subject))/binary>>}
             | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    kz_amqp_worker:cast(Notify, fun kapi_notifications:publish_system_alert/1).
