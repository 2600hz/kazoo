%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_notify).

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
-export([system_alert/2, system_alert/3]).

-include("../include/wh_types.hrl").

-define(APP_NAME, <<"whistle">>).
-define(APP_VERSION, <<"1.2.1">>).

-spec cnam_request(wh_json:object()) -> 'ok'.
-spec cnam_request(wh_json:object(), wh_json:object()) -> 'ok'.

cnam_request(PhoneNumber) ->
    AccountId = wh_json:get_ne_value(<<"pvt_assigned_to">>, PhoneNumber),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, Account} -> cnam_request(Account, PhoneNumber);
        {error, Reason} ->
            Number = wh_json:get_value(<<"_id">>, PhoneNumber),
            Subject = io_lib:format("unable to open account ~s for cnam update on ~s", [AccountId, Number]),
            Body = io_lib:format("Failed to open account ~s (~s) for cnam update on ~s.~nReason: ~p", [AccountId, AccountDb, Number, Reason]),
            generic_alert(Subject, Body)
    end.
            
cnam_request(PhoneNumber, Account) ->
    Notify = [{<<"Account">>, Account}
              ,{<<"Phone-Number">>, PhoneNumber}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).

-spec port_request(wh_json:object()) -> 'ok'.
-spec port_request(wh_json:object(), wh_json:object()) -> 'ok'.

port_request(PhoneNumber) ->
    AccountId = wh_json:get_ne_value(<<"pvt_assigned_to">>, PhoneNumber),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, Account} -> cnam_request(Account, PhoneNumber);
        {error, Reason} ->
            Number = wh_json:get_value(<<"_id">>, PhoneNumber),
            Subject = io_lib:format("unable to open account ~s for port request of ~s", [AccountId, Number]),
            Body = io_lib:format("Failed to open account ~s (~s) for port request of ~s.~nReason: ~p", [AccountId, AccountDb, Number, Reason]),
            generic_alert(Subject, Body)
    end.    

port_request(PhoneNumber, Account) ->
    Notify = [{<<"Account">>, Account}
              ,{<<"Phone-Number">>, PhoneNumber}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_cnam_request(Notify).

-spec deregister(wh_json:object()) -> 'ok'.
-spec deregister(wh_json:object(), wh_json:object()) -> 'ok'.
-spec deregister(wh_json:object(), wh_json:object(), wh_json:object()) -> 'ok'.

deregister(LastReg) ->
    AuthorizingId = wh_json:get_value(<<"Authorizing-ID">>, LastReg),
    AccountDb = wh_json:get_value(<<"Account-DB">>, LastReg),
    case couch_mgr:open_cache_doc(AccountDb, AuthorizingId) of
        {ok, Endpoint} -> deregister(LastReg, Endpoint);
        {error, _R} ->
            lager:info("unable to lookup endpoint ~s in database ~s for deregister notice: ~p", [AuthorizingId, AccountDb, _R])
    end.

deregister(LastReg, Endpoint) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, LastReg),
    AccountDb = wh_json:get_value(<<"Account-DB">>, LastReg),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, Account} -> deregister(LastReg, Endpoint, Account);
        {error, _R} ->
            lager:info("unable to open account ~s deregister notice: ~p", [AccountId, _R])
    end.

deregister(LastReg, Endpoint, Account) ->
    Notify = [{<<"Account">>, Account}
              ,{<<"Endpoint">>, Endpoint}
              ,{<<"Last-Registration">>, LastReg}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_deregister(Notify).

low_balance(_Account, _Credit) ->
    ok.

new_account(_User, _Account) ->
    ok.

pwd_recovery(_User, _Account) ->
    ok.

abnormal_hangup(_CDR, _Account) ->
    ok.

first_call(_Account) ->
    ok.

first_registration(_Account) ->
    ok.

-spec transaction(ne_binary(), wh_json:object()) -> 'ok'.
-spec transaction(ne_binary(), wh_json:object(), api_object()) -> 'ok'.

transaction(Account, Transaction) ->
    transaction(Account, Transaction, undefined).

transaction(Account, Transaction, ServicePlan) ->
    Notify = [{<<"Account-ID">>, wh_util:format_account_id(Account, raw)}
              ,{<<"Transaction">>, Transaction}
              ,{<<"Service-Plan">>, ServicePlan}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_transaction(props:filter_undefined(Notify)).

-spec system_alert(atom() | string() | binary(), [term()]) -> 'ok'.
-spec system_alert(atom() | string() | binary(), [term()], proplist()) -> 'ok'.

system_alert(Format, Args) ->
    system_alert(Format, Args, []).

system_alert(Format, Args, Props) ->
    Msg = io_lib:format(Format, Args),
    Notify= [{<<"Message">>, wh_util:to_binary(Msg)}
             ,{<<"Subject">>, <<"KAZOO: ", (wh_util:to_binary(Msg))/binary>>}
             ,{<<"Details">>, wh_json:from_list(Props)}
             | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    wapi_notifications:publish_system_alert(Notify).

-spec generic_alert(atom() | string() | binary(), atom() | string() | binary()) -> 'ok'.
generic_alert(Subject, Msg) ->
    Notify= [{<<"Message">>, wh_util:to_binary(Msg)}
             ,{<<"Subject">>, <<"KAZOO: ", (wh_util:to_binary(Subject))/binary>>}
             | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    wapi_notifications:publish_system_alert(Notify).
