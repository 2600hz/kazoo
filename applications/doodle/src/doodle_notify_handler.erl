%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handlers for various AMQP payloads
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(doodle_notify_handler).

-export([handle_req/2]).

-include("doodle.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_registration:success_v(JObj),
    _ = kz_log:put_callid(JObj),
    Username = kz_json:get_value(<<"Username">>, JObj),
    Realm = kz_json:get_value(<<"Realm">>, JObj),
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} -> handle_account_req(AccountDb, Username);
        {'error', 'not_found'} -> handle_no_account_req(Realm, Username)
    end.

-spec handle_account_req(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_account_req(AccountDb, Username) ->
    AccountId = kz_util:format_account_id(AccountDb),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
        {'ok', EndpointId} ->
            case kz_endpoint:get(EndpointId, AccountDb) of
                {'ok', Endpoint} ->
                    OwnerId = kz_json:get_value(<<"owner_id">>, Endpoint),
                    doodle_maintenance:start_check_sms_by_device_id(AccountId, EndpointId),
                    doodle_maintenance:start_check_sms_by_owner_id(AccountId, OwnerId);
                {'error', _E} ->
                    lager:debug("error getting Endpoint ~s from account db ~s : ~p"
                               ,[EndpointId, AccountDb, _E])
            end;
        {'error', _E} ->
            lager:debug("error getting EndpointId with username ~s from account db ~s : ~p"
                       ,[Username, AccountDb, _E])
    end.

-spec handle_no_account_req(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_no_account_req(Realm, Username) ->
    case doodle_util:endpoint_from_sipdb(Realm, Username) of
        {'ok', Endpoint} ->
            AccountId = kz_doc:account_id(Endpoint),
            EndpointId = kz_doc:id(Endpoint),
            OwnerId = kz_json:get_value(<<"owner_id">>, Endpoint),
            doodle_maintenance:start_check_sms_by_device_id(AccountId, EndpointId),
            doodle_maintenance:start_check_sms_by_owner_id(AccountId, OwnerId);
        {'error', _E} ->
            lager:debug("error finding ~s@~s endpoint in sip_db : ~p", [Username, Realm, _E])
    end.
