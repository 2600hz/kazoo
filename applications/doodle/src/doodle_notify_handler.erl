%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(doodle_notify_handler).

-export([handle_req/2]).

-include("doodle.hrl").


-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_registration:success_v(JObj),
    _ = wh_util:put_callid(JObj),
    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb } ->
            AccountId = wh_util:format_account_id(AccountDb),
            case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
                {'ok', EndpointId} ->
                    case cf_endpoint:get(EndpointId, AccountDb) of
                        {'ok', Endpoint} ->
                            OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
                            doodle_maintenance:start_check_sms_by_device_id(AccountId, EndpointId),
                            doodle_maintenance:start_check_sms_by_owner_id(AccountId, OwnerId);
                        {'error', _E} ->
                            lager:debug("error getting Endpoint ~s from account db ~s : ~p", [EndpointId, AccountDb, _E])
                    end;
                {'error', _E} ->
                    lager:debug("error getting EndpointId with username ~s from account db ~s : ~p", [Username, AccountDb, _E])
            end;
        {'error', 'not_found'} ->
            case doodle_util:endpoint_from_sipdb(Realm, Username) of
                {'ok', Endpoint} ->
                    AccountId = wh_json:get_value(<<"pvt_account_id">>, Endpoint),
                    EndpointId = wh_json:get_value(<<"_id">>, Endpoint),
                    OwnerId = wh_json:get_value(<<"owner_id">>, Endpoint),
                    doodle_maintenance:start_check_sms_by_device_id(AccountId, EndpointId),
                    doodle_maintenance:start_check_sms_by_owner_id(AccountId, OwnerId);
                {'error', _E} ->
                    lager:debug("error finding ~s@~s endpoint in sip_db : ~p", [Username, Realm, _E])
            end
    end.
