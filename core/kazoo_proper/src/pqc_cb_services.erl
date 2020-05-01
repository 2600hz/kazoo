%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_services).

-export([create_service_plan/2
        ,delete_service_plan/2
        ,assign_service_plan/3
        ,available_service_plans/2
        ,cleanup/0
        ]).

-include("kazoo_proper.hrl").

-spec create_service_plan(pqc_cb_api:state(), kzd_service_plan:doc()) ->
          {'ok', kzd_service_plan:doc()}.
create_service_plan(_API, ServicePlan) ->
    %% No API to add service plans to master account
    %% Doing so manually for now
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    {'ok', OldVsn} = kz_datamgr:save_doc(MasterAccountDb, ServicePlan),
    io:format("old: ~p~n", [OldVsn]),
    Migrate = kazoo_services_maintenance:migrate_service_plan(MasterAccountDb, OldVsn),
    io:format("mig: ~p~n", [Migrate]),
    {'ok', Migrate}.

-spec delete_service_plan(pqc_cb_api:state(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
delete_service_plan(_API, ServicePlanId) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    kz_datamgr:del_doc(MasterAccountDb, ServicePlanId).

-spec assign_service_plan(pqc_cb_api:state(), kz_term:ne_binary() | proper_types:type(), kz_term:ne_binary()) -> pqc_cb_api:response().
assign_service_plan(API, AccountId, ServicePlanId) ->
    URL = account_service_plan_url(AccountId),
    RequestHeaders = pqc_cb_api:request_headers(API),

    RequestData = kz_json:from_list([{<<"add">>, [ServicePlanId]}]),
    RequestEnvelope = pqc_cb_api:create_envelope(RequestData),

    Expectations = [#expectation{response_codes = [200, 404]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:post/3
                           ,URL
                           ,RequestHeaders
                           ,kz_json:encode(RequestEnvelope)
                           ).

-spec available_service_plans(pqc_cb_api:state(), kz_term:ne_binary() | proper_types:type()) ->
          pqc_cb_api:response().
available_service_plans(API, AccountId) ->
    URL = string:join([account_service_plan_url(AccountId), "available"], "/"),
    RequestHeaders = pqc_cb_api:request_headers(API),
    Expectations = [#expectation{response_codes = [200]}],
    pqc_cb_api:make_request(Expectations
                           ,fun kz_http:get/2
                           ,URL
                           ,RequestHeaders
                           ).

-spec account_service_plan_url(kz_term:ne_binary()) -> string().
account_service_plan_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "services"], "/").

-spec cleanup() -> 'ok'.
cleanup() ->
    kazoo_services_maintenance:remove_orphaned_services(),
    kt_cleanup:cleanup_soft_deletes(<<"services">>).
