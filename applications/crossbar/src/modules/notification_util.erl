%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Provide helper functions for firing notifications based on crossbar changes
%%% @author Mark Magnusson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(notification_util).

-include("crossbar.hrl").

-export([
         maybe_notify_account_change/2
        ]).

-spec maybe_notify_account_change(kz_json:object(), cb_context:context()) -> 'ok'.
maybe_notify_account_change(Old, Context) ->
    New = cb_context:doc(Context),
    Filter = fun({K, V}) ->
                     case kz_json:get_value(K, New) of
                         V -> 'false';
                         _ -> 'true'
                     end
             end,

    AccountId = kz_doc:id(New),
    Changed   = kz_json:filter(Filter, Old),
    Notify    = fun({K, _}) -> notify_account_change(AccountId, {K, kz_json:get_value(K, New)}, Context) end,

    kz_json:foreach(Notify, Changed).

-spec notify_account_change(kz_term:api_binary(), {kz_term:ne_binary(), kz_json:object()}, cb_context:context()) -> 'ok'.
notify_account_change(AccountId, {<<"zones">>, Zones}, _Context) ->
    lager:info("publishing zone change notification for ~p, zones: ~p", [AccountId, Zones]),
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Zones">>, Zones}
             | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
            ],
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_account_zone_change/1);

notify_account_change(AccountId, {<<"pvt_enabled">>, IsEnabled}, Context) ->
    lager:info("account ~s enabled has changed, sending registrations flush: pvt_enable: ~p", [AccountId, IsEnabled]),
    crossbar_util:flush_registrations(Context),
    crossbar_util:maybe_refresh_fs_xml('account', Context);

notify_account_change(_Account, {_Key, _Value}, _Context) ->
    'ok'.
