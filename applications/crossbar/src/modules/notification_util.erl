%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%%
%%% Provide helper functions for firing notifications based on crossbar changes
%%%
%%% @end
%%% @contributors
%%%     Mark Magnusson
%%%-------------------------------------------------------------------
-module(notification_util).

-include("crossbar.hrl").

-export([
         maybe_notify_account_change/2
        ]).

-spec maybe_notify_account_change(kz_json:object(), kz_json:object()) -> 'ok'.
maybe_notify_account_change(Old, New) ->
    Filter = fun({K, V}) ->
                     case kz_json:get_value(K, New) of
                         V -> 'false';
                         _ -> 'true'
                     end
             end,

    AccountId = kz_json:get_value(<<"id">>, New),
    Changed   = kz_json:filter(Filter, Old),
    Notify    = fun(V) -> maybe_notify(AccountId, V) end,

    kz_json:foreach(Notify, Changed).

-spec maybe_notify(api_binary(), {ne_binary(), kz_json:object()}) -> 'ok'.
maybe_notify(Account, {<<"zones">>, Zones}) ->
    lager:info("publishing zone change notification for ~p, zones: ~p", [Account, Zones]),
    Notify = [
              {<<"Account-ID">>, Account}
             ,{<<"Zones">>, Zones}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapi_notifications:publish_account_zone_change(Notify);

maybe_notify(_Account, {_Key, _Value}) ->
    'ok'.
