%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_webhook).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).
-export([handle_webhook/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = handle_webhook(Data, Call),
    cf_exe:continue(Call).

-spec handle_webhook(kz_json:object(), kapps_call:call()) -> 'ok' | {'error', any()}.
handle_webhook(Data, Call) ->
    CallJObj = format_call_data(Call),
    Hook = set_hook(Data, CallJObj),
    Props = [{<<"Hook">>, Hook}
            ,{<<"Timestamp">>, kz_time:now_s()}
            ,{<<"Data">>, CallJObj}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    kapps_notify_publisher:cast(Props, fun kapi_notifications:publish_webhook/1).

-spec format_call_data(kapps_call:call()) -> kz_json:object().
format_call_data(Call) ->
    JObj = kapps_call:to_json(Call),
    RemoveKeys = [<<"Key-Value-Store">>
                 ,<<"Control-Queue">>
                 ,<<"Controller-Queue">>
                 ,<<"Custom-Channel-Vars">>
                 ],
    kz_json:normalize_jobj(JObj, RemoveKeys, []).

-spec set_hook(kz_json:object(), kz_json:object()) -> kz_json:object().
set_hook(Data, CallJObj) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    kz_json:from_list(
      [{<<"_id">>, kz_term:to_binary(Now)}
      ,{<<"uri">>, kz_json:get_ne_binary_value(<<"uri">>, Data)}
      ,{<<"hook">>, <<"notifications">>}
      ,{<<"http_verb">>, kz_json:get_ne_binary_value(<<"http_verb">>, Data)}
      ,{<<"retries">>, kz_json:get_integer_value(<<"retries">>, Data)}
      ,{<<"pvt_account_id">>, kz_json:get_ne_binary_value(<<"account_id">>, CallJObj)}
      ,{<<"custom_data">>, kz_json:get_json_value(<<"custom_data">>, Data)}
      ]).
