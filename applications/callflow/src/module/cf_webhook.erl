%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cf_webhook).

-include("../callflow.hrl").

-export([handle/2]).
-export([handle_webhook/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    _ = wh_util:spawn(?MODULE, 'handle_webhook', [Data, Call]),
    cf_exe:continue(Call).

-spec handle_webhook(wh_json:object(), whapps_call:call()) -> 'ok' | {'error', any()}.
handle_webhook(Data, Call) ->
    CallJObj = format_call_data(Call),
    Hook = set_hook(Data, CallJObj),
    JObj = wh_json:from_list(
             [{<<"Hook">>, Hook}
              ,{<<"Timestamp">>, wh_util:current_tstamp()}
              ,{<<"Data">>, CallJObj}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    whapps_util:amqp_pool_send(JObj, fun wapi_notifications:publish_webhook/1).

-spec format_call_data(whapps_call:call()) -> wh_json:object().
format_call_data(Call) ->
    JObj = whapps_call:to_json(Call),
    RemoveKeys = [<<"Key-Value-Store">>
                  ,<<"Control-Queue">>
                  ,<<"Controller-Queue">>
                  ,<<"Custom-Channel-Vars">>
                 ],
    wh_json:normalize_jobj(JObj, RemoveKeys, []).

-spec set_hook(wh_json:object(), wh_json:object()) -> wh_json:object().
set_hook(Data, CallJObj) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    wh_json:from_list(
        props:filter_undefined(
          [{<<"_id">>, wh_util:to_binary(Now)}
           ,{<<"uri">>, wh_json:get_value(<<"uri">>, Data)}
           ,{<<"hook">>, <<"callflow">>}
           ,{<<"http_verb">>, wh_json:get_value(<<"http_verb">>, Data)}
           ,{<<"retries">>, wh_json:get_value(<<"retries">>, Data)}
           ,{<<"pvt_account_id">>, wh_json:get_value(<<"account_id">>, CallJObj)}
           ,{<<"custom_data">>, wh_json:get_value(<<"custom_data">>, Data)}
          ])
     ).
