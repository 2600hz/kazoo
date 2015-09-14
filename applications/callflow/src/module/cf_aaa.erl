%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is main application module
%%%
%%% "data":{
%%%   "request_fields": // List of fields for sending as part of RADIUS-request (part of Custom-Channel-Vars)
%%%     {
%%%       "Custom-Field1":"value1",
%%%       "Custom-Field2":"value2",
%%%     },
%%%   ,"response_field": // List of fields should be extracted from CCV list of RADIUS-response
%%%     [
%%%       ["Custom-Channel-Vars","Account-Balance"],
%%%        "Account-Max-Overdraft"
%%%     ]
%%%   ,"channel": "a", "both"
%%% }
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cf_aaa).

%% API
-export([handle/2
         ,maybe_do_radius_request/3
         ,process_response/3]).

-include("../callflow.hrl").

-define(CCV, <<"Custom-Channel-Vars">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:debug("Call is ~p", [Call]),
    lager:debug("Data is ~p", [Data]),
    RequestFields = wh_json:get_value(<<"request_fields">>, Data),
    lager:debug("Request fields: ~p", [RequestFields]),
    % preparing Request structure
    RequestFields1 = [{wh_json:get_value(<<"key">>, JObj), wh_json:get_value(<<"value">>, JObj)} || JObj <- RequestFields],    
    CallProp = whapps_call:to_json(Call),
    CallProp1 = wh_json:delete_key(<<"Key-Value-Store">>, CallProp),
    CallProp2 = wh_json:set_value(<<"AAA-Request-Type">>, <<"cf_aaa">>, CallProp1),
    CallProp3 = wh_json:set_values(RequestFields1, CallProp2),
    CallProp4 = wh_json:set_values(wh_api:default_headers(?APP_NAME, ?APP_VERSION), CallProp3),
    maybe_do_radius_request(CallProp4, Data, Call).

-spec maybe_do_radius_request(wh_json:object(), wh_json:object(), whapps_call:call()) -> any().
maybe_do_radius_request(Request, Data, Call) ->
    lager:debug("Do RADIUS request with next fields: ~p", [Request]),
    case wh_amqp_worker:call_collect(Request
                                     ,fun wapi_aaa:publish_custom_req/1
                                     ,{'circlemaker', fun wapi_aaa:custom_resp_v/1}
                                    )
    of
        {'ok', []} ->
            lager:debug("Error: empty response");
        {'ok', [Response]} ->
            process_response(Response, Data, Call);
        {'returned', _JObj, BR} ->
            lager:debug("Return something: ~p", [BR]);
        {'timeout', Resp} ->
            lager:debug("Timeout: ~p", [Resp]);
        {'error', Error} ->
            lager:debug("Error: ~p", [Error])
    end.

get_last_part_of_key(Key) when is_list(Key) -> lists:last(Key);
get_last_part_of_key(Key) when is_binary(Key) -> Key.

process_response(Response, Data, Call) ->
    lager:debug("Response received: ~p", [Response]),
    ResponseFields = wh_json:get_value(<<"response_fields">>, Data),
    lager:debug("Response fields: ~p", [ResponseFields]),
    Channel = wh_json:get_value(<<"channel">>, Data, <<"a">>),
    % find needed vars in CCV
    FilteredResponse = props:filter_undefined([{get_last_part_of_key(Key), wh_json:get_value(Key, Response)} || Key <- ResponseFields]),
    lager:debug("Filtered response is ~p", [FilteredResponse]),
    % set each var for the call and channel
    Call1 = lists:foldl(
        fun({Name, Value}, AccIn) ->
            lager:debug("Variable with name ~p and value ~p set for the call and channel", [Name, Value]),
            set_variable(Name, Value, Channel, AccIn),
            whapps_call:insert_custom_channel_var(Name, Value, AccIn)
        end, Call, FilteredResponse),
    lager:debug("Update call by new value ~p", [Call1]),
    cf_exe:set_call(Call1),
    lager:debug("Continue..."),
    cf_exe:continue(Call1).

-spec set_variable(api_binary(), api_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
set_variable('undefined', _Value, _Channel, _Call) ->
    lager:warning("Cant set variable w/o name!");
set_variable(_Name, 'undefined', _Channel, _Call) ->
    lager:warning("Cant set variable w/o value!");
set_variable(Name, Value, Channel, Call) ->
    lager:debug("Set ~s/~s pair on ~s-leg", [Name, Value, Channel]),
    Var = wh_json:from_list([{Name, Value}]),
    execute_set_var(Var, Channel, Call).

-spec execute_set_var(wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
execute_set_var(Var, <<"a">>, Call) ->
    whapps_call_command:set(Var, 'undefined', Call);
execute_set_var(Var, <<"both">>, Call) ->
    whapps_call_command:set('undefined', Var, Call).
