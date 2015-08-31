%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Accept third-party dialplan
%%% "data":{
%%%   "voice_url":"http://your.pivot.server/path/to/pivot/dialplan"
%%%   ,"cdr_url":"http://your.pivot.server/path/to/cdr/receiver"
%%%   ,"req_format":"kazoo" // or "twiml"
%%%   ,"method":"get" // or "post"
%%%   ,"debug":false // or true
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_pivot).

-include("../callflow.hrl").

-export([handle/2]).

-define(DEFAULT_EVENT_WAIT, 10000).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%%
%% Expected data payload:
%%   http_method: "get" | "post"
%%   voice_url: string(), url to get/post to
%%   req_format: string(), data format and payload expected for initial
%%     request (defaults to twiml for the moment),
%%     formats: twiml, kazoo
%%   cdr_url: string(), url to POST the CDR
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    Prop = props:filter_empty(
             [{<<"Call">>, whapps_call:to_json(Call)}
              ,{<<"Voice-URI">>, maybe_use_variable(Data, Call)}
              ,{<<"CDR-URI">>, wh_json:get_value(<<"cdr_url">>, Data)}
              ,{<<"Request-Format">>, wh_json:get_value(<<"req_format">>, Data)}
              ,{<<"HTTP-Method">>, kzt_util:http_method(wh_json:get_value(<<"method">>, Data, 'get'))}
              ,{<<"Debug">>, wh_json:is_true(<<"debug">>, Data, 'false')}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_pivot:publish_req(Prop),
    lager:info("published pivot request"),
    wait_for_pivot(Data, Call).

-spec maybe_use_variable(wh_json:object(), whapps_call:call()) -> api_binary().
maybe_use_variable(Data, Call) ->
    case wh_json:get_value(<<"var">>, Data) of
        'undefined' ->
            wh_json:get_value(<<"voice_url">>, Data);
        Variable ->
            wh_json:get_value(<<"value">>, cf_kvs_set:get_kv(Variable, Call))
    end.

-spec wait_for_pivot(wh_json:object(), whapps_call:call()) -> any().
wait_for_pivot(Data, Call) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT, 'true') of
        {'ok', JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"call_event">>,<<"CHANNEL_DESTROY">>} ->
                    lager:debug("CHANNEL_DESTROY received stoping call"),
                    cf_exe:stop(Call);
                {<<"pivot">>,<<"failed">>} ->
                    lager:warning("pivot failed failing back to next callflow action"),
                    cf_exe:continue(Call);
                _ ->
                    wait_for_pivot(Data, Call)
            end;
        {'error', 'timeout'} ->
            wait_for_pivot(Data, Call)
    end.
