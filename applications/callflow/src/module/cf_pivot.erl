%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Accept third-party dialplan
%%% "data":{
%%%   "voice_url":"http://your.pivot.server/path/to/pivot/dialplan"
%%%   ,"cdr_url":"http://your.pivot.server/path/to/cdr/receiver"
%%%   ,"req_format":"kazoo" // or "twiml"
%%%   ,"req_body_format":"form" // or "json"
%%%   ,"method":"get" // or "post"
%%%   ,"debug":false // or true
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_pivot).

-behaviour(gen_cf_action).

-include("callflow.hrl").

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
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    Prop = props:filter_empty(
             [{<<"Call">>, kapps_call:to_json(Call)}
             ,{<<"Voice-URI">>, kz_json:get_ne_binary_value(<<"voice_url">>, Data)}
             ,{<<"CDR-URI">>, kz_json:get_ne_binary_value(<<"cdr_url">>, Data)}
             ,{<<"Request-Format">>, kz_json:get_ne_binary_value(<<"req_format">>, Data)}
             ,{<<"Request-Body-Format">>, kz_json:get_ne_binary_value(<<"req_body_format">>, Data, <<"form">>)}
             ,{<<"HTTP-Method">>, kzt_util:http_method(kz_json:get_value(<<"method">>, Data, 'get'))}
             ,{<<"Debug">>, kz_json:is_true(<<"debug">>, Data, 'false')}
              | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    kapi_pivot:publish_req(Prop),
    lager:info("published pivot request"),
    wait_for_pivot(Data, Call).

-spec wait_for_pivot(kz_json:object(), kapps_call:call()) -> any().
wait_for_pivot(Data, Call) ->
    case kapps_call_command:receive_event(?DEFAULT_EVENT_WAIT, 'true') of
        {'ok', JObj} ->
            case kz_util:get_event_type(JObj) of
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
