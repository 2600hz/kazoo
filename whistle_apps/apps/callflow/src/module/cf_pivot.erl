%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Accept third-party dialplan
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_pivot).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%%
%% Expected data payload:
%%   http_method: "get" | "post"
%%   req_timeout: integer(), defaults to 5 seconds
%%   voice_url: string(), url to get/post to
%%   req_format: string(), data format and payload expected for initial
%%     request (defaults to twiml for the moment),
%%     formats: twiml, kazoo
%%
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:json_object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    Prop = props:filter_empty(
             [{<<"Call">>, whapps_call:to_json(Call)}
              ,{<<"Voice-URI">>, wh_json:get_value(<<"voice_url">>, Data)}
              ,{<<"CDR-URI">>, wh_json:get_value(<<"cdr_url">>, Data)}
              ,{<<"Request-Format">>, wh_json:get_value(<<"req_format">>, Data)}
              ,{<<"HTTP-Method">>, kzt_util:http_method(wh_json:get_atom_value(<<"http_method">>, Data, get))}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ]),
    wapi_pivot:publish_req(Prop),
    cf_exe:control_usurped(Call).
