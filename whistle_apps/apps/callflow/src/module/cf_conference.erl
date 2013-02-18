%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_conference).

-include("./src/callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Command = [{<<"Call">>, whapps_call:to_json(Call)}
               ,{<<"Conference-ID">>, wh_json:get_value(<<"id">>, Data)}
               ,{<<"Moderator">>, wh_json:get_binary_boolean(<<"moderator">>, Data)}
               | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],
    wapi_conference:publish_discovery_req(Command).
