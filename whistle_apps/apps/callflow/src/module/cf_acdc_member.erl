%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Data: {
%%%   "id":"queue id"
%%% }
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_acdc_member).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),
    whapps_call_command:hold(Call),
    MemberCall = props:filter_undefined(
                   [{<<"Account-ID">>, whapps_call:account_id(Call)}
                    ,{<<"Queue-ID">>, wh_json:get_value(<<"id">>, Data)}
                    ,{<<"Call">>, whapps_call:to_json(Call)}
                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                   ]),
    wapi_acdc_queue:publish_member_call(MemberCall),
    cf_exe:stop(Call).
