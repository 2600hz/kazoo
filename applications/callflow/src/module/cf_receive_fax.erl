%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Instructs the switch to receive a fax from the caller
%%% Stores the fax in the database and optionally emails a configured
%%% user(s).
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_receive_fax).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:info("receive fax for owner: ~s", [wh_json:get_value(<<"owner_id">>, Data)]),
    wapi_fax:publish_req(
      props:filter_empty([{<<"Call">>, whapps_call:to_json(Call)}
                         ,{<<"Action">>, <<"receive">>}
                         ,{<<"Owner-ID">>, wh_json:get_value(<<"owner_id">>, Data)}
                         ,{<<"Enable-T38-Fax">>, wh_json:get_value(<<"enable_t38">>, Data)}
                         ,{<<"Enable-T38-Fax-Request">>, wh_json:get_value(<<"enable_t38">>, Data)}
                          | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                         ])
     ),
    cf_exe:control_usurped(Call).
