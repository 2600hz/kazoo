%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Instructs the switch to receive a fax from the caller
%%% Stores the fax in the database and optionally emails a configured
%%% user(s).
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cf_faxbox).

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
    FaxboxId = get_faxbox_id(Data),
    lager:info("receive fax for faxbox: ~s", [FaxboxId]),
    Props = props:filter_undefined(
              props:filter_empty([{<<"Call">>, whapps_call:to_json(Call)}
                               ,{<<"Action">>, <<"receive">>}
                               ,{<<"FaxBox-ID">>, FaxboxId}
                                | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                               ])),
    wapi_fax:publish_req(Props),
    cf_exe:control_usurped(Call).

-spec get_faxbox_id(wh_json:object()) -> api_binary().
get_faxbox_id(JObj) ->
    wh_json:get_first_defined([<<"id">>, <<"faxbox_id">>], JObj).
