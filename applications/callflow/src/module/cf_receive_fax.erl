%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Instructs the switch to receive a fax from the caller
%%% Stores the fax in the database and optionally emails a configured
%%% user(s).
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cf_receive_fax).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:info("receive fax for owner: ~s", [kz_json:get_value(<<"owner_id">>, Data)]),
    Props = props:filter_undefined(
              props:filter_empty(
                [{<<"Call">>, kapps_call:to_json(Call)}
                ,{<<"Action">>, <<"receive">>}
                ,{<<"Owner-ID">>, kz_json:get_value(<<"owner_id">>, Data)}
                ,{<<"Fax-T38-Option">>, kz_json:get_value([<<"media">>, <<"fax_option">>], Data)}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ])),
    kapi_fax:publish_req(Props),
    cf_exe:control_usurped(Call).
