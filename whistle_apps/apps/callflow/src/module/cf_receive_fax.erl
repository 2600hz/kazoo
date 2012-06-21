%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallJObj = whapps_call:to_json(Call),
    wapi_fax:publish_req([{<<"Call">>, CallJObj}
                          ,{<<"Action">>, <<"receive">>}
                         ]),
    cf_exe:control_usurped(Call).
