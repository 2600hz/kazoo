%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016, SIPLABS, LLC
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_edr).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("../callflow.hrl").

-define(SPECIAL_VARS, [<<"Caller-ID-Name">>
                      ,<<"Caller-ID-Number">>
                      ,<<"Account-ID">>
                      ,<<"Owner-ID">>
                      ,<<"Fetch-ID">>
                      ,<<"Bridge-ID">>
                      ,<<"Authorizing-ID">>
                      ,<<"Authorizing-Type">>
                      ,<<"Bridge-ID">>
                      ,<<"Other-Leg-Call-ID">>
                      ]).
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CallProps = kapps_call:to_proplist(Call),
    Tags = [{Key, _Val} || {Key, _Val} <- CallProps, lists:member(Key, ?SPECIAL_VARS)],
    AdditionalTags = [{<<"Custom-Data">>, Data}
                     ,{<<"Callflow-ID">>, props:get_value([<<"Key-Value-Store">>, <<"cf_flow_id">>], CallProps)}
                     ],
    JTags = kz_json:from_list(props:filter_undefined(Tags ++ AdditionalTags)),
    kz_edr:event(?APP_NAME, ?APP_VERSION, 'ok', 'info', JTags, kapps_call:account_id(Call)),
    cf_exe:continue(Call).
