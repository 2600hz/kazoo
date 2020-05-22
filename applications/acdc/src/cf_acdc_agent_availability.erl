%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016 Voxter Communications Inc.
%%% @doc Data: {
%%%   "id":"queue id"
%%% }
%%%
%%%
%%% @author Daniel Finke
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_acdc_agent_availability).

-export([handle/2]).

-include_lib("callflow/src/callflow.hrl").

-define(AVAILABLE_BRANCH_KEY, <<"available">>).
-define(UNAVAILABLE_BRANCH_KEY, <<"unavailable">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    QueueId = kz_doc:id(Data),
    Req = props:filter_undefined([{<<"Account-ID">>, kapps_call:account_id(Call)}
                                 ,{<<"Queue-ID">>, QueueId}
                                 ,{<<"Skills">>, kapps_call:kvs_fetch('acdc_required_skills', Call)}
                                  | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                 ]),
    case kz_amqp_worker:call(Req
                            ,fun kapi_acdc_queue:publish_agents_available_req/1
                            ,fun kapi_acdc_queue:agents_available_resp_v/1
                            ) of
        {'error', E} ->
            lager:debug("error ~p when getting agents availability in queue ~s", [E, QueueId]),
            cf_exe:attempt(?AVAILABLE_BRANCH_KEY, Call);
        {'ok', Resp} -> branch_on_availability(kz_json:get_integer_value(<<"Agent-Count">>, Resp), Call)
    end,
    'ok'.

-spec branch_on_availability(non_neg_integer(), kapps_call:call()) -> {'attempt_resp', 'ok' | {'error', 'empty'}}.
branch_on_availability(0, Call) -> cf_exe:attempt(?UNAVAILABLE_BRANCH_KEY, Call);
branch_on_availability(_, Call) -> cf_exe:attempt(?AVAILABLE_BRANCH_KEY, Call).
