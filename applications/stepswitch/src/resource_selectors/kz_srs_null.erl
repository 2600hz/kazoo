%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_srs_null).

-export([handle_req/5]).

-include("stepswitch.hrl").

-spec handle_req(stepswitch_resources:resources(), kz_term:ne_binary(), kapi_offnet_resource:req(), kz_term:ne_binary(), kz_json:object()) ->
                        stepswitch_resources:resources().
handle_req(_Resources, _Number, _OffnetJObj, _Db, _Params) ->
    [].
