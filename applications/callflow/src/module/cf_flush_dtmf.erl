%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017 Voxter Communications
%%% @doc Flush DTMF collection on a call.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`collection_name'</dt>
%%%   <dd>The collection to flush.</dd>
%%% </dl>
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_flush_dtmf).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Collection = collection_name(Data),
    cf_exe:continue(kapps_call:set_dtmf_collection('undefined', Collection, Call)).

-spec collection_name(kz_json:object()) -> kz_term:ne_binary().
collection_name(Data) ->
    kz_json:get_ne_binary_value(<<"collection_name">>, Data, <<"default">>).
