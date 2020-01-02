%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_response).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Code0 = kz_json:get_integer_value(<<"code">>, Data, 486),
    Code = kz_term:to_binary(Code0),
    Cause = kz_json:get_ne_binary_value(<<"message">>, Data),
    Media = kz_media_util:media_path(kz_json:get_binary_value(<<"media">>, Data)
                                    ,kapps_call:account_id(Call)
                                    ),
    lager:info("responding to call with ~s ~s", [Code, Cause]),
    _ = kapps_call_command:response(Code, Cause, Media, Call),
    cf_exe:stop(Call).
