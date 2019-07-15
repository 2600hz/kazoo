%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Accept customized `dialplan'.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`function_ref'</dt>
%%%   <dd> a couchDB document Id reference to retrieve Javascript function.</dd>
%%%
%%%   <dt>`language_pack'</dt>
%%%   <dd> the language and version are required for the function runtime.</dd>
%%%
%%%   <dt>`skip_module'</dt>
%%%   <dd>`boolean()'.</dd>
%%%
%%%   <dt>`debug'</dt>
%%%   <dd>`boolean()'</dd>
%%% </dl>
%%%
%%% @author Ming Luo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_function).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2
        ,run_function/2
        ]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%%
%% Expected data payload:
%%   id: string(), the function ID for the account
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    %% add language_pack support for other language and versions.
    FunctionId = kz_json:get_ne_binary_value(<<"id">>, Data),
    case run_function(FunctionId, Call) of
        {'ok', Flow} ->
            maybe_branch(Flow, Call);
        {'error', _E} ->
            lager:info("error for ~s: ~p", [FunctionId, _E]),
            cf_exe:continue(Call)
    end.

-spec run_function(kz_term:ne_binary(), kapps_call:call()) -> {'ok', kz_json:object()} |
                                                              kz_datamgr:data_error().
run_function(FunctionId, Call) ->
    BaseParams = kzt_kazoo:req_params(Call),
    UserParams = kzt_translator:get_user_vars(Call),
    Params = kz_json:set_values(BaseParams, UserParams),

    QueryString = kz_http_util:json_to_querystring(Params),
    kz_datamgr:show(?KZ_FUNCTIONS_DB, <<"functions/wh">>, FunctionId, options(QueryString)).

-spec options(iodata()) -> [{'query_string', binary()}].
options([]) -> [];
options(<<>>) -> [];
options(QueryString) ->
    [{'query_string', kz_term:to_binary(QueryString)}].

-spec maybe_branch(kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_branch(FlowJObj, Call) ->
    case kzd_callflows:validate_flow(
           kzd_callflows:set_flow(kzd_callflows:new(), FlowJObj)
          )
    of
        {'error', Errors} ->
            lager:info("error validating flow: ~p", [Errors]),
            cf_exe:continue(Call);
        {'ok', ValidCallflow} ->
            lager:info("branching callflow to ~p", [ValidCallflow]),
            cf_exe:branch(kzd_callflows:flow(ValidCallflow), Call)
    end.
