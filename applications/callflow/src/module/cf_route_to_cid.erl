%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Kirill Sysoev
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_route_to_cid).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc This module attempts to lookup endpoints by it's cid number.
%% Returns continue if fails to connect or stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case endpoints_lookup(Data, Call) of
        [] ->
            lager:debug("~p not found", [kapps_call:callee_id_number(Call)]),
            cf_exe:continue(Call);
        EndpointJObjs ->
            Vals = [{<<"strategy">>, <<"simultaneous">>}
                   ,{<<"endpoints">>, EndpointJObjs}
                   ,{<<"timeout">>, kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S)}
                   ],
            cf_group:handle(kz_json:set_values(Vals, Data), Call)
    end.

-spec endpoints_lookup(kz_json:object(), kapps_call:call()) -> kz_json:objects().
endpoints_lookup(Data, Call) ->
    cid_type_based_lookup(Data, Call, kz_json:get_list_value(<<"cid_types">>, Data, [])).

-spec cid_type_based_lookup(kz_json:object(), kapps_call:call(), kz_term:ne_binaries()) ->
          kz_json:objects().
cid_type_based_lookup(Data, Call, []) ->
    lager:info("no CID type restrictions"),
    lookup(Data, Call, [{'key', [kapps_call:callee_id_number(Call)]}]);
cid_type_based_lookup(Data, Call, CIDTypes) ->
    Keys = [[kapps_call:callee_id_number(Call), CIDType] || CIDType <- CIDTypes],
    lookup(Data, Call, [{'keys', Keys}]).

-spec lookup(kz_json:object(), kapps_call:call(), kz_datamgr:view_options()) ->
          kz_json:objects().
lookup(Data, Call, ViewOptions) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_results(AccountDb, <<"attributes/endpoints_lookup">>, ViewOptions) of
        {'ok', JObjs} -> maybe_filter_results(Data, JObjs);
        _E -> []
    end.

-spec maybe_filter_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
maybe_filter_results(Data, JObjs) ->
    EndpointTypes = kz_json:get_value(<<"endpoint_types">>, Data, []),
    [endpoint_format(EndpointId, EndpointType, Data)
     || JObj <- JObjs,
        EndpointId <- [kz_json:get_ne_binary_value(<<"id">>, JObj)],
        EndpointType <- [kz_json:get_ne_binary_value(<<"value">>, JObj)],
        'undefined' =/= EndpointId
            andalso ([] =:= EndpointTypes
                     orelse lists:member(EndpointType, EndpointTypes)
                    )
    ].

-spec endpoint_format(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
endpoint_format(EndpointId, EndpointType, Data) ->
    Values = [{<<"endpoint_type">>, EndpointType}
             ,{<<"id">>, EndpointId}
             ,{<<"delay">>, 0}
             ,{<<"timeout">>, kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S)}
             ],
    kz_json:from_list(Values).
