%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Kirill Sysoev
%%%-------------------------------------------------------------------
-module(cf_route_to_cid).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This module attempts to lookup endpoints by it's cid number.
%% Returns continue if fails to connect or stop when successful.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case endpoints_lookup(Data, Call) of
        [] ->
            lager:debug("~p not found", [kapps_call:callee_id_number(Call)]),
            cf_exe:continue(Call);
        EndpointJObjs ->
            Vals = [{<<"strategy">>,<<"simultaneous">>}
                   ,{<<"endpoints">>, EndpointJObjs}
                   ,{<<"timeout">>,kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S)}
                   ],
            cf_group:handle(kz_json:set_values(Vals, Data), Call)
    end.

-spec endpoints_lookup(kz_json:object(), kapps_call:call()) -> kz_proplist().
endpoints_lookup(Data, Call) ->
    lists:foldl(fun(CIDType, Acc) -> cid_type_based_lookup(CIDType, Data, Call) ++ Acc end
               ,[]
               ,kz_json:get_value(<<"cid_types">>, Data)
               ).

-spec cid_type_based_lookup(ne_binary(), kz_json:object(), kapps_call:call()) -> kz_proplist().
cid_type_based_lookup(CIDType, Data, Call) ->
    CIDNum = kapps_call:callee_id_number(Call),
    ViewOptions = [{'key', [CIDNum, CIDType]}],
    AccountDb = kapps_call:account_db(Call),
    EndpointTypes = kz_json:get_value(<<"endpoint_types">>, Data, []),
    case kz_datamgr:get_results(AccountDb, <<"attributes/endpoints_lookup">>, ViewOptions) of
        {'ok', JObjs} ->
            [endpoint_format(kz_json:get_ne_binary_value(<<"id">>, JObj)
                            ,kz_json:get_ne_binary_value(<<"value">>, JObj)
                            ,Data
                            )
             || JObj <- JObjs
                    ,kz_json:get_ne_binary_value(<<"id">>, JObj) =/= 'undefined'
                    andalso lists:member(kz_json:get_ne_binary_value(<<"value">>, JObj), EndpointTypes)
            ];
        _E ->
            []
    end.

-spec endpoint_format(ne_binary(), ne_binary(), kz_json:object()) -> kz_json:object().
endpoint_format(EndpointId, EndpointType, Data) ->
    Values = [{<<"endpoint_type">>, EndpointType}
             ,{<<"id">>, EndpointId}
             ,{<<"delay">>,0}
             ,{<<"timeout">>,kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S)}
             ],
    kz_json:from_list(Values).
