%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_db).

-include("fmc.hrl").

%% API
-export([get_fmc_config/0, get_fmc_item/2]).

get_fmc_config() ->
    case couch_mgr:open_doc(?WH_FMC_DB, <<"config">>) of
        {'ok', Doc} ->
            Doc;
        Error ->
            Error
    end.

get_fmc_item(ANumber, ExtractedFMCValue) ->
    lager:debug("Find for an FMC record where A-Number is ~p and FMC Value is ~p...", [ANumber, ExtractedFMCValue]),
    case couch_mgr:get_all_results(?WH_FMC_DB, <<"fmc/numbers_listing">>) of
        {'ok', JObjs} ->
            JObjFMCNumbers = [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs],
            JObjsFiltered = [JObj || JObj <- JObjFMCNumbers,
                (wnm_util:normalize_number(wh_json:get_value(<<"a_number">>, JObj)) == wnm_util:normalize_number(ANumber))
                    andalso (wh_json:get_value(<<"x_fmc_value">>, JObj) == ExtractedFMCValue)],
            case JObjsFiltered of
                [] ->
                    lager:debug("FMC record wasn't found"),
                    {'error', 'not_found'};
                [JObjFiltered] ->
                    lager:debug("FMC record found: ~p", [JObjFiltered]),
                    {'ok', JObjFiltered};
                JObjsFiltered ->
                    lager:debug("Error: more than one FMC record with same data: ", [JObjsFiltered]),
                    {'error', 'found_more_than_one'}
            end;
        Error ->
            lager:debug("Error on find operation: ~p", [Error]),
            Error
    end.
