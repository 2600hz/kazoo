%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_other).

-export([find_numbers/3]).
-export([check_numbers/2]).
-export([is_number_billable/1]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([should_lookup_cnam/0]).

-include("../wnm.hrl").

-define(DEFAULT_COUNTRY, <<"US">>).
-define(WNM_OTHER_CONFIG_CAT, <<"number_manager.other">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'error', _} |
                          wh_json:objects().
find_numbers(Number, Quantity, Props) ->
    case whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' -> {'error', 'non_available'};
        Url ->
            case props:get_value(<<"blocks">>, Props) of
                'undefined' ->
                    get_numbers(Url, Number, Quantity, Props);
                _ ->
                    get_blocks(Url, Number, Quantity, Props)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec check_numbers(ne_binaries(), wh_proplist()) -> {'error', any()} | {'ok', any()}.
check_numbers(Numbers, _Props) ->
    FormatedNumbers = [wnm_util:to_npan(Number) || Number <- Numbers],
    case whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            {'error', 'non_available'};
        Url ->
            DefaultCountry = whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
            ReqBody = wh_json:set_value(<<"data">>, FormatedNumbers, wh_json:new()),
            Uri = <<Url/binary,  "/numbers/", DefaultCountry/binary, "/status">>,
            lager:debug("making request to ~s with body ~p", [Uri, ReqBody]),
            case ibrowse:send_req(binary:bin_to_list(Uri), [], 'post', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    lager:error("numbers check failed: ~p", [Reason]),
                    {'error', Reason};
                {'ok', "200", _Headers, Body} ->
                    format_check_numbers(wh_json:decode(Body));
                {'ok', _Status, _Headers, Body} ->
                    lager:error("numbers check failed: ~p", [Body]),
                    {'error', Body}
            end
    end.

-spec format_check_numbers(wh_json:object()) ->
                                  {'error', _} |
                                  {'ok', _}.
format_check_numbers(Body) ->
    case wh_json:get_value(<<"status">>, Body) of
        <<"success">> ->
            JObj =  lists:foldl(
                        fun(NumberJObj, Acc) ->
                            Number = wh_json:get_value(<<"number">>, NumberJObj),
                            Status = wh_json:get_value(<<"status">>, NumberJObj),
                            wh_json:set_value(Number, Status, Acc)
                        end
                        ,wh_json:new()
                        ,wh_json:get_value(<<"data">>, Body, [])
                    ),
            {'ok', JObj};
        _Error ->
            lager:error("number check resp error: ~p", [_Error]),
            {'error', 'resp_error'}
    end.



%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(wnm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{dry_run='true'}=Number) -> Number;
acquire_number(#number{number=Num}=Number) ->
    DefaultCountry = whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    case whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"phonebook_url">>) of
        'undefined' ->
            Error = <<"Unable to acquire numbers missing provider url">>,
            wnm_number:error_carrier_fault(Error, Number);
        Url ->
            Hosts = case whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"endpoints">>) of
                        'undefined' -> [];
                        Endpoint when is_binary(Endpoint) ->
                            [Endpoint];
                        Endpoints -> Endpoints
                    end,

            ReqBody = wh_json:set_values([{[<<"data">>, <<"numbers">>], [Num]}
                                          ,{[<<"data">>, <<"gateways">>], Hosts}
                                         ]
                                         ,wh_json:new()
                                        ),

            Uri = <<Url/binary,  "/numbers/", DefaultCountry/binary, "/order">>,
            case ibrowse:send_req(wh_util:to_list(Uri), [], 'put', wh_json:encode(ReqBody)) of
                {'error', Reason} ->
                    lager:error("number lookup failed to ~s: ~p", [Uri, Reason]),
                    wnm_number:error_carrier_fault(Reason, Number);
                {'ok', "200", _Headers, Body} ->
                    format_acquire_resp(wh_json:decode(Body), Number);
                {'ok', _Status, _Headers, Body} ->
                    lager:error("number lookup failed to ~s with ~s: ~s", [Uri, _Status, Body]),
                    wnm_number:error_carrier_fault(Body, Number)
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.

-spec get_numbers(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                         {'error', 'non_available'} |
                         wh_json:objects().
get_numbers(Url, Number, Quantity, Props) ->
    Offset = props:get_value(<<"offset">>, Props, <<"0">>),
    Country = whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    ReqBody = <<"?prefix=", Number/binary, "&limit=", Quantity/binary, "&offset=", Offset/binary>>,
    Uri = <<Url/binary, "/numbers/", Country/binary, "/search", ReqBody/binary>>,
    case ibrowse:send_req(wh_util:to_list(Uri), [], 'get') of
        {'error', _Reason} ->
            lager:error("number lookup error to ~s: ~p", [Uri, _Reason]),
            {'error', 'non_available'};
        {'ok', "200", _Headers, Body} ->
            format_numbers_resp(wh_json:decode(Body));
        {'ok', _Status, _Headers, _Body} ->
            lager:error("number lookup failed to ~s with ~s: ~s", [Uri, _Status, _Body]),
            {'error', 'non_available'}
    end.

-spec format_numbers_resp(wh_json:object()) ->
                                 {'error', 'non_available'} |
                                 wh_json:objects().
format_numbers_resp(JObj) ->
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            {'ok', wh_json:get_value(<<"data">>, JObj)};
        _Error ->
            lager:error("block lookup resp error: ~p", [_Error]),
            {'error', 'non_available'}
    end.

-spec get_blocks(ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                        {'error', 'non_available'} |
                        wh_json:objects().
get_blocks(Url, Number, Quantity, Props) ->
    Offset = props:get_value(<<"offset">>, Props, <<"0">>),
    Limit = props:get_value(<<"blocks">>, Props, <<"0">>),
    Country = whapps_config:get(?WNM_OTHER_CONFIG_CAT, <<"default_country">>, ?DEFAULT_COUNTRY),
    ReqBody = <<"?prefix=", (wh_util:uri_encode(Number))/binary
                            ,"&size=", Quantity/binary
                            ,"&offset=", Offset/binary
                            ,"&limit=", Limit/binary>>,
    Uri = <<Url/binary, "/blocks/", Country/binary, "/search", ReqBody/binary>>,
    lager:debug("making request to ~s", [Uri]),
    case ibrowse:send_req(binary:bin_to_list(Uri), [], 'get') of
        {'error', Reason} ->
            lager:error("block lookup error: ~p", [Reason]),
            {'error', 'non_available'};
        {'ok', "200", _Headers, Body} ->
            format_blocks_resp(wh_json:decode(Body));
        {'ok', _Status, _Headers, Body} ->
            lager:error("block lookup failed: ~p ~p", [_Status, Body]),
            {'error', 'non_available'}
    end.

-spec format_blocks_resp(wh_json:object()) ->
                                {'error', 'non_available'} |
                                wh_json:object() |
                                wh_json:objects().
format_blocks_resp(JObj) ->
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            Numbers = lists:foldl(fun format_blocks_resp_fold/2
                                 ,[]
                                 ,wh_json:get_value(<<"data">>, JObj, [])
                                 ),
            {'bulk', Numbers};
        _Error ->
            lager:error("block lookup resp error: ~p", [JObj]),
            {'error', 'non_available'}
    end.

-spec format_blocks_resp_fold(wh_json:object(), wh_json:objects()) -> wh_json:objects().
format_blocks_resp_fold(JObj, Numbers) ->
    StartNumber = wnm_util:to_e164(wh_json:get_value(<<"start_number">>, JObj)),
    EndNumber = wnm_util:to_e164(wh_json:get_value(<<"end_number">>, JObj)),
    Props = [{<<"start_number">>, StartNumber}
            ,{<<"end_number">>, EndNumber}
            ],
    [wh_json:set_values(Props, JObj) | Numbers].

-spec format_acquire_resp(wh_json:object(), wnm_number()) -> wnm_number().
format_acquire_resp(Body, #number{number=Num}=Number) ->
    JObj = wh_json:get_value([<<"data">>, Num], Body, wh_json:new()),
    case wh_json:get_value(<<"status">>, JObj) of
        <<"success">> ->
            Routines = [fun maybe_merge_opaque/2
                        ,fun maybe_merge_locality/2
                       ],
            lists:foldl(fun(F, N) -> F(JObj, N) end, Number, Routines);
        Error ->
            lager:error("number lookup resp error: ~p", [Error]),
            wnm_number:error_carrier_fault(Error, Number)
    end.

-spec maybe_merge_opaque(wh_json:object(), wnm_number()) -> wnm_number().
maybe_merge_opaque(JObj, Number) ->
    case wh_json:get_ne_value(<<"opaque">>, JObj) of
        'undefined' -> Number;
        Opaque ->
            Number#number{module_data=Opaque}
    end.

-spec maybe_merge_locality(wh_json:object(), wnm_number()) -> wnm_number().
maybe_merge_locality(JObj, #number{number_doc=Doc}=Number) ->
    case wh_json:get_ne_value(<<"locality">>,  JObj) of
        'undefined' -> Number;
        Locality ->
            Updated = wh_json:set_value(<<"locality">>, Locality, Doc),
            Number#number{number_doc=Updated}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
