%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_ips).

-include("kazoo_ips.hrl").

-export([available/0
        ,available/1
        ,available/2
        ]).
-export([assigned/1]).
-export([zones/0]).
-export([hosts/0]).
-export([summary/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available() -> {'ok', kz_json:objects()} |
                     {'error', any()}.
-spec available(api_binary()) ->
                       {'ok', kz_json:objects()} |
                       {'error', any()}.

available() -> available('undefined').

available(Zone) -> available(Zone, 1).

-spec available(api_binary(), non_neg_integer()) ->
                       {'ok', kz_json:objects()} |
                       {'error', any()}.
available(Zone, Quantity) ->
    ViewOptions = props:filter_undefined(
                    [{'key', Zone}
                    ,{'limit', Quantity}
                    ]
                   ),
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/available_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> available(Zone, Quantity) end);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
        {'error', _R}=E ->
            lager:debug("unable to get available dedicated ips: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec assigned(ne_binary()) ->
                      {'ok', kz_json:objects()} |
                      {'error', any()}.
assigned(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ViewOptions = [{'key', AccountId}
                  ],
    View = <<"dedicated_ips/assigned_to_listing">>,
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB, View, ViewOptions) of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> assigned(Account) end);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
        {'error', _R}=E ->
            lager:debug("unable to get assigned dedicated ips: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec zones() ->
                   {'ok', ne_binaries()} |
                   {'error', any()}.
zones() ->
    ViewOptions = ['group'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/zone_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun zones/0);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"key">>, JObj)
                    || JObj <- JObjs
                   ]};
        {'error', _R}=E ->
            lager:debug("unable to get zones: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec hosts() ->
                   {'ok', ne_binaries()} |
                   {'error', any()}.
hosts() ->
    ViewOptions = ['group'
                  ,{'group_level', 1}
                  ],
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/host_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun hosts/0);
        {'ok', JObjs} ->
            {'ok', [kz_json:get_value(<<"key">>, JObj)
                    || JObj <- JObjs
                   ]};
        {'error', _R}=E ->
            lager:debug("unable to get hosts: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec summary(api_binary()) ->
                     {'ok', kz_json:objects()} |
                     {'error', any()}.
summary(Host) ->
    ViewOptions = props:filter_undefined([{'key', Host}]),
    case kz_datamgr:get_results(?KZ_DEDICATED_IP_DB
                               ,<<"dedicated_ips/summary_listing">>
                               ,ViewOptions
                               )
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(fun() -> summary(Host) end);
        {'ok', JObjs} -> {'ok', [kz_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
        {'error', _R}=E ->
            lager:debug("unable to get host ips: ~p", [_R]),
            E
    end.
