%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014 2600Hz, INC
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
-spec available() -> {'ok', wh_json:objects()} | {'error', _}.
available() -> available('undefined').

-spec available(api_binary()) -> {'ok', wh_json:objects()} | {'error', _}.
available(Zone) -> available(Zone, 1).

-spec available(api_binary(), non_neg_integer()) ->{'ok', wh_json:objects()} | {'error', _}.
available(Zone, Quantity) ->
    ViewOptions = props:filter_undefined(
                    [{'key', Zone}
                     ,{'limit', Quantity}
                    ]
                   ),
    case couch_mgr:get_results(?WH_DEDICATED_IP_DB
                               ,<<"dedicated_ips/available_listing">>
                               ,ViewOptions)
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(
              fun() -> kz_ips:available(Zone, Quantity) end
             );
        {'ok', JObjs} ->
            {'ok', [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
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
assigned(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    ViewOptions = [{'key', AccountId}],
    case couch_mgr:get_results(?WH_DEDICATED_IP_DB
                               ,<<"dedicated_ips/assigned_to_listing">>
                               ,ViewOptions)
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(
              fun() -> kz_ips:assigned(Account) end
             );
        {'ok', JObjs} ->
            {'ok', [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
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
-spec zones() -> ne_binaries().
zones() ->
    ViewOptions = [{'group', 'true'}
                   ,{'group_level', 1}
                  ],
    case couch_mgr:get_results(?WH_DEDICATED_IP_DB
                               ,<<"dedicated_ips/zone_listing">>
                               ,ViewOptions)
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(
              fun() -> kz_ips:zones() end
             );
        {'ok', JObjs} ->
            {'ok', [wh_json:get_value(<<"key">>, JObj)
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
-spec hosts() -> ne_binaries().
hosts() ->
    ViewOptions = [{'group', 'true'}
                   ,{'group_level', 1}
                  ],
    case couch_mgr:get_results(?WH_DEDICATED_IP_DB
                               ,<<"dedicated_ips/host_listing">>
                               ,ViewOptions)
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(
              fun() -> kz_ips:hosts() end
             );
        {'ok', JObjs} ->
            {'ok', [wh_json:get_value(<<"key">>, JObj)
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
-spec summary(api_binary()) -> wh_json:objects().
summary(Host) ->
    ViewOptions = props:filter_undefined([{'key', Host}]),
    case couch_mgr:get_results(?WH_DEDICATED_IP_DB
                               ,<<"dedicated_ips/summary_listing">>
                               ,ViewOptions)
    of
        {'error', 'not_found'} ->
            kz_ip_utils:refresh_database(
              fun() -> kz_ips:summary(Host) end
             );
        {'ok', JObjs} -> {'ok', [wh_json:get_value(<<"value">>, JObj) || JObj <- JObjs]};
        {'error', _R}=E ->
            lager:debug("unable to get host ips: ~p", [_R]),
            E
    end.
