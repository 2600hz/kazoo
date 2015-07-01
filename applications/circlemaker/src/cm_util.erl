%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is a set of different utility functions
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_util).

-export([network_address_to_ip_tuple/1, parent_account_id/1]).

-include("circlemaker.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%  Converts a binary string to a IP address tuple
%% @end
%%--------------------------------------------------------------------
-spec network_address_to_ip_tuple(ne_binary()) -> {'ok', {integer(), integer(), integer(), integer()}} |
                                                  {'error', 'einval'}.
network_address_to_ip_tuple(Address) when is_binary(Address) ->
    ResolvedAddressList = wh_network_utils:resolve(Address),
    FirstResolvedAddress = get_first(ResolvedAddressList),
    parse_address(FirstResolvedAddress).

get_first([]) -> {'error', 'unresolved'};
get_first([Address | _Addresses]) -> Address.

parse_address({'error', _} = Err) -> Err;
parse_address(Address) ->
    case inet_parse:ipv4_address(binary_to_list(Address)) of
        {'ok', Res} -> Res;
        Err -> Err
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%  Returns parent account ID. Any Reseller IDs will be skipped.
%% @end
%%--------------------------------------------------------------------
-spec parent_account_id(wh_json:object()) -> api_binary().
parent_account_id(JObj) ->
    case kz_account:parent_account_id(JObj) of
        'undefined' -> <<"system_config">>;
        AccountId -> AccountId
    end.
