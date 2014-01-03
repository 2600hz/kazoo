%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wnm_vitelity).

-export([find_numbers/3
         ,acquire_number/1
         ,disconnect_number/1
        ]).

-include("../wnm.hrl").

-define(WNM_VITELITY_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".vitelity">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', wh_json:object()} |
                          {'error', term()}.
find_numbers(Number, Quanity, Opts) ->
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{auth_by=AuthBy
                       ,assigned_to=AssignedTo
                       ,module_data=Data
                      }=N) ->
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.
