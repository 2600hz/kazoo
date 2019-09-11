%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cnam_util).

-export([json_to_template_props/1]).

-include("cnam.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec json_to_template_props(kz_term:api_object()) -> 'undefined' | kz_term:proplist().
json_to_template_props('undefined') -> 'undefined';
json_to_template_props(JObj) ->
    normalize_proplist(kz_json:recursive_to_proplist(JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize_proplist(kz_term:proplist()) -> kz_term:proplist().
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

-spec normalize_proplist_element({kz_term:proplist_key(), kz_term:proplist_value()}) ->
                                        {kz_term:proplist_key(), kz_term:proplist_value()}.
normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), kz_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

-spec normalize_value(binary()) -> binary().
normalize_value(Value) ->
    binary:replace(kz_term:to_lower_binary(Value), <<"-">>, <<"_">>, ['global']).
