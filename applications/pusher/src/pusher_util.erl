%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(pusher_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([binary_to_keycert/1]).
-export([user_agent_push_properties/1]).

-include("pusher.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec binary_to_keycert(binary()) -> {term(), term()}.
binary_to_keycert(Binary) ->
    RSAEntries = public_key:pem_decode(Binary),
    [Cert] =  [Bin || {'Certificate', Bin, 'not_encrypted'} <- RSAEntries],
    [Key] =  [{'PrivateKeyInfo', Bin} || {'PrivateKeyInfo', Bin, 'not_encrypted'} <- RSAEntries],
    {Key, Cert}.

-spec user_agent_push_properties(binary()) -> api_object().
user_agent_push_properties(UserAgent) ->
    UAs = whapps_config:get(?CONFIG_CAT, <<"User-Agents">>, wh_json:new()),
    user_agent_push_properties(UserAgent, wh_json:to_proplist(UAs)).

user_agent_push_properties(_UserAgent, []) ->
    'undefined';
user_agent_push_properties(UserAgent, [{_UA, JObj}|UAs]) ->
    case re:run(UserAgent, wh_json:get_value(<<"regex">>, JObj, <<"^$">>)) of
        'nomatch' -> user_agent_push_properties(UserAgent, UAs);
        _ -> wh_json:get_value(<<"properties">>, JObj, wh_json:new())
    end.
