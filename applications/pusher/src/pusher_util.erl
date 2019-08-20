%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pusher_util).

%%==============================================================================
%% API functions
%%==============================================================================

-export([binary_to_keycert/1]).
-export([user_agent_push_properties/1]).

-include("pusher.hrl").

-type pki_asn1_type() :: 'Certificate' |
                         'RSAPrivateKey'| 'RSAPublicKey' |
                         'DSAPrivateKey' | 'DSAPublicKey' |
                         'DHParameter' | 'SubjectPublicKeyInfo'|
                         'PrivateKeyInfo' | 'CertificationRequest'.

-type pem_entry() :: {pki_asn1_type()
                     ,binary()
                     ,'not_encrypted' | cipher_info()
                     }.

-type cipher_info() :: any().
%% {"RC2-CBC" | "DES-CBC" | "DES-EDE3-CBC"
%%  ,binary() | 'PBES2-params'
%% }.

-type keycert() :: {'undefined' |
                    {'PrivateKeyInfo' | 'RSAPrivateKey', binary()}
                   ,kz_term:api_binary()
                   }.
-export_type([keycert/0]).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec binary_to_keycert(binary()) -> keycert().
binary_to_keycert(Binary) ->
    RSAEntries = public_key:pem_decode(Binary),
    keycert(RSAEntries).

-spec keycert([pem_entry()]) -> keycert().
keycert(RSAEntries) ->
    lists:foldl(fun keycert_fold/2, {'undefined', 'undefined'}, RSAEntries).

-spec keycert_fold(pem_entry(), keycert()) -> keycert().
keycert_fold({'Certificate', Bin, 'not_encrypted'}, {Key, 'undefined'}) ->
    {Key, Bin};
keycert_fold({'PrivateKeyInfo', Bin, 'not_encrypted'}, {'undefined', Cert}) ->
    {{'PrivateKeyInfo', Bin}, Cert};
keycert_fold({'RSAPrivateKey', Bin, 'not_encrypted'}, {'undefined', Cert}) ->
    {{'RSAPrivateKey', Bin}, Cert};
keycert_fold(_Entry, KeyCert) ->
    KeyCert.

-spec user_agent_push_properties(kz_term:ne_binary()) -> kz_term:api_object().
user_agent_push_properties(UserAgent) ->
    UAs = kapps_config:get_json(?CONFIG_CAT, <<"User-Agents">>, kz_json:new()),
    user_agent_push_properties(UserAgent, kz_json:values(UAs)).

-spec user_agent_push_properties(kz_term:ne_binary(), kz_json:objects()) -> kz_term:api_object().
user_agent_push_properties(_UserAgent, []) -> 'undefined';
user_agent_push_properties(UserAgent, [JObj|UAs]) ->
    case re:run(UserAgent, kz_json:get_value(<<"regex">>, JObj, <<"^\$">>)) of
        'nomatch' -> user_agent_push_properties(UserAgent, UAs);
        _ ->
            kz_json:get_value(<<"properties">>, JObj, kz_json:new())
    end.
