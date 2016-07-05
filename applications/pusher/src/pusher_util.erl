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

%% ====================================================================
%% Internal functions
%% ====================================================================

-type keycert() :: {'undefined' | {'PrivateKeyInfo', binary()}
                   ,api_binary()
                   }.
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
keycert_fold(_Entry, KeyCert) ->
    KeyCert.

-spec user_agent_push_properties(ne_binary()) -> api_object().
-spec user_agent_push_properties(ne_binary(), kz_json:objects()) -> api_object().
user_agent_push_properties(UserAgent) ->
    UAs = kapps_config:get(?CONFIG_CAT, <<"User-Agents">>, kz_json:new()),
    {Vs, _Ks} = kz_json:get_values(UAs),
    user_agent_push_properties(UserAgent, Vs).

user_agent_push_properties(_UserAgent, []) -> 'undefined';
user_agent_push_properties(UserAgent, [JObj|UAs]) ->
    case re:run(UserAgent, kz_json:get_value(<<"regex">>, JObj, <<"^$">>)) of
        'nomatch' -> user_agent_push_properties(UserAgent, UAs);
                                                                  _ -> kz_json:get_value(<<"properties">>, JObj, kz_json:new())
                                                                end.
