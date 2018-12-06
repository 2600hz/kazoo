%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_payment_tokens).

-export([tokens/1
        ,tokens/2
        ]).
-export([defaults/1
        ,default/2
        ]).
-export([update/3
        ,updates/3
        ]).
-export([delete/3]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec tokens(kz_services:services() | kz_term:ne_binary()) ->  kz_json:objects().
tokens(?NE_BINARY = AccountId) ->
    tokens(kz_services:fetch(AccountId));
tokens(Services) ->
    ServicesJObj = kz_services:services_jobj(Services),
    kzd_services:payment_token(ServicesJObj, []).

-spec tokens(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary()) ->  kz_json:objects().
tokens(Thing, ?NE_BINARY = Bookkeeper) ->
    [Token
     || Token <- tokens(Thing)
            ,Bookkeeper =:= kz_json:get_ne_binary_value(<<"bookkeeper">>, Token)
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec defaults(kz_services:services() | kz_term:ne_binary()) ->  kz_json:objects().
defaults(Thing) ->
    [Token
     || Token <- tokens(Thing)
            ,kz_json:get_boolean_value(<<"default">>, Token, 'false')
    ].

-spec default(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:api_object().
default(Thing, Bookkeeper) ->
    case [Token
          || Token <- tokens(Thing, Bookkeeper)
                 ,kz_json:get_boolean_value(<<"default">>, Token, 'false')
         ]
    of
        [First|_] -> First;
        _Else -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_services:service() | kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                    kz_services:services().
update(?NE_BINARY = AccountId, Bookkeeper, Token) ->
    update(kz_services:fetch(AccountId), Bookkeeper, Token);
update(Services, Bookkeeper, Token) ->
    {TokenId, EnsuredToken} = ensure_payment_defaults(Bookkeeper, Token),
    ServicesJObj = kz_services:services_jobj(Services),
    Setters = [{fun kz_services:set_services_jobj/2
               ,kzd_services:set_payment_token(ServicesJObj, TokenId, EnsuredToken)
               }
              ],
    kz_services:setters(Services, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec updates(kz_servics:services() | kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                                   kz_services:services().
updates(?NE_BINARY = AccountId, Bookkeeper, ProposedTokens) ->
    updates(kz_services:fetch(AccountId), Bookkeeper, ProposedTokens);
updates(Services, Bookkeeper, ProposedTokens) ->
    ServicesJObj = kz_services:services_jobj(Services),
    Filtered = kz_json:filter(fun({_, Value}) ->
                                      Bookkeeper =/= kz_json:get_ne_binary_value(<<"bookkeeper">>, Value)
                              end
                             ,kzd_services:payment_tokens(ServicesJObj, kz_json:new())
                             ),
    NewTokens = kz_json:from_list(
                  [ensure_payment_defaults(Bookkeeper, Token)
                   || Token <- ProposedTokens,
                      Bookkeeper =:= kz_json:get_ne_binary_value(<<"bookkeeper">>, Token)
                  ]),
    Setters = [{fun kz_services:set_services_jobj/2
               ,kzd_services:set_payment_tokens(ServicesJObj, kz_json:merge(NewTokens, Filtered))
               }
              ],
    kz_services:setters(Services, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete(kz_services:service() | kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                                  kz_services:services().
delete(?NE_BINARY = AccountId, Bookkeeper, Token) ->
    delete(kz_services:fetch(AccountId), Bookkeeper, Token);
delete(Services, _Bookkeeper, Token) ->
    ServicesJObj = kz_services:services_jobj(Services),
    TokenId = kz_json:get_ne_binary_value(<<"id">>, Token),
    NewTokens = kz_json:delete_key(TokenId, kzd_services:payment_tokens(ServicesJObj, kz_json:new())),
    Setters = [{fun kz_services:set_services_jobj/2
               ,kzd_services:set_payment_tokens(ServicesJObj, NewTokens)
               }
              ],
    kz_services:setters(Services, Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ensure_payment_defaults(kz_term:ne_binary(), kz_json:object()) ->
                                     {kz_term:ne_binary(), kz_json:object()}.
ensure_payment_defaults(?NE_BINARY = Bookkeeper, Token) ->
    Defaults = [{<<"bookkeeper">>, Bookkeeper}
               ,{<<"created">>, kz_time:now_s()}
               ,{<<"default">>, 'false'}
               ,{<<"expiration">>, kz_time:now_s()}
               ,{<<"id">>, kz_binary:rand_hex(5)}
               ,{<<"modified">>, kz_time:now_s()}
               ],
    Updated = kz_json:insert_values(Defaults, Token),
    TokenId = kz_doc:id(Updated),
    lager:debug("update payment token ~s for bookkeeper ~s", [TokenId, Bookkeeper]),
    {TokenId, Updated}.
