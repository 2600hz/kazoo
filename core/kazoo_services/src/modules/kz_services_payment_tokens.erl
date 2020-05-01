%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_payment_tokens).

-export([fetch/1
        ,fetch/2
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
-spec fetch(kz_services:services() | kz_term:ne_binary()) ->  kz_json:object().
fetch(?NE_BINARY = AccountId) ->
    fetch(kz_services:fetch(AccountId));
fetch(Services) ->
    ServicesJObj = kz_services:services_jobj(Services),
    kzd_services:payment_tokens(ServicesJObj, kz_json:new()).

-spec fetch(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary()) ->  kz_json:object().
fetch(Thing, ?NE_BINARY = Bookkeeper) ->
    kz_json:filter(fun({_TokenId, Token}) ->
                           Bookkeeper =:= kz_json:get_ne_binary_value(<<"bookkeeper">>, Token)
                   end
                  ,fetch(Thing)
                  ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec defaults(kz_services:services() | kz_term:ne_binary()) ->  kz_json:object().
defaults(Thing) ->
    kz_json:filter(fun({_TokenId, Token}) ->
                           kz_json:is_true(<<"default">>, Token)
                   end
                  ,fetch(Thing)
                  ).

-spec default(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
default(Thing, Bookkeeper) ->
    Now = kz_time:now_s(),
    Pays = kz_json:filter(fun({_TokenId, Token}) ->
                                  kz_json:is_true(<<"default">>, Token)
                                      andalso kz_json:get_integer_value(<<"expiration">>, Token, Now) > Now
                          end
                         ,fetch(Thing, Bookkeeper)
                         ),
    case kz_json:is_empty(Pays) of
        'true' -> 'undefined';
        'false' ->
            [TokenId|_] = kz_json:get_keys(Pays),
            kz_json:get_ne_json_value(TokenId, Pays)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
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
-spec updates(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary(), kz_json:objects()) ->
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
-spec delete(kz_services:services() | kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object() | kz_term:ne_binary()) ->
          kz_services:services().
delete(<<AccountId/binary>>, Bookkeeper, Token) ->
    delete(kz_services:fetch(AccountId), Bookkeeper, Token);
delete(Services, _Bookkeeper, ?NE_BINARY = TokenId) ->
    ServicesJObj = kz_services:services_jobj(Services),
    NewTokens = kz_json:delete_key(TokenId, kzd_services:payment_tokens(ServicesJObj, kz_json:new())),
    Setters = [{fun kz_services:set_services_jobj/2
               ,kzd_services:set_payment_tokens(ServicesJObj, NewTokens)
               }
              ],
    kz_services:setters(Services, Setters);
delete(Services, Bookkeeper, TokenJObj) ->
    case kz_json:is_json_object(TokenJObj) of
        'true' ->
            delete(Services, Bookkeeper, kz_json:get_ne_binary_value(<<"id">>, TokenJObj));
        'false' ->
            Services
    end.

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
