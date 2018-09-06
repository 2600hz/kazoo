%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_client_token).

-export([url/0]).
-export([get_client_token/1]).

-define(DEFAULT_TOKEN_VERSION, kapps_config:get_binary(<<"braintree">>, <<"client_token_version">>, <<"2">>)).

-include("braintree.hrl").

%%------------------------------------------------------------------------------
%% @doc Create the partial URL for this module.
%% @end
%%------------------------------------------------------------------------------

-spec url() -> string().

url() ->
    "/client_token/".

%%------------------------------------------------------------------------------
%% @doc Create client Token.
%% @end
%%------------------------------------------------------------------------------

-spec get_client_token(kz_term:ne_binary()) -> kz_term:api_ne_binary().
get_client_token(<<_/binary>> = CustomerId) ->
    Url = url(),
    Props = [{'customer-id', CustomerId}
            ,{'version', ?DEFAULT_TOKEN_VERSION}
            ],
    Request = braintree_util:make_doc_xml(Props, 'client_token'),
    Xml = braintree_request:post(Url, Request),
    kz_xml:get_value(["/client-token", "/value/text()"], Xml).
