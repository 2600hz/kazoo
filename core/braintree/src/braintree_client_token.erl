%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(braintree_client_token).

-export([url/0]).
-export([get_client_token/1]).

-define(DEFAULT_TOKEN_VERSION, kapps_config:get_binary(<<"braintree">>, <<"client_token_version">>, <<"2">>)).

-include("bt.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------
-spec url() -> string().

url() ->
    "/client_token/".

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create client Token
%% @end
%%--------------------------------------------------------------------
-spec get_client_token(ne_binary()) -> api_ne_binary().
get_client_token(<<_/binary>> = CustomerId) ->
    Url = url(),
    Props = [{'customer-id', CustomerId}
            ,{'version', ?DEFAULT_TOKEN_VERSION}
            ],
    Request = braintree_util:make_doc_xml(Props, 'client_token'),
    Xml = braintree_request:post(Url, Request),
    kz_util:get_xml_value(["/client-token", "/value/text()"], Xml).
