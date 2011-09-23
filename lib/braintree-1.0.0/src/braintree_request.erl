%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_request).

-include("braintree.hrl").

-export([get/1, post/2, put/3, delete/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a get request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (Path) -> bt_result() when
      Path :: string().

get(Path) ->
    case do_request(get, Path, <<>>) of
        {ok, _, _, Body} ->
            {ok, Body};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a post request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (Path, Record) -> bt_result() when
      Path :: string(),
      Record :: #bt_customer{}.

post(Path, Record) ->
    {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a put request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec put/2 :: (Path, Record) -> bt_result() when
      Path :: string(),
      Record :: #bt_customer{}.

put(Path, Record) ->
    {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a delete request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (Path) -> bt_result() when
      Path :: string().

delete(Path) ->
    {error, not_implemented}.

do_request(Method, Path, Body) ->
    Config = #bt_config{},
    Url = ["https://"
           ,braintree_server_url(Config#bt_config.environment)
           ,"/merchants/", Config#bt_config.merchant_id
           ,Path],
    Headers = [{"Accept", "application/xml"}
               ,{"User-Agent", "Braintree Erlang Library 1"}
               ,{"X-ApiVersion", wh_util:to_list(?BT_API_VERSION)}
               ,{"Content-Type", "application/xml"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{basic_auth, {Config#bt_config.public_key, Config#bt_config.private_key}}],
    Options = [],
    ibrowse:send_req(Url, Headers, Method, Body, HTTPOptions).

braintree_server_url(Env) ->
    proplists:get_value(Env, ?BT_SERVER_URL).
