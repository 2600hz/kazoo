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
-include_lib("xmerl/include/xmerl.hrl").

-export([get/1, post/2, put/2, delete/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a get request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (Path) -> bt_result() when
      Path :: string().
get(Path) ->
    do_request(get, Path, <<>>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a post request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (Path, Request) -> bt_result() when
      Path :: string(),
      Request :: #bt_customer{}.
post(Path, Request) ->
    do_request(post, Path, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a put request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec put/2 :: (Path, Request) -> bt_result() when
      Path :: string(),
      Request :: #bt_customer{}.
put(Path, Request) ->
    do_request(put, Path, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a delete request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (Path) -> bt_result() when
      Path :: string().
delete(Path) ->
    do_request(delete, Path, <<>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Preform a request to the braintree service
%% @end
%%--------------------------------------------------------------------
-spec do_request/3 :: (Method, Path, Body) -> term() when
      Method :: atom(),
      Path :: string(),
      Body :: binary().
do_request(Method, Path, Body) ->
    io:format("~s ~s~n~s~n", [Method, Path, Body]),
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
    case ibrowse:send_req(lists:flatten(Url), Headers, Method, Body, HTTPOptions) of
        {ok, "401", _, _} ->
            {error, authentication};
        {ok, "403", _, Response} ->
            {error, authorization};
        {ok, "404", _, _} ->
            {error, not_found};
        {ok, "426", _, _} ->
            {error, upgrade_required};
        {ok, "500", _, _} ->
            {error, server_error};
        {ok, "503", _, _} ->
            {error, maintenance};
        {ok, _, _, [$<,$?,$x,$m,$l|_]=Response} ->
            file:write_file("/tmp/braintree.xml", Response),
            {Xml, _} = xmerl_scan:string(Response),
            verify_response(Xml);
        {ok, _, _, [$<,$s,$e,$a,$r,$c,$h|_]=Response} ->
            file:write_file("/tmp/braintree.xml", Response),
            {Xml, _} = xmerl_scan:string(Response),
            verify_response(Xml);
        {ok, _, _, Response} ->
            file:write_file("/tmp/braintree.xml", Response),
            {ok, ?BT_EMPTY_XML};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the base URL for the braintree service
%% @end
%%--------------------------------------------------------------------
-spec braintree_server_url/1 :: (Env) -> string() when
      Env :: string().
braintree_server_url(Env) ->
    proplists:get_value(Env, ?BT_SERVER_URL).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the response was valid
%% @end
%%--------------------------------------------------------------------
-spec verify_response/1 :: (Xml) -> tuple(ok, bt_xml()) | tuple(error, term()) when
      Xml :: bt_xml().
verify_response(Xml) ->
    case xmerl_xpath:string("//api-error-response", Xml) of
        [] ->
            {ok, Xml};
        _ ->
            Errors = [{braintree_utils:get_xml_value("/error/code/text()", Error)
                       ,braintree_utils:get_xml_value("/error/message/text()", Error)
                       ,braintree_utils:get_xml_value("/error/attribute/text()", Error)}
                     || Error <- xmerl_xpath:string("/api-error-response/errors/*/errors/error", Xml)],
            {error, Errors}
    end.
