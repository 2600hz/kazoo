%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_request).

-include_lib("braintree/include/braintree.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([get/1, post/2, put/2, delete/1]).

-type error_types() :: 'authentication' | 'authorization' | 'not_found' | 'upgrade_required' | 'server_error' | 'maintenance' | #bt_api_error{}.
-type do_request_ret() :: {'error', error_types()} | {'ok', bt_xml()}.

-export_type([error_types/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a get request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (nonempty_string()) -> do_request_ret().
get(Path) ->
    do_request(get, Path, <<>>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a post request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (nonempty_string(), ne_binary()) -> do_request_ret().
post(Path, Request) ->
    do_request(post, Path, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a put request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec put/2 :: (nonempty_string(), ne_binary()) -> do_request_ret().
put(Path, Request) ->
    do_request(put, Path, Request).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Preform a delete request to braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (nonempty_string()) -> do_request_ret().
delete(Path) ->
    do_request(delete, Path, <<>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Preform a request to the braintree service
%% @end
%%--------------------------------------------------------------------

-spec do_request/3 :: ('put' | 'post' | 'get' | 'delete', nonempty_string(), binary()) -> do_request_ret().
do_request(Method, Path, Body) ->
    StartTime = erlang:now(),
    lager:debug("making ~s request to braintree ~s", [Method, Path]),
    Url = lists:flatten(["https://"
                         ,braintree_server_url(whapps_config:get_string(<<"braintree">>, <<"default_environment">>, <<>>))
                         ,"/merchants/", whapps_config:get_string(<<"braintree">>, <<"default_merchant_id">>, <<>>)
                         ,Path
                        ]),
    Headers = [{"Accept", "application/xml"}
               ,{"User-Agent", "Braintree Erlang Library 1"}
               ,{"X-ApiVersion", wh_util:to_list(?BT_API_VERSION)}
               ,{"Content-Type", "application/xml"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{basic_auth, {whapps_config:get_string(<<"braintree">>, <<"default_public_key">>, <<>>)
                                  ,whapps_config:get_string(<<"braintree">>, <<"default_private_key">>, <<>>)}}
                  ],
    ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                      ,io_lib:format("Request:~n~s ~s~n~s~n", [Method, Url, Body])),
    case ibrowse:send_req(Url, Headers, Method, Body, HTTPOptions) of
        {ok, "401", _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n401~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("braintree error response(~pms): 401 Unauthenticated", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            {error, authentication};
        {ok, "403", _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("braintree error response(~pms): 403 Unauthorized", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            {error, authorization};
        {ok, "404", _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("braintree error response(~pms): 404 Not Found", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            {error, not_found};
        {ok, "426", _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n426~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("braintree error response(~pms): 426 Upgrade Required", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            {error, upgrade_required};
        {ok, "500", _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("braintree error response(~pms): 500 Server Error", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            {error, server_error};
        {ok, "503", _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("braintree error response(~pms): 503 Maintenance", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            {error, maintenance};
        {ok, Code, _, [$<,$?,$x,$m,$l|_]=Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                              ,[append]),
            {Xml, _} = xmerl_scan:string(Response),
            lager:debug("braintree xml response(~pms)", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            verify_response(Xml);
        {ok, Code, _, [$<,$s,$e,$a,$r,$c,$h|_]=Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                              ,[append]),
            {Xml, _} = xmerl_scan:string(Response),
            lager:debug("braintree xml response(~pms)", [timer:now_diff(erlang:now(), StartTime) div 1000]),
            verify_response(Xml);
        {ok, Code, _, _Response} ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, _Response])
                                              ,[append]),
            lager:debug("braintree empty response(~pms): ~p", [timer:now_diff(erlang:now(), StartTime) div 1000, Code]),
            {ok, ?BT_EMPTY_XML};
        {error, _}=E ->
            ?BT_DEBUG andalso file:write_file("/tmp/braintree.xml"
                                              ,io_lib:format("Response:~nerror~n~p~n", [E])
                                              ,[append]),
            lager:debug("braintree request error(~pms): ~p", [timer:now_diff(erlang:now(), StartTime) div 1000, E]),
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
-spec verify_response/1 :: (Xml) -> {'ok', Xml} | {'error', #bt_api_error{}}.

verify_response(Xml) ->
    case xmerl_xpath:string("/api-error-response", Xml) of
        [] ->
            lager:debug("braintree affirmative response"),
            {ok, Xml};
        _ ->
            lager:debug("braintree api error response"),
            Errors = [#bt_error{code = braintree_util:get_xml_value("/error/code/text()", Error)
                                ,message = braintree_util:get_xml_value("/error/message/text()", Error)
                                ,attribute = braintree_util:get_xml_value("/error/attribute/text()", Error)}
                      || Error <- xmerl_xpath:string("/api-error-response/errors//errors/error", Xml)],
            Verif = #bt_verification{verification_status =
                                         braintree_util:get_xml_value("/api-error-response/verification/status/text()", Xml)
                                     ,processor_response_code =
                                         braintree_util:get_xml_value("/api-error-response/verification/processor-response-code/text()", Xml)
                                     ,processor_response_text =
                                         braintree_util:get_xml_value("/api-error-response/verification/processor-response-text/text()", Xml)
                                     ,cvv_response_code =
                                         braintree_util:get_xml_value("/api-error-response/verification/cvv-response-code/text()", Xml)
                                     ,avs_response_code =
                                         braintree_util:get_xml_value("/api-error-response/verification/avs-error-response-code/text()", Xml)
                                     ,postal_response_code =
                                         braintree_util:get_xml_value("/api-error-response/verification/avs-postal-code-response-code/text()", Xml)
                                     ,street_response_code =
                                         braintree_util:get_xml_value("/api-error-response/verification/avs-street-address-response-code/text()", Xml)
                                     ,gateway_rejection_reason =
                                         braintree_util:get_xml_value("/api-error-response/verification/gateway-rejection-reason/text()", Xml)},
            {error, #bt_api_error{errors = Errors
                                  ,verification=Verif
                                  ,message = braintree_util:get_xml_value("/api-error-response/message/text()", Xml)}}
    end.
