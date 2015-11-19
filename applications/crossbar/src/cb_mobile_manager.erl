%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(cb_mobile_manager).

-export([delete_account/1]).

-include("./crossbar.hrl").

-define(MOD_CONFIG_CAT, <<"mobile_manager">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
delete_account(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthToken = cb_context:auth_token(Context),

    UrlString = req_uri([<<"accounts">>, AccountId]),
    Headers = req_headers(AuthToken),

    lager:debug("mobile_manager delete via ~s", [UrlString]),

    Resp = ibrowse:send_req(UrlString, Headers, 'delete'),
    handle_resp(Resp).

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_resp(ibrowse_ret()) -> 'ok'.
handle_resp({'ok', "200", _, Resp}) ->
    lager:debug("mobile_manager success ~s", [decode(Resp)]);
handle_resp({'ok', Code, _, Resp}) ->
    lager:warning("mobile_manager error ~p. ~s", [Code, decode(Resp)]);
handle_resp(_Error) ->
    lager:error("mobile_manager fatal error ~p", [_Error]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(string()) -> ne_binary().
decode(JSON) ->
    try wh_json:encode(JSON) of
        JObj -> wh_json:decode(JObj)
    catch
        'error':_R ->
            io:format("~p~n", [_R]),
            JSON
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec req_uri(ne_binaries()) -> list().
req_uri(ExplodedPath) ->
    Url = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"url">>),
    Uri = wh_util:uri(Url, ExplodedPath),
    binary:bin_to_list(Uri).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec req_headers(ne_binary()) -> wh_proplist().
req_headers(AuthToken) ->
    props:filter_undefined([
        {"Content-Type", "application/json"}
        ,{"X-Auth-Token", wh_util:to_list(AuthToken)}
        ,{"User-Agent", wh_util:to_list(erlang:node())}
    ]).