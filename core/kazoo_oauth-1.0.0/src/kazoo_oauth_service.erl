%% @author root
%% @doc @todo Add description to kazoo_oauth_util.


-module(kazoo_oauth_service).

-include("kazoo_oauth.hrl").

-export([service_token/2]).


-spec service_token(api_binary() | oauth_service_app(), api_binary()) -> api_object().
service_token('undefined', _) ->
    'undefined';
service_token(_, 'undefined') ->
    'undefined';
service_token(AppId, Scopes) when is_binary(AppId) ->
    lager:debug("getting service app"),
    case kazoo_oauth_util:get_oauth_service_app(AppId) of
        {'ok', App } -> service_token(App, Scopes);
        {'error', Error} ->
            lager:debug("service token ~p",[Error]),'undefined'
    end;   
service_token(#oauth_service_app{private_key=PrivateKey,provider=#oauth_provider{auth_url=URL}}=ServiceApp, Scopes) ->
    Assertion = kazoo_oauth_util:jwt(ServiceApp, Scopes),
    GrantType = wh_util:to_list(wh_util:uri_encode(?OAUTH_GRANT_TYPE)),	
    Headers = [{"Content-Type","application/x-www-form-urlencoded"}
               ,{"User-Agent", "Kazoo"}
              ],
    Fields = [{"grant_type", GrantType}
              ,{"assertion", wh_util:to_list(wh_util:uri_encode(Assertion))}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),
    case ibrowse:send_req(wh_util:to_list(URL), Headers, 'post', Body) of
        {'ok', "200", _RespHeaders, RespXML} ->
            wh_json:decode(RespXML);
        _Else -> 
            lager:debug("unable to request service token: ~p", [_Else]),
            'undefined'
    end.

