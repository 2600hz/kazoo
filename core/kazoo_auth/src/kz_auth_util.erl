%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_auth_util).

-include("kazoo_auth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([map_keys_to_atoms/1]).

-export([get_json_from_url/1, get_json_from_url/2]).

-export([fetch_access_code/2, fetch_access_code/3]).

-spec map_keys_to_atoms(map()) -> map().
map_keys_to_atoms(Map) ->
    maps:fold(fun map_keys_to_atoms_fold/3, #{}, Map).

map_keys_to_atoms_fold(K, V, Acc) when is_map(V) ->
    Acc#{kz_util:to_atom(K, 'true') => map_keys_to_atoms(V)};
map_keys_to_atoms_fold(K, V, Acc) ->
    Acc#{kz_util:to_atom(K, 'true') => V}.



get_json_from_url(Url) ->
    get_json_from_url(Url, []).

get_json_from_url(Url, ReqHeaders) ->
    case kz_http:get(kz_util:to_list(Url), ReqHeaders) of
        {'ok', 200, _RespHeaders, Body} ->
            JObj = kz_json:decode(Body),
            case kz_util:is_empty(JObj) of
                'true' -> {'error', 'empty'};
                'false' -> {'ok', JObj}
            end;
        {'ok', Code, _RespHeaders, Body} ->
            lager:debug("unexpected code ~b downloading json from ~s : ~s", [Code, Url, Body]),
            {'error', 'unexpected_result'};
        Else ->
            lager:debug("error downloading json from ~s : ~p", [Url, Else]),
            {'error', Else}
    end.

-spec fetch_access_code(ne_binary() | map(), ne_binary() ) ->
                               {'ok', kz_json:object()} |
                               {'error', any()}.
fetch_access_code(AppId, AuthorizationCode) ->
    fetch_access_code(AppId, AuthorizationCode, <<"postmessage">>).


-spec fetch_access_code(ne_binary() | map(), ne_binary(), ne_binary()) ->
                               {'ok', kz_json:object()} |
                               {'error', any()}.
fetch_access_code(AppId, AuthorizationCode, RedirectUri)
  when is_binary(AppId) ->
    lager:debug("getting auth-app ~p",[AppId]),
    case get_app_and_provider(AppId) of
        #{} = Map -> fetch_access_code(Map, AuthorizationCode, RedirectUri);
        Error -> Error
    end;
fetch_access_code(#{auth_app := #{name := ClientId
                                 ,pvt_secret := Secret
                                 }
                   ,auth_provider := #{access_code_url := URL}
                   }, AuthorizationCode, RedirectUri) ->
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],

    Fields = [{"client_id", ClientId}
             ,{"redirect_uri", RedirectUri}
             ,{"client_secret", Secret}
             ,{"grant_type", "authorization_code"}
             ,{"code", AuthorizationCode}
             ],
    Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K, kz_util:to_list(V)], "=") ] end, Fields)),"&"),
    case kz_http:post(kz_util:to_list(URL), Headers, Body) of
        {'ok', 200, _RespHeaders, RespXML} -> {'ok', kz_json:decode(RespXML)};
        Else -> {'error', Else}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_app_and_provider(AppId) ->
    case kz_auth_apps:get_auth_app(AppId) of
        #{pvt_auth_provider := Provider} = App ->
            #{auth_app => App
             ,auth_provider => kz_auth_providers:get_auth_provider(Provider)
             };
        Error -> Error
    end.
