%% @author root
%% @doc @todo Add description to kazoo_oauth_util.


-module(kazoo_oauth_service).

-include("kazoo_oauth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([service_token/2]).

	

	
service_token(#oauth_service_app{email=AccountEmail,provider=#oauth_provider{auth_url=URL}}=ServiceApp, SCOPES) ->
	Assertion = kazoo_oauth_util:jwt(ServiceApp, SCOPES),
	GrantType = wh_util:to_list( wh_util:uri_encode(?OAUTH_GRANT_TYPE)),
	
	
	Headers = [{"Content-Type","application/x-www-form-urlencoded"}
			   ,{"User-Agent", "Kazoo"}
			  ],
	Fields = [{"grant_type",GrantType}
			 ,{"assertion", wh_util:to_list(wh_util:uri_encode(Assertion))}
			 ],
	Body = string:join(lists:append(lists:map(fun({K,V}) -> [string:join([K,V], "=") ] end, Fields)),"&"),
	case ibrowse:send_req(wh_util:to_list(URL), Headers, 'post', Body) of
		{'ok', "200", _RespHeaders, RespXML} ->
			lager:info("OAuth ~s",[RespXML]),
			JObjOAuth = wh_json:decode(RespXML),
			lager:info("JObjOAuth : ~p",[JObjOAuth]),
			JObjOAuth;
		A -> lager:info("Error ~p",[A]), 'undefined'
	end.

	
	


%% ====================================================================
%% Internal functions
%% ====================================================================


