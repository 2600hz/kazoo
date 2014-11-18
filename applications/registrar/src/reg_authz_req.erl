%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Handle authz_req messages
%%% @end
%%% @contributors
%%%   David Singer (cando)
%%%-------------------------------------------------------------------
-module(reg_authz_req).

-export([init/0
	, handle_req/2
	]).

-include("reg.hrl").

init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authz:authz_req_v(JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    AccId = wh_json:get_ne_value(<<"Account-ID">>, CCVs),
    lager:debug("Account-ID ~p from CCVs: ~p, req: ~p", [AccId, CCVs, JObj]),
    case AccId of
	'undefined' ->
	    wh_util:put_callid(JObj),
	    IP = wh_json:get_value(<<"From-Network-Addr">>, JObj),
	    lager:debug("Missing Account-ID, trying to authenticate with auth-by-IP ~s.", [IP]),
	    case reg_authn_req:lookup_account_by_ip(IP) of
	        {'ok', AccProps} ->
		    lager:debug("replaying authz_req with auth by IP account CCVs: ~p", [AccProps]),
		    wapi_authz:publish_authz_req(
			wh_json:set_value(
			    <<"Custom-Channel-Vars">>
			    , wh_json:set_values(AccProps, CCVs)
			    , JObj)
		    );
		_Err -> _Err
	    end;
	_ ->
	    'ok'
    end.
