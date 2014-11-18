%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Look up IP for authorization/replaying of route_req
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_route_req).

-export([init/0, handle_route_req/2]).

-include("reg.hrl").

init() -> whapps_maintenance:refresh(?WH_SIP_DB).

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    case wh_json:get_ne_value(<<"Account-ID">>, CCVs) of
        'undefined' ->
	    case wh_json:get_value(<<"From-Network-Addr">>, JObj) of
		'undefined' -> 'ok';
		IP ->
		    lager:debug("trying to see if this route req is an auth-by-ip'd device: ~s", [IP]),
		    case reg_authn_req:lookup_account_by_ip(IP) of
			{'ok', AccProps} ->
			    lager:debug("replaying route_req with auth by IP account CCVs: ~p", [AccProps]),
			    wapi_route:publish_req(
				wh_json:set_value(
				    <<"Custom-Channel-Vars">>
				    , wh_json:set_values(AccProps, CCVs)
				    , JObj)
			    );
			_E -> _E
		    end
	    end;
        _AccountId -> 'ok'
    end.
