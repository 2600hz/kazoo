%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle authn_req messages
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(stepswitch_authn_req).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec handle_req(wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    true = wapi_authn:req_v(JObj),

    put(callid, wh_json:get_value(<<"Msg-ID">>, JObj, <<"000000000000">>)),
    AuthR = wh_json:get_value(<<"Auth-Realm">>, JObj),
    Resources = props:get_value(resources, Props),

    case wh_json:get_value(<<"Method">>, JObj) =:= <<"reverse-lookup">>
        andalso stepswitch_util:maybe_gateway_by_address(AuthR, Resources) 
    of
        #gateway{}=Gateway -> send_auth_resp(Gateway, JObj);
        _Else -> ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec send_auth_resp/2  :: (#gateway{}, wh_json:json_object()) -> 'ok'.
send_auth_resp(#gateway{resource_id=AuthId, realm=Realm
                        ,username=Username, password=Password}, JObj) ->
    Category = wh_json:get_value(<<"Event-Category">>, JObj),

    CCVs = [{<<"Username">>, Username}
            ,{<<"Realm">>, Realm}
%%             TODO: this can only be added if callflows and trunkstore are updated....
%%               since they look here to see if they should attempt to process
%%               this common logic should be moved to whapps_call if possible.          
%%            ,{<<"Authorizing-Type">>, <<"resource">>}
            ,{<<"Inception">>, <<"off-net">>}
            ,{<<"Authorizing-ID">>, AuthId}
           ],

    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Auth-Password">>, Password}
            ,{<<"Auth-Username">>, Username}
            ,{<<"Auth-Method">>, <<"password">>}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(props:filter_undefined(CCVs))}
            | wh_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending SIP authentication reply, with credentials"),
    wapi_authn:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                            ,props:filter_undefined(Resp)).
