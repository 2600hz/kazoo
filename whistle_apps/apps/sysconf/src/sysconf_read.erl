%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle requests to read configuration data
%%% Support nested keys a la wh_json, with a # 
%%% as a separator i.e key#subkey#subsubkey
%%% @end
%%%-------------------------------------------------------------------
-module(sysconf_read).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

init() ->
    ok.

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    ?LOG_START("received sysconf read query"),
    true = wapi_sysconf:read_req_v(ApiJObj),
    
    %% default headers
    EventCategory = wh_json:get_value(<<"Event-Category">>, ApiJObj),
    EventName = <<"read_resp">>,
    Namespace = wh_json:get_value(<<"Namespace">>, ApiJObj), 
    ServerId =  wh_json:get_value(<<"Server-ID">>, ApiJObj), 

    %% sysconf data
    Key = wh_json:get_value(<<"Key">>, ApiJObj),
    Value = case couch_mgr:open_doc(?SYSTEM_CONFIG_DB, Namespace) of 
        {ok, Doc} -> case wh_json:get_value(binary:split(Key, <<"#">>, [global]), Doc) of
                      undefined -> <<"key_not_found">>;
                      V -> V
                      end;
        _ -> <<"namespace_not_found">>
      end,

    Resp = [{<<"Namespace">>, Namespace} 
	    ,{<<"Key">>, Key}
            ,{<<"Value">>, Value}
            | wh_api:default_headers(ServerId, EventCategory, EventName, ?APP_NAME, ?APP_VERSION)],
    wapi_sysconf:publish_read_resp(Resp),
    
    ?LOG_END("sysconf read query result").

