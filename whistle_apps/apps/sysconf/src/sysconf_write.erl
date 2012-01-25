%%%-------------------------------------------------------------------
%%% @author Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Handle AMQP requests to write system configuration data.
%%% Support nested keys like key#subkey#subsubkey
%%% @end
%%%-------------------------------------------------------------------
-module(sysconf_write).

-export([init/0, handle_req/2]).

-include("sysconf.hrl").

init() ->
    ok.

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(ApiJObj, _Props) ->
    ?LOG_START("received sysconf write query"),
    true = wapi_sysconf:write_req_v(ApiJObj),
    
    %% default headers
    EventCategory = wh_json:get_value(<<"Event-Category">>, ApiJObj),
    EventName = <<"write_resp">>,
    Namespace = wh_json:get_value(<<"Namespace">>, ApiJObj), 
    ServerId =  wh_json:get_value(<<"Server-ID">>, ApiJObj), 
    
    %% sysconf data
    Key = wh_json:get_value(<<"Key">>, ApiJObj),
    Value = wh_json:get_value(<<"Value">>, ApiJObj),
    Status = case couch_mgr:open_doc(?SYSTEM_CONFIG_DB, Namespace) of 
        {ok, Doc} ->  UpdatedDoc =wh_json:set_value(binary:split(Key, <<"#">>, [global]), Value, Doc),
                      case couch_mgr:save_doc(?SYSTEM_CONFIG_DB, UpdatedDoc) of
                        {ok, _} -> <<"success">>;
                        {error, _} -> <<"error">>
                      end;
        _ -> <<"namespace_not_found">>
    end,

    Resp = [{<<"Namespace">>, Namespace} 
	    ,{<<"Key">>, Key}
            ,{<<"Value">>, Value}
            ,{<<"Status">>, Status}
            | wh_api:default_headers(ServerId, EventCategory, EventName, ?APP_NAME, ?APP_VERSION)],
    wapi_sysconf:publish_write_resp(Resp),
    
    ?LOG_END("sysconf write query result").

