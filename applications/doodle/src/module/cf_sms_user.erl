%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_sms_user).

-include("../doodle.hrl").

-export([handle/2
         ,get_endpoints/3
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    UserId = wh_json:get_ne_value(<<"id">>, Data),
    Endpoints = get_endpoints(UserId, Data, Call),
    whapps_util:amqp_pool_collect(doodle_util:create_sms(Call, Endpoints)
                                      ,fun wapi_sms:publish_message/1
                                      ,fun is_resp/1
                                     ,30000),
%%     whapps_util:amqp_pool_request(Payload
%%                                   ,fun wapi_sms:publish_message/1
%%                                   ,fun wapi_sms:delivery_v/1
%%                                   ,30000).

    
    doodle_exe:continue(Call).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loop over the provided endpoints for the callflow and build the
%% json object used in the bridge API
%% @end
%%--------------------------------------------------------------------
-spec is_resp(wh_json:objects()) -> {'ok', iolist()} |
                                    {'error', string()}.
is_resp([JObj|_Y]) ->
    wapi_sms:delivery_v(JObj).


-spec get_endpoints(api_binary(), wh_json:object(), whapps_call:call()) ->
                           wh_json:objects().
get_endpoints('undefined', _, _) -> [];
get_endpoints(UserId, Data, Call) ->
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    lists:foldr(fun(EndpointId, Acc) ->
                        case cf_endpoint:build(EndpointId, Params, Call) of
                            {'ok', Endpoint} -> Endpoint ++ Acc;
                            {'error', _E} -> Acc
                        end
                end, [], cf_attributes:owned_by(UserId, <<"device">>, Call)).

