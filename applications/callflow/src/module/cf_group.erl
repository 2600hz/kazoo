%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_group).

-include("../callflow.hrl").
-include_lib("whistle/src/api/wapi_dialplan.hrl").

-export([handle/2]).

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
    case wh_json:get_ne_value(<<"endpoints">>, Data) of
        'undefined' -> attempt_group(Data, Call);
        _Else -> cf_ring_group:handle(Data, Call)
    end.

-spec attempt_group(wh_json:object(), whapps_call:call()) -> 'ok'.
attempt_group(Data, Call) ->
    GroupId = wh_doc:id(Data),
    AccountId = whapps_call:account_id(Call),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, GroupId) of
        {'ok', JObj} -> attempt_endpoints(JObj, Data, Call);
        {'error', _R} ->
            lager:debug("unable to open group document ~s in ~s", [GroupId, AccountId]),
            cf_exe:continue(Call)
    end.

-spec attempt_endpoints(wh_json:object(), wh_json:object(), whapps_call:call()) -> 'ok'.
attempt_endpoints(JObj, Data, Call) ->
    Endpoints = build_endpoints(JObj, Call),
    Timeout = wh_util:to_integer(
                wh_json:find(<<"timeout">>, [JObj, Data], ?DEFAULT_TIMEOUT_S)
               ),
    Strategy = wh_util:to_binary(
                 wh_json:find(<<"strategy">>, [JObj, Data], ?DIAL_METHOD_SIMUL)
                ),
    IgnoreForward = wh_util:to_binary(
                      wh_json:find(<<"ignore_forward">>, [JObj, Data], <<"true">>)
                     ),
    Ringback = wh_util:to_binary(
                 wh_json:find(<<"ringback">>, [JObj, Data])
                ),
    lager:info("attempting group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    whapps_call_command:b_answer(Call),
    case whapps_call_command:b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Ringback, 'undefined', IgnoreForward, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the group - call finished normally"),
            cf_exe:stop(Call);
        {'fail', _}=F ->
            case cf_util:handle_bridge_failure(F, Call) of
                'ok' -> lager:debug("bridge failure handled");
                'not_found' -> cf_exe:continue(Call)
            end;
        {'error', _R} ->
            lager:info("error bridging to group: ~p"
                       ,[wh_json:get_value(<<"Error-Message">>, _R)]
                      ),
            cf_exe:continue(Call)
    end.

-spec build_endpoints(wh_json:object(), whapps_call:call()) -> wh_json:objects().
build_endpoints(JObj, Call) ->
    Members = wh_json:to_proplist(<<"endpoints">>, JObj),
    Routines = [fun build_device_endpoints/3
                ,fun build_user_endpoints/3
               ],
    lists:flatten(
      [Endpoint
       || {_, {'ok', Endpoint}} <-
              lists:foldl(
                fun(F, E) -> F(E, Members, Call) end
                ,[]
                ,Routines
               )
      ]
     ).

-type endpoints_acc() :: [{ne_binary(), {'ok', wh_json:objects()}},...] | [].

-spec build_device_endpoints(endpoints_acc(), wh_proplist(), whapps_call:call()) ->
                                    endpoints_acc().
build_device_endpoints(Endpoints, [], _) -> Endpoints;
build_device_endpoints(Endpoints, [{MemberId, Member} | Members], Call) ->
    case wh_json:get_value(<<"type">>, Member, <<"device">>) =:= <<"device">>
        andalso props:get_value(MemberId, Endpoints) =:= 'undefined'
        andalso MemberId =/= whapps_call:authorizing_id(Call)
    of
        'true' ->
            M = wh_json:set_value(<<"source">>, ?MODULE, Member),
            E = [{MemberId, cf_endpoint:build(MemberId, M, Call)}|Endpoints],
            build_device_endpoints(E, Members, Call);
        'false' -> build_device_endpoints(Endpoints, Members, Call)
    end.

-spec build_user_endpoints(endpoints_acc(), wh_proplist(), whapps_call:call()) -> endpoints_acc().
build_user_endpoints(Endpoints, [], _) -> Endpoints;
build_user_endpoints(Endpoints, [{MemberId, Member} | Members], Call) ->
    case wh_json:get_value(<<"type">>, Member, <<"user">>) =:= <<"user">> of
        'true' ->
            DeviceIds = cf_attributes:owned_by(MemberId, <<"device">>, Call),
            M = wh_json:set_values([{<<"source">>, ?MODULE}
                                    ,{<<"type">>, <<"device">>}
                                   ], Member),
            E = build_device_endpoints(Endpoints
                                       ,[{DeviceId, M} || DeviceId <- DeviceIds]
                                       ,Call
                                      ),
            build_user_endpoints(E, Members, Call);
        'false' -> build_user_endpoints(Endpoints, Members, Call)
    end.
