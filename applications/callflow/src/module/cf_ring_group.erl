%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_ring_group).

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
    Repeat = wh_json:get_value(<<"repeats">>, Data, 1),
    repeat(Repeat, Data, Call).

-spec repeat(integer(), wh_json:object(), whapps_call:call()) -> 'ok'.
repeat(0, _Data, Call) ->
    cf_exe:continue(Call);
repeat(N, Data, Call) when N > 0 ->
    Next = case get_endpoints(Data, Call) of
        [] ->
            lager:notice("ring group has no endpoints, moving to next callflow element"),
            'no_endpoints';
        Endpoints -> attempt_endpoints(Endpoints, Data, Call)
    end,
    case Next of
        'stop' -> cf_exe:stop(Call);
        'continue' -> repeat(N - 1, Data, Call);
        'no_endpoints' -> cf_exe:continue(Call);
        'fail' -> 'ok'
    end.

-spec attempt_endpoints(wh_json:objects(), wh_json:object(), whapps_call:call()) ->
    'stop' | 'fail' | 'continue'.
attempt_endpoints(Endpoints, Data, Call) ->
    Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Strategy = wh_json:get_binary_value(<<"strategy">>, Data, ?DIAL_METHOD_SIMUL),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    IgnoreForward = wh_json:get_binary_boolean(<<"ignore_forward">>, Data, <<"true">>),
    lager:info("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case whapps_call_command:b_bridge(Endpoints, Timeout, Strategy, <<"true">>, Ringback, 'undefined', IgnoreForward, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the ring group - call finished normally"),
            'stop';
        {'fail', _}=F ->
            case cf_util:handle_bridge_failure(F, Call) of
                'ok' -> lager:debug("bridge failure handled"), 'fail';
                'not_found' -> 'continue'
            end;
        {'error', 'timeout'} ->
            lager:debug("bridge timed out waiting for someone to answer"),
            'continue';
        {'error', _R} ->
            lager:info("error bridging to ring group: ~p"
                       ,[wh_json:get_value(<<"Error-Message">>, _R)]
                      ),
            'continue'
    end.

-spec get_endpoints(wh_json:object(), whapps_call:call()) -> wh_json:objects().
get_endpoints(Data, Call) ->
    Builders = start_builders(Data, Call),
    receive_endpoints(Builders).

-spec receive_endpoints(pids()) -> wh_json:objects().
receive_endpoints(Builders) ->
    lists:foldl(fun receive_endpoint_fold/2, [], Builders).

-spec receive_endpoint_fold(pid(), wh_json:objects()) -> wh_json:objects().
receive_endpoint_fold(Pid, Acc) ->
    receive
        {Pid, {'ok', EP}} when is_list(EP) -> EP ++ Acc;
        {Pid, {'ok', EP}} -> [EP | Acc];
        {Pid, _} -> Acc
    end.

-spec start_builders(wh_json:object(), whapps_call:call()) -> pids().
start_builders(Data, Call) ->
    [start_builder(EndpointId, Member, Call)
     || {EndpointId, Member} <- resolve_endpoint_ids(Data, Call)
    ].

-spec start_builder(ne_binary(), wh_json:object(), whapps_call:call()) -> pid().
start_builder(EndpointId, Member, Call) ->
    S = self(),
    wh_util:spawn(
      fun() ->
              wh_util:put_callid(whapps_call:call_id(Call)),
              S ! {self(), catch cf_endpoint:build(EndpointId, Member, Call)}
      end
     ).

-spec resolve_endpoint_ids(wh_json:object(), whapps_call:call()) -> wh_proplist().
resolve_endpoint_ids(Data, Call) ->
    Members = wh_json:get_value(<<"endpoints">>, Data, []),
    [{Id, wh_json:set_value(<<"source">>, ?MODULE, Member)}
     || {Type, Id, Member} <- resolve_endpoint_ids(Members, [], Data, Call)
            ,Type =:= <<"device">>
            ,Id =/= whapps_call:authorizing_id(Call)
    ].

-type endpoint_intermediate() :: {ne_binary(), ne_binary(), api_object()}.
-type endpoint_intermediates() :: [] | [endpoint_intermediate(),...].

-spec resolve_endpoint_ids(wh_json:objects(), endpoint_intermediates(), wh_json:object(), whapps_call:call()) ->
                                  endpoint_intermediates().
resolve_endpoint_ids([], EndpointIds, _, _) -> EndpointIds;
resolve_endpoint_ids([Member|Members], EndpointIds, Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Member),
    Type = wh_json:get_value(<<"endpoint_type">>, Member, <<"device">>),
    case wh_util:is_empty(Id)
        orelse lists:keymember(Id, 2, EndpointIds)
        orelse Type
    of
        'true' ->
            resolve_endpoint_ids(Members, EndpointIds, Data, Call);
        <<"group">> ->
            lager:info("member ~s is a group, merge the group's members", [Id]),
            GroupMembers = get_group_members(Member, Id, Data, Call),
            Ids = resolve_endpoint_ids(GroupMembers, EndpointIds, Data, Call),
            resolve_endpoint_ids(Members, [{Type, Id, 'undefined'}|Ids], Data, Call);
        <<"user">> ->
            lager:info("member ~s is a user, get all the user's endpoints", [Id]),
            Ids = get_user_endpoint_ids(Member, EndpointIds, Id, Call),
            resolve_endpoint_ids(Members, Ids, Data, Call);
        <<"device">> ->
            resolve_endpoint_ids(Members, [{Type, Id, Member}|EndpointIds], Data, Call)
    end.

-spec get_user_endpoint_ids(wh_json:object(), endpoint_intermediates(), ne_binary(), whapps_call:call()) ->
                                   endpoint_intermediates().
get_user_endpoint_ids(Member, EndpointIds, Id, Call) ->
    lists:foldr(
      fun(EndpointId, Acc) ->
              case lists:keymember(EndpointId, 2, Acc) of
                  'true' -> Acc;
                  'false' ->
                      [{<<"device">>, EndpointId, Member} | Acc]
              end
      end
      ,[{<<"user">>, Id, 'undefined'} | EndpointIds]
      ,cf_attributes:owned_by(Id, <<"device">>, Call)
     ).

-spec get_group_members(wh_json:object(), ne_binary(), wh_json:object(), whapps_call:call()) -> wh_json:objects().
get_group_members(Member, Id, Data, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, Id) of
        {'ok', JObj} ->
            maybe_order_group_members(Member, JObj, Data);
        {'error', _R} ->
            lager:warning("unable to lookup members of group ~s: ~p", [Id, _R]),
            []
    end.

-spec maybe_order_group_members(wh_json:object(), wh_json:object(), wh_json:object()) -> wh_json:objects().
maybe_order_group_members(Member, JObj, Data) ->
    case wh_json:get_binary_value(<<"strategy">>, Data, ?DIAL_METHOD_SIMUL) of
        ?DIAL_METHOD_SINGLE ->
            order_group_members(Member, JObj);
        _ ->
            unordered_group_members(Member, JObj)
    end.

-spec unordered_group_members(wh_json:object(), wh_json:object()) -> wh_json:objects().
unordered_group_members(Member, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"endpoints">>, JObj, wh_json:new()),
    wh_json:foldl(
      fun(Key, Endpoint, Acc) ->
              [create_group_member(Key, Endpoint, Member) | Acc]
      end
      ,[]
      ,Endpoints
     ).

-spec order_group_members(wh_json:object(), wh_json:object()) -> wh_json:objects().
order_group_members(Member, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"endpoints">>, JObj, wh_json:new()),
    GroupMembers =
        wh_json:foldl(
          fun(Key, Endpoint, Acc) ->
                  case wh_json:get_value(<<"weight">>, Endpoint) of
                      'undefined' ->
                          lager:debug("endpoint ~s has no weight, removing from ordered group", [Key]),
                          Acc;
                      Weight ->
                          GroupMember = create_group_member(Key, Endpoint, Member),
                          orddict:store(Weight, GroupMember, Acc)
                  end
          end
          ,orddict:new()
          ,Endpoints
         ),
    [V || {_, V} <- orddict:to_list(GroupMembers)].

-spec create_group_member(ne_binary(), wh_json:object(), wh_json:object()) -> wh_json:object().
create_group_member(Key, Endpoint, Member) ->
    DefaultDelay = wh_json:get_value(<<"delay">>, Member),
    DefaultTimeout = wh_json:get_value(<<"timeout">>, Member),
    wh_json:set_values([{<<"endpoint_type">>, wh_json:get_value(<<"type">>, Endpoint)}
                        ,{<<"id">>, Key}
                        ,{<<"delay">>, wh_json:get_value(<<"delay">>, Endpoint, DefaultDelay)}
                        ,{<<"timeout">>, wh_json:get_value(<<"timeout">>, Endpoint, DefaultTimeout)}
                       ]
                       ,Member
                      ).
