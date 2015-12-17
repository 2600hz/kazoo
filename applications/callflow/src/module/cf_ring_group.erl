%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   SIPLABS LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_ring_group).

-include("../callflow.hrl").
-include_lib("whistle/src/api/wapi_dialplan.hrl").

-export([handle/2]).

-ifdef(TEST).
-export([weighted_random_sort/1]).
-endif.

-type group_weight() :: 1..100.

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
    repeat(Data, Call, repeats(Data)).

-spec repeat(wh_json:object(), whapps_call:call(), non_neg_integer()) -> 'ok'.
repeat(_Data, Call, 0) ->
    cf_exe:continue(Call);
repeat(Data, Call, N) ->
    Next = case get_endpoints(Data, Call) of
               [] ->
                   lager:notice("ring group has no endpoints, moving to next callflow element"),
                   'no_endpoints';
               Endpoints -> attempt_endpoints(Endpoints, Data, Call)
           end,
    repeat(Data, Call, N, Next).

-type attempt_result() :: 'stop' |
                          'continue' |
                          'no_endpoints' |
                          'fail'.

-spec repeat(wh_json:object(), whapps_call:call(), non_neg_integer(), attempt_result()) -> 'ok'.
repeat(_Data, Call, _N, 'stop') ->
    cf_exe:stop(Call);
repeat(Data, Call, N, 'continue') ->
    repeat(Data, Call, N-1);
repeat(_Data, Call, _N, 'no_endpoints') ->
    cf_exe:continue(Call);
repeat(_Data, _Call, _N, 'fail') ->
    'ok'.

-spec attempt_endpoints(wh_json:objects(), wh_json:object(), whapps_call:call()) ->
    'stop' | 'fail' | 'continue'.
attempt_endpoints(Endpoints, Data, Call) ->
    Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Strategy = freeswitch_strategy(Data),
    Ringback = wh_json:get_value(<<"ringback">>, Data),
    IgnoreForward = wh_json:get_binary_boolean(<<"ignore_forward">>, Data, <<"true">>),
    lager:info("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case whapps_call_command:b_bridge(Endpoints
                                      ,Timeout
                                      ,Strategy
                                      ,<<"true">>
                                      ,Ringback
                                      ,'undefined'
                                      ,IgnoreForward
                                      ,Call
                                     )
    of
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
    ResolvedEndpoints = resolve_endpoint_ids(Members, [], Data, Call),
    FilteredEndpoints = [{Weight, {Id, wh_json:set_value(<<"source">>, ?MODULE, Member)}}
                         || {Type, Id, Weight, Member} <- ResolvedEndpoints
                            ,Type =:= <<"device">>
                            ,Id =/= whapps_call:authorizing_id(Call)
                        ],
    Strategy = strategy(Data),
    order_endpoints(Strategy, FilteredEndpoints).

-spec order_endpoints(ne_binary(), wh_proplist()) -> wh_proplist().
order_endpoints(Method, Endpoints)
  when Method =:= ?DIAL_METHOD_SIMUL
       orelse Method =:= ?DIAL_METHOD_SINGLE ->
    [{Id, Endpoint} || {_, {Id, Endpoint}} <- Endpoints];
order_endpoints(<<"weighted_random">>, Endpoints) ->
    weighted_random_sort(Endpoints).

-type endpoint_intermediate() :: {ne_binary(), ne_binary(), group_weight(), api_object()}.
-type endpoint_intermediates() :: [endpoint_intermediate()].

-spec resolve_endpoint_ids(wh_json:objects(), endpoint_intermediates(), wh_json:object(), whapps_call:call()) ->
                                  endpoint_intermediates().
resolve_endpoint_ids([], EndpointIds, _, _) -> EndpointIds;
resolve_endpoint_ids([Member|Members], EndpointIds, Data, Call) ->
    Id = wh_doc:id(Member),
    Type = wh_json:get_value(<<"endpoint_type">>, Member, <<"device">>),
    Weight = group_weight(Member, 20),
    case wh_util:is_empty(Id)
        orelse lists:keymember(Id, 2, EndpointIds)
        orelse Type
    of
        'true' ->
            resolve_endpoint_ids(Members, EndpointIds, Data, Call);
        <<"group">> ->
            lager:info("member ~s is a group, merge the group's members", [Id]),
            GroupMembers = get_group_members(Member, Id, Weight, Data, Call),
            Ids = resolve_endpoint_ids(GroupMembers, EndpointIds, Data, Call),
            resolve_endpoint_ids(Members, [{Type, Id, 'undefined'}|Ids], Data, Call);
        <<"user">> ->
            lager:info("member ~s is a user, get all the user's endpoints", [Id]),
            Ids = get_user_endpoint_ids(Member, EndpointIds, Id, Weight, Call),
            resolve_endpoint_ids(Members, Ids, Data, Call);
        <<"device">> ->
            resolve_endpoint_ids(Members, [{Type, Id, Weight, Member}|EndpointIds], Data, Call)
    end.

-spec get_user_endpoint_ids(wh_json:object(), endpoint_intermediates(), ne_binary(), group_weight(), whapps_call:call()) ->
                                   endpoint_intermediates().
get_user_endpoint_ids(Member, EndpointIds, Id, GroupWeight, Call) ->
    lists:foldr(
      fun(EndpointId, Acc) ->
              case lists:keymember(EndpointId, 2, Acc) of
                  'true' -> Acc;
                  'false' ->
                      [{<<"device">>, EndpointId, GroupWeight, Member} | Acc]
              end
      end
      ,[{<<"user">>, Id, 'undefined'} | EndpointIds]
      ,cf_attributes:owned_by(Id, <<"device">>, Call)
     ).

-spec get_group_members(wh_json:object(), ne_binary(), group_weight(), wh_json:object(), whapps_call:call()) -> wh_json:objects().
get_group_members(Member, Id, GroupWeight, Data, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, Id) of
        {'ok', JObj} ->
            maybe_order_group_members(GroupWeight, Member, JObj, Data);
        {'error', _R} ->
            lager:warning("unable to lookup members of group ~s: ~p", [Id, _R]),
            []
    end.

-spec maybe_order_group_members(group_weight(), wh_json:object(), wh_json:object(), wh_json:object()) ->
                                       wh_json:objects().
maybe_order_group_members(Weight, Member, JObj, Data) ->
    case strategy(Data) of
        ?DIAL_METHOD_SINGLE ->
            order_group_members(Weight, Member, JObj);
        _ ->
            unordered_group_members(Weight, Member, JObj)
    end.

-spec unordered_group_members(group_weight(), wh_json:object(), wh_json:object()) ->
                                     wh_json:objects().
unordered_group_members(Weight, Member, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"endpoints">>, JObj, wh_json:new()),
    wh_json:foldl(
      fun(Key, Endpoint, Acc) ->
              [create_group_member(Key, Endpoint, Weight, Member) | Acc]
      end
      ,[]
      ,Endpoints
     ).

-spec order_group_members(group_weight(), wh_json:object(), wh_json:object()) -> wh_json:objects().
order_group_members(GroupWeight, Member, JObj) ->
    Endpoints = wh_json:get_ne_value(<<"endpoints">>, JObj, wh_json:new()),
    GroupMembers =
        wh_json:foldl(
          fun(Key, Endpoint, Acc) ->
                  order_group_member_fold(Key, Endpoint, Acc, GroupWeight, Member)
          end
          ,orddict:new()
          ,Endpoints
         ),
    [V || {_, V} <- orddict:to_list(GroupMembers)].

-spec order_group_member_fold(wh_json:key(), wh_json:object(), orddict:orddict(), group_weight(), wh_json:object()) ->
                                     orddict:orddict().
order_group_member_fold(Key, Endpoint, Acc, GroupWeight, Member) ->
    case group_weight(Endpoint) of
        'undefined' ->
            lager:debug("endpoint ~s has no weight, removing from ordered group", [Key]),
            Acc;
        Weight ->
            GroupMember = create_group_member(Key, Endpoint, GroupWeight, Member),
            orddict:store(Weight, GroupMember, Acc)
    end.

-spec create_group_member(ne_binary(), wh_json:object(), group_weight(), wh_json:object()) ->
                                 wh_json:object().
create_group_member(Key, Endpoint, GroupWeight, Member) ->
    DefaultDelay = wh_json:get_value(<<"delay">>, Member),
    DefaultTimeout = wh_json:get_value(<<"timeout">>, Member),
    wh_json:set_values(
      [{<<"endpoint_type">>, wh_json:get_value(<<"type">>, Endpoint)}
       ,{<<"id">>, Key}
       ,{<<"delay">>, wh_json:get_integer_value(<<"delay">>, Endpoint, DefaultDelay)}
       ,{<<"timeout">>, wh_json:get_integer_value(<<"timeout">>, Endpoint, DefaultTimeout)}
       ,{<<"weight">>, GroupWeight}
      ]
      ,Member
     ).

-spec weighted_random_sort(wh_proplist()) -> wh_json:objects().
weighted_random_sort(Endpoints) ->
    _ = random:seed(os:timestamp()),
    WeightSortedEndpoints = lists:sort(Endpoints),
    weighted_random_sort(WeightSortedEndpoints, []).

-spec set_intervals_on_weight(wh_proplist(), wh_proplist(), integer()) ->
                                     wh_proplist().
set_intervals_on_weight([{Weight, _} =E | Tail], Acc, Sum) ->
    set_intervals_on_weight(Tail, [{Weight + Sum, E} | Acc], Sum + Weight);
set_intervals_on_weight([], Acc, _Sum) ->
    Acc.

-spec weighted_random_sort(wh_proplist(), wh_proplists()) -> wh_proplists().
weighted_random_sort([_ | _] = ListWeight, Acc) ->
    [{Sum, _} | _] = ListInterval = set_intervals_on_weight(ListWeight, [], 0),
    Pivot = random_integer(Sum),
    {_, {_, Endpoint} = Element} = weighted_random_get_element(ListInterval, Pivot),
    ListNew = lists:delete(Element, ListWeight),
    weighted_random_sort(ListNew, [Endpoint | Acc]);
weighted_random_sort([], Acc) ->
    Acc.

-spec weighted_random_get_element([{integer(), {integer(), wh_proplist()}},...], integer()) -> {integer(), {integer(), wh_proplist()}}.
weighted_random_get_element(List, Pivot) ->
    {_, {Weight, _}} = case [E || E={X, _} <- List, X =< Pivot] of
                           [] -> lists:last(List);
                           L -> hd(L)
                       end,
    ListFiltered = [E || E = {_, {W, _}} <- List, W =:= Weight],
    lists:nth(random_integer(length(ListFiltered)), ListFiltered).

-spec random_integer(integer()) -> integer().
random_integer(I) ->
    random:uniform(I).

-spec repeats(wh_json:object()) -> pos_integer().
repeats(Data) ->
    max(1, wh_json:get_integer_value(<<"repeats">>, Data, 1)).

-spec group_weight(wh_json:object()) -> group_weight() | 'undefined'.
-spec group_weight(wh_json:object(), Default) -> group_weight() | Default.
group_weight(Endpoint) ->
    group_weight(Endpoint, 'undefined').
group_weight(Endpoint, Default) ->
    case wh_json:get_integer_value(<<"weight">>, Endpoint) of
        'undefined' -> Default;
        N when N < 1 -> 1;
        N when N > 100 -> 100;
        N -> N
    end.

-spec strategy(wh_json:object()) -> ne_binary().
strategy(Data) ->
    wh_json:get_binary_value(<<"strategy">>, Data, ?DIAL_METHOD_SIMUL).

-spec freeswitch_strategy(wh_json:object()) -> ne_binary().
freeswitch_strategy(Data) ->
    case strategy(Data) of
        ?DIAL_METHOD_SIMUL -> ?DIAL_METHOD_SIMUL;
        _ -> ?DIAL_METHOD_SINGLE
    end.
