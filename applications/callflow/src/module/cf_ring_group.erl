%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%   SIPLABS LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(cf_ring_group).

-behaviour(gen_cf_action).

-include("callflow.hrl").
-include_lib("kazoo/src/api/kapi_dialplan.hrl").

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
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    repeat(Data, Call, repeats(Data)).

-spec repeat(kz_json:object(), kapps_call:call(), non_neg_integer()) -> 'ok'.
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

-spec repeat(kz_json:object(), kapps_call:call(), non_neg_integer(), attempt_result()) -> 'ok'.
repeat(_Data, Call, _N, 'stop') ->
    cf_exe:stop(Call);
repeat(Data, Call, N, 'continue') ->
    repeat(Data, Call, N-1);
repeat(_Data, Call, _N, 'no_endpoints') ->
    cf_exe:continue(Call);
repeat(_Data, _Call, _N, 'fail') ->
    'ok'.

-spec attempt_endpoints(kz_json:objects(), kz_json:object(), kapps_call:call()) ->
                               'stop' | 'fail' | 'continue'.
attempt_endpoints(Endpoints, Data, Call) ->
    FailOnSingleReject = kz_json:get_value(<<"fail_on_single_reject">>, Data, 'undefined'),
    Timeout = kz_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    Strategy = freeswitch_strategy(Data),
    Ringback = kz_json:get_value(<<"ringback">>, Data),
    IgnoreForward = kz_json:get_binary_boolean(<<"ignore_forward">>, Data, <<"true">>),

    Command = [{<<"Application-Name">>, <<"bridge">>}
              ,{<<"Endpoints">>, Endpoints}
              ,{<<"Timeout">>, Timeout}
              ,{<<"Ignore-Early-Media">>, <<"true">>}
              ,{<<"Ringback">>, kz_media_util:media_path(Ringback, Call)}
              ,{<<"Fail-On-Single-Reject">>, FailOnSingleReject}
              ,{<<"Dial-Endpoint-Method">>, Strategy}
              ,{<<"Ignore-Forward">>, IgnoreForward}
              ],

    lager:info("attempting ring group of ~b members with strategy ~s", [length(Endpoints), Strategy]),
    case bridge(Command, Timeout, Call)
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
                      ,[kz_json:get_value(<<"Error-Message">>, _R)]
                      ),
            'continue'
    end.

-spec get_endpoints(kz_json:object(), kapps_call:call()) -> kz_json:objects().
get_endpoints(Data, Call) ->
    receive_endpoints(start_builders(Data, Call)).

-spec receive_endpoints(pids()) -> kz_json:objects().
receive_endpoints(Builders) ->
    lists:foldl(fun receive_endpoint_fold/2, [], Builders).

-spec receive_endpoint_fold(pid(), kz_json:objects()) -> kz_json:objects().
receive_endpoint_fold(Pid, Acc) ->
    receive
        {Pid, {'ok', EP}} when is_list(EP) -> EP ++ Acc;
        {Pid, {'ok', EP}} -> [EP | Acc];
        {Pid, _} -> Acc
    end.

-spec start_builders(kz_json:object(), kapps_call:call()) -> pids().
start_builders(Data, Call) ->
    [start_builder(EndpointId, Member, Call)
     || {EndpointId, Member} <- resolve_endpoint_ids(Data, Call)
    ].

-spec start_builder(ne_binary(), kz_json:object(), kapps_call:call()) -> pid().
start_builder(EndpointId, Member, Call) ->
    S = self(),
    kz_util:spawn(
      fun() ->
              kz_util:put_callid(kapps_call:call_id(Call)),
              S ! {self(), catch kz_endpoint:build(EndpointId, Member, Call)}
      end
     ).

-type endpoint() :: {ne_binary(), kz_json:object()}.
-type endpoints() :: [endpoint()].

-type weighted_endpoint() :: {integer(), {ne_binary(), kz_json:object()}}.
-type weighted_endpoints() :: [weighted_endpoint()].

-spec resolve_endpoint_ids(kz_json:object(), kapps_call:call()) -> endpoints().
resolve_endpoint_ids(Data, Call) ->
    Members = kz_json:get_value(<<"endpoints">>, Data, []),
    ResolvedEndpoints = resolve_endpoint_ids(Members, [], Data, Call),

    FilteredEndpoints = [{Weight, {Id, kz_json:set_value(<<"source">>, ?MODULE, Member)}}
                         || {Type, Id, Weight, Member} <- ResolvedEndpoints,
                            Type =:= <<"device">>,
                            Id =/= kapps_call:authorizing_id(Call)
                        ],
    Strategy = strategy(Data),
    order_endpoints(Strategy, FilteredEndpoints).

-spec order_endpoints(ne_binary(), weighted_endpoints()) -> endpoints().
order_endpoints(Method, Endpoints)
  when Method =:= ?DIAL_METHOD_SIMUL
       orelse Method =:= ?DIAL_METHOD_SINGLE ->
    [{Id, Endpoint} || {_Weight, {Id, Endpoint}} <- Endpoints];
order_endpoints(<<"weighted_random">>, Endpoints) ->
    weighted_random_sort(Endpoints).

-type endpoint_intermediate() :: {ne_binary(), ne_binary(), group_weight(), api_object()}.
-type endpoint_intermediates() :: [endpoint_intermediate()].

-spec resolve_endpoint_ids(kz_json:objects(), endpoint_intermediates(), kz_json:object(), kapps_call:call()) ->
                                  endpoint_intermediates().
resolve_endpoint_ids(Members, EndpointIds, Data, Call) ->
    lists:foldl(fun(Member, Acc) ->
                        resolve_endpoint_id(Member, Acc, Data, Call)
                end
               ,EndpointIds
               ,Members
               ).

-spec resolve_endpoint_id(kz_json:object(), endpoint_intermediates(), kz_json:object(), kapps_call:call()) ->
                                 endpoint_intermediates().
resolve_endpoint_id(Member, EndpointIds, Data, Call) ->
    Id = kz_doc:id(Member),
    Type = kz_json:get_value(<<"endpoint_type">>, Member, <<"device">>),
    Weight = group_weight(Member, 20),
    case kz_util:is_empty(Id)
        orelse lists:keymember(Id, 2, EndpointIds)
        orelse Type
    of
        'true' -> EndpointIds;
        <<"group">> ->
            lager:info("member ~s is a group, merge the group's members", [Id]),
            GroupMembers = get_group_members(Member, Id, Weight, Data, Call),
            Ids = resolve_endpoint_ids(GroupMembers, EndpointIds, Data, Call),
            [{Type, Id, 'undefined'}|Ids];
        <<"user">> ->
            lager:info("member ~s is a user, get all the user's endpoints", [Id]),
            get_user_endpoint_ids(Member, EndpointIds, Id, Weight, Call);
        <<"device">> ->
            [{Type, Id, Weight, Member}|EndpointIds]
    end.

-spec get_user_endpoint_ids(kz_json:object(), endpoint_intermediates(), ne_binary(), group_weight(), kapps_call:call()) ->
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
               ,kz_attributes:owned_by(Id, <<"device">>, Call)
     ).

-spec get_group_members(kz_json:object(), ne_binary(), group_weight(), kz_json:object(), kapps_call:call()) -> kz_json:objects().
get_group_members(Member, Id, GroupWeight, Data, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:open_cache_doc(AccountDb, Id) of
        {'ok', JObj} ->
            maybe_order_group_members(GroupWeight, Member, JObj, Data);
        {'error', _R} ->
            lager:warning("unable to lookup members of group ~s: ~p", [Id, _R]),
            []
    end.

-spec maybe_order_group_members(group_weight(), kz_json:object(), kz_json:object(), kz_json:object()) ->
                                       kz_json:objects().
maybe_order_group_members(Weight, Member, JObj, Data) ->
    case strategy(Data) of
        ?DIAL_METHOD_SINGLE ->
            order_group_members(Weight, Member, JObj);
        _ ->
            unordered_group_members(Weight, Member, JObj)
    end.

-spec unordered_group_members(group_weight(), kz_json:object(), kz_json:object()) ->
                                     kz_json:objects().
unordered_group_members(Weight, Member, JObj) ->
    Endpoints = kz_json:get_ne_value(<<"endpoints">>, JObj, kz_json:new()),
    kz_json:foldl(
      fun(Key, Endpoint, Acc) ->
              [create_group_member(Key, Endpoint, Weight, Member) | Acc]
      end
                 ,[]
                 ,Endpoints
     ).

-spec order_group_members(group_weight(), kz_json:object(), kz_json:object()) -> kz_json:objects().
order_group_members(GroupWeight, Member, JObj) ->
    Endpoints = kz_json:get_ne_value(<<"endpoints">>, JObj, kz_json:new()),
    GroupMembers =
        kz_json:foldl(
          fun(Key, Endpoint, Acc) ->
                  order_group_member_fold(Key, Endpoint, Acc, GroupWeight, Member)
          end
                     ,orddict:new()
                     ,Endpoints
         ),
    [V || {_, V} <- orddict:to_list(GroupMembers)].

-spec order_group_member_fold(kz_json:key(), kz_json:object(), orddict:orddict(), group_weight(), kz_json:object()) ->
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

-spec create_group_member(ne_binary(), kz_json:object(), group_weight(), kz_json:object()) ->
                                 kz_json:object().
create_group_member(Key, Endpoint, GroupWeight, Member) ->
    DefaultDelay = kz_json:get_value(<<"delay">>, Member),
    DefaultTimeout = kz_json:get_value(<<"timeout">>, Member),
    kz_json:set_values(
      [{<<"endpoint_type">>, kz_json:get_value(<<"type">>, Endpoint)}
      ,{<<"id">>, Key}
      ,{<<"delay">>, kz_json:get_integer_value(<<"delay">>, Endpoint, DefaultDelay)}
      ,{<<"timeout">>, kz_json:get_integer_value(<<"timeout">>, Endpoint, DefaultTimeout)}
      ,{<<"weight">>, GroupWeight}
      ]
                      ,Member
     ).

-spec weighted_random_sort(weighted_endpoints()) -> endpoints().
weighted_random_sort(Endpoints) ->
    _ = rand:seed('exsplus'),
    WeightSortedEndpoints = lists:sort(Endpoints),
    weighted_random_sort(WeightSortedEndpoints, []).

-spec weighted_random_sort(weighted_endpoints(), endpoints()) -> endpoints().
weighted_random_sort([_ | _] = ListWeight, Acc) ->
    [{Sum, _} | _] = ListInterval = set_intervals_on_weight(ListWeight, [], 0),
    Pivot = random_integer(Sum),
    {_W, {_Id, _Endpoint} = Element} = weighted_random_get_element(ListInterval, Pivot),
    ListNew = lists:delete(Element, ListWeight),
    weighted_random_sort(ListNew, [Element | Acc]);
weighted_random_sort([], Acc) ->
    Acc.

-spec set_intervals_on_weight(weighted_endpoints(), weighted_endpoints(), integer()) ->
                                     weighted_endpoints().
set_intervals_on_weight([{Weight, _}=E | Tail], Acc, Sum) ->
    set_intervals_on_weight(Tail, [{Weight + Sum, E} | Acc], Sum + Weight);
set_intervals_on_weight([], Acc, _Sum) ->
    Acc.

-spec weighted_random_get_element(weighted_endpoints(), integer()) -> weighted_endpoint().
weighted_random_get_element(List, Pivot) ->
    {_, {Weight, _}} = case [E || E={X, _} <- List, X =< Pivot] of
                           [] -> lists:last(List);
                           L -> hd(L)
                       end,
    ListFiltered = [E || E = {_, {W, _}} <- List, W =:= Weight],
    lists:nth(random_integer(length(ListFiltered)), ListFiltered).

-spec random_integer(integer()) -> integer().
random_integer(I) ->
    rand:uniform(I).

-spec repeats(kz_json:object()) -> pos_integer().
repeats(Data) ->
    max(1, kz_json:get_integer_value(<<"repeats">>, Data, 1)).

-spec group_weight(kz_json:object()) -> group_weight() | 'undefined'.
-spec group_weight(kz_json:object(), Default) -> group_weight() | Default.
group_weight(Endpoint) ->
    group_weight(Endpoint, 'undefined').
group_weight(Endpoint, Default) ->
    case kz_json:get_integer_value(<<"weight">>, Endpoint) of
        'undefined' -> Default;
        N when N < 1 -> 1;
        N when N > 100 -> 100;
        N -> N
    end.

-spec strategy(kz_json:object()) -> ne_binary().
strategy(Data) ->
    kz_json:get_binary_value(<<"strategy">>, Data, ?DIAL_METHOD_SIMUL).

-spec freeswitch_strategy(kz_json:object()) -> ne_binary().
freeswitch_strategy(Data) ->
    case strategy(Data) of
        ?DIAL_METHOD_SIMUL -> ?DIAL_METHOD_SIMUL;
        _ -> ?DIAL_METHOD_SINGLE
    end.

-spec bridge(kz_proplist(), integer(), kapps_call:call()) -> kapps_api_bridge_return().
bridge(Command, Timeout, Call) ->
    kapps_call_command:send_command(Command, Call),
    kapps_call_command:b_bridge_wait(Timeout, Call).
