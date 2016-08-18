%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_page_group).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    case get_endpoints(kz_json:get_value(<<"endpoints">>, Data, []), Call) of
        [] ->
            lager:notice("page group has no endpoints, moving to next callflow element"),
            cf_exe:continue(Call);
        Endpoints -> attempt_page(Endpoints, Data, Call)
    end.

-spec attempt_page(kz_json:objects(), kz_json:object(), kapps_call:call()) -> 'ok'.
attempt_page(Endpoints, Data, Call) ->
    Timeout = kz_json:get_binary_value(<<"timeout">>, Data, 5),
    lager:info("attempting page group of ~b members", [length(Endpoints)]),
    case kapps_call_command:b_page(Endpoints, Timeout, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the page group - call finished normally"),
            cf_exe:stop(Call);
        {'error', _R} ->
            lager:info("error bridging to page group: ~p", [_R]),
            cf_exe:continue(Call)
    end.

-spec get_endpoints(kz_json:objects(), kapps_call:call()) -> kz_json:objects().
get_endpoints(Members, Call) ->
    S = self(),
    Builders = [kz_util:spawn(
                  fun() ->
                          kz_util:put_callid(kapps_call:call_id(Call)),
                          S ! {self(), catch kz_endpoint:build(EndpointId, Member, Call)}
                  end)
                || {EndpointId, Member} <- resolve_endpoint_ids(Members, Call)
               ],
    lists:foldl(fun(Pid, Acc) ->
                        receive
                            {Pid, {'ok', EP}} when is_list(EP) ->
                                EP ++ Acc;
                            {Pid, {'ok', EP}} ->
                                [EP | Acc];
                            {Pid, _} -> Acc
                        end
                end, [], Builders).

-spec resolve_endpoint_ids(kz_json:objects(), kapps_call:call()) -> [{ne_binary(), kz_json:object()}].
resolve_endpoint_ids(Members, Call) ->
    [{Id, kz_json:set_value(<<"source">>, ?MODULE, Member)}
     || {Type, Id, Member} <- resolve_endpoint_ids(Members, [], Call)
            ,Type =:= <<"device">>
            ,Id =/= kapps_call:authorizing_id(Call)
    ].

-type endpoint_intermediate() :: {ne_binary(), ne_binary(), api_object()}.
-type endpoint_intermediates() :: [endpoint_intermediate()].
-spec resolve_endpoint_ids(kz_json:objects(), endpoint_intermediates(), kapps_call:call()) ->
                                  endpoint_intermediates().
resolve_endpoint_ids([], EndpointIds, _) ->
    EndpointIds;
resolve_endpoint_ids([Member|Members], EndpointIds, Call) ->
    Id = kz_doc:id(Member),
    Type = kz_json:get_value(<<"endpoint_type">>, Member, <<"device">>),
    case kz_util:is_empty(Id)
        orelse lists:keymember(Id, 2, EndpointIds)
        orelse Type
    of
        'true' ->
            resolve_endpoint_ids(Members, EndpointIds, Call);
        <<"group">> ->
            lager:info("member ~s is a group, merge the group's members", [Id]),
            GroupMembers = get_group_members(Member, Id, Call),
            Ids = resolve_endpoint_ids(GroupMembers, EndpointIds, Call),
            resolve_endpoint_ids(Members, [{Type, Id, 'undefined'}|Ids], Call);
        <<"user">> ->
            lager:info("member ~s is a user, get all the user's endpoints", [Id]),
            Ids = lists:foldr(fun(EndpointId, Acc) ->
                                      case lists:keymember(EndpointId, 2, Acc) of
                                          'true' -> Acc;
                                          'false' ->
                                              [{<<"device">>, EndpointId, Member}|Acc]
                                      end
                              end
                             ,[{Type, Id, 'undefined'}|EndpointIds]
                             ,kz_attributes:owned_by(Id, <<"device">>, Call)),
            resolve_endpoint_ids(Members, Ids, Call);
        <<"device">> ->
            resolve_endpoint_ids(Members, [{Type, Id, Member}|EndpointIds], Call)
    end.

-spec get_group_members(kz_json:object(), ne_binary(), kapps_call:call()) ->
                               kz_json:objects().
get_group_members(Member, Id, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:open_cache_doc(AccountDb, Id) of
        {'ok', JObj} ->
            Endpoints = kz_json:get_ne_value(<<"endpoints">>, JObj, kz_json:new()),
            DefaultDelay = kz_json:get_value(<<"delay">>, Member),
            DefaultTimeout = kz_json:get_value(<<"timeout">>, Member),
            [kz_json:set_values([{<<"endpoint_type">>, kz_json:get_value([Key, <<"type">>], Endpoints)}
                                ,{<<"id">>, Key}
                                ,{<<"delay">>, kz_json:get_value([Key, <<"delay">>], Endpoints, DefaultDelay)}
                                ,{<<"timeout">>, kz_json:get_value([Key, <<"timeout">>], Endpoints, DefaultTimeout)}
                                ], Member)
             || Key <- kz_json:get_keys(Endpoints)
            ];
        {'error', _R} ->
            lager:warning("unable to lookup members of group ~s: ~p", [Id, _R]),
            []
    end.
