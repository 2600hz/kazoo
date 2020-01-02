%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_page_group).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    case get_endpoints(kz_json:get_list_value(<<"endpoints">>, Data), Call) of
        [] ->
            lager:notice("page group has no endpoints, moving to next callflow element"),
            cf_exe:continue(Call);
        Endpoints -> attempt_page(Endpoints, Data, Call)
    end.

-spec attempt_page(kz_json:objects(), kz_json:object(), kapps_call:call()) -> 'ok'.
attempt_page(Endpoints, Data, Call) ->
    Timeout = kz_json:get_integer_value(<<"timeout">>, Data, 5),
    CCVs = barge_option(Data),
    Options = audio_option(Data),
    case send_page(Endpoints, Timeout, CCVs, Options, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the page group - call finished normally"),
            cf_exe:stop(Call);
        {'error', _R} ->
            lager:info("error bridging to page group: ~p", [_R]),
            cf_exe:continue(Call)
    end.

-spec barge_option(kz_json:object()) -> tuple().
barge_option(Data) ->
    case kz_json:is_true(<<"barge_calls">>, Data) of
        'false' -> kz_json:from_list([{<<"Auto-Answer-Suppress-Notify">>, 'true'}]);
        'true' -> kz_json:from_list([{<<"Auto-Answer-Suppress-Notify">>, 'false'}])
    end.

-spec audio_option(kz_json:object()) -> tuple().
audio_option(Data) ->
    case kz_json:get_ne_binary_value(<<"audio">>, Data, <<"one-way">>) of
        <<"two-way">> -> kz_json:from_list([{<<"Two-Way-Audio">>, 'true'}]);
        <<"one-way">> -> kz_json:from_list([{<<"Two-Way-Audio">>, 'false'}]);
        _ -> kz_json:from_list([{<<"Two-Way-Audio">>, 'false'}])
    end.

-spec send_page(kz_json:objects(), integer(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
          {'error', 'timeout' | kz_json:object()} | {'ok', kz_json:object()}.
send_page(Endpoints, Timeout, CCVs, Options, Call) ->
    {CIDNumber, CIDName} = kz_attributes:caller_id(Call),
    lager:info("attempting page group of ~b members with caller-id (~s/~s)", [length(Endpoints), CIDNumber, CIDName]),
    kapps_call_command:b_page(Endpoints
                             ,Timeout
                             ,CIDName
                             ,CIDNumber
                             ,'undefined'
                             ,CCVs
                             ,Options
                             ,Call
                             ).

-spec get_endpoints(kz_term:api_objects(), kapps_call:call()) -> kz_json:objects().
get_endpoints(undefined, Call) -> get_endpoints([], Call);
get_endpoints(Members, Call) ->
    S = self(),
    Builders = [kz_process:spawn(
                  fun() ->
                          kz_log:put_callid(kapps_call:call_id(Call)),
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

-spec resolve_endpoint_ids(kz_json:objects(), kapps_call:call()) ->
          [{kz_term:ne_binary(), kz_json:object()}].
resolve_endpoint_ids(Members, Call) ->
    [{Id, kz_json:set_value(<<"source">>, kz_term:to_binary(?MODULE), Member)}
     || {Type, Id, Member} <- resolve_endpoint_ids(Members, [], Call)
            ,Type =:= <<"device">>
            ,Id =/= kapps_call:authorizing_id(Call)
    ].

-type endpoint_intermediate() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_object()}.
-type endpoint_intermediates() :: [endpoint_intermediate()].
-spec resolve_endpoint_ids(kz_json:objects(), endpoint_intermediates(), kapps_call:call()) ->
          endpoint_intermediates().
resolve_endpoint_ids([], EndpointIds, _) ->
    EndpointIds;
resolve_endpoint_ids([Member|Members], EndpointIds, Call) ->
    Id = kz_doc:id(Member),
    Type = kz_json:get_value(<<"endpoint_type">>, Member, <<"device">>),
    case kz_term:is_empty(Id)
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

-spec get_group_members(kz_json:object(), kz_term:ne_binary(), kapps_call:call()) ->
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
