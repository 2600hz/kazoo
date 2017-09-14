-module(pqc_kazoo_model).

-export([new/1
        ,pp/1 % pretty print kazoo model

         %% Getters
        ,api/1
        ,account_id_by_name/2
        ,number_data/2
        ,ratedeck/1, ratedeck/2
        ,dedicated_ip/2

         %% Predicates
        ,has_accounts/1
        ,does_account_exist/2
        ,does_service_plan_exist/2
        ,is_account_missing/2
        ,is_number_in_account/3
        ,is_number_missing_in_account/3
        ,is_rate_missing/3, does_rate_exist/3
        ,does_ratedeck_exist/2
        ,has_rate_matching/3, has_service_plan_rate_matching/3
        ,does_ip_exist/2, is_ip_missing/2
        ,is_ip_unassigned/2
        ,is_ip_assigned/3

         %% Model manipulations
        ,add_service_plan/2, add_service_plan/3
        ,add_account/3
        ,add_service_plan_to_account/3
        ,add_number_to_account/4
        ,add_rate_to_ratedeck/3, remove_rate_from_ratedeck/3
        ,remove_number_from_account/2
        ,transition_number_state/3
        ,add_dedicated_ip/4
        ,assign_dedicated_ip/3
        ,unassign_dedicated_ip/2
        ]).

-include("kazoo_proper.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").

%% #{"prefix" => cost}
-type rate_data() :: #{ne_binary() => non_neg_integer()}.
-type service_plans() :: [{ne_binary(), kzd_service_plan:plan()}].

-type account_data() :: #{'name' => ne_binary()
                         ,'service_plans' => service_plans()
                         }.
%% #{AccountId => AccountData}
-type accounts() :: #{ne_binary() => account_data()}.

-type number_data() :: #{'number_state' => atom()
                        ,'account_id' => ne_binary()
                        }.
%% #{DID => NumberData}
-type numbers() :: #{ne_binary() => number_data()}.

-type ratedecks() :: #{ne_binary() => rate_data()}.

-type dedicated_ip() :: #{'host' => ne_binary()
                         ,'zone' => ne_binary()
                         ,'assigned_to' => api_ne_binary()
                         }.
%% #{IP => #{...}}
-type dedicated_ips() :: #{ne_binary() => dedicated_ip()}.

-record(kazoo_model
       ,{'accounts' = #{} :: accounts()
        ,'numbers' = #{} :: numbers()
        ,'ratedecks' = #{} :: ratedecks()
        ,'service_plans' = [] :: service_plans()
        ,'dedicated_ips' = #{} :: dedicated_ips()
        ,'api' :: pqc_cb_api:state()
        }
       ).
-type model() :: #kazoo_model{}.

-export_type([model/0]).

-spec new(pqc_cb_api:state()) -> model().
new(API) ->
    #kazoo_model{'api'=API}.

-spec pp(model()) -> kz_proplist().
pp(#kazoo_model{accounts=Account
               ,numbers=Numbers
               ,ratedecks=Ratedecks
               ,service_plans=ServicePlans
               ,dedicated_ips=IPs
               ,api=API
               }
  ) ->
    props:filter_empty(
      [{'accounts', Account}
      ,{'numbers', Numbers}
      ,{'ratedecks', Ratedecks}
      ,{'service_plans', ServicePlans}
      ,{'dedicated_ips', IPs}
      ,{'api', API}
      ]).

-spec api(model()) -> pqc_cb_api:state().
api(#kazoo_model{'api'=API}) -> API.

-spec ratedeck(model()) -> rate_data().
-spec ratedeck(model(), ne_binary()) -> rate_data().
ratedeck(Model) ->
    ratedeck(Model, ?KZ_RATES_DB).

ratedeck(#kazoo_model{'ratedecks'=Ratedecks}, Name) ->
    maps:get(Name, Ratedecks, #{}).

-spec dedicated_ip(model(), ne_binary()) -> dedicated_ip() | 'undefined'.
dedicated_ip(#kazoo_model{'dedicated_ips'=IPs}, IP) ->
    maps:get(IP, IPs, 'undefined').

-spec has_accounts(model()) -> boolean().
has_accounts(#kazoo_model{'accounts'=Accounts}) ->
    0 =:= map_size(Accounts).

-spec has_rate_matching(model(), ne_binary(), ne_binary()) ->
                               'false' |
                               {'true', integer()}.
has_rate_matching(#kazoo_model{}=Model, RatedeckId, DID) ->
    Ratedeck = ratedeck(Model, RatedeckId),
    has_rate_matching(Ratedeck, DID).

-spec has_service_plan_rate_matching(model(), ne_binary(), ne_binary()) -> boolean().
has_service_plan_rate_matching(#kazoo_model{'accounts'=Accounts
                                           ,'service_plans'=SPs
                                           }=Model
                              ,AccountId
                              ,DID
                              ) ->
    Account = maps:get(AccountId, Accounts, #{}),
    case maps:get('service_plans', Account, []) of
        [] ->
            ?DEBUG("no service plans for account, checking ~s", [?KZ_RATES_DB]),
            has_rate_matching(Model, ?KZ_RATES_DB, DID);
        [{SPId, SP}] ->
            case lists:member({SPId, SP}, SPs) of
                'false' -> 'false';
                'true' ->
                    [RatedeckId] = kzd_service_plan:items(SP, <<"ratedeck">>),
                    Ratedeck = ratedeck(Model, RatedeckId),
                    ?DEBUG("from service plan ~s, got ratedeck ~s: ~p", [SPId, RatedeckId, Ratedeck]),
                    has_rate_matching(Ratedeck, DID)
            end
    end.

-spec has_rate_matching(rate_data(), ne_binary()) -> boolean().
has_rate_matching(Ratedeck, <<"+", Number/binary>>) ->
    has_rate_matching(Ratedeck, Number);
has_rate_matching(Ratedeck, Number) ->
    case [Cost || {Prefix, Cost} <- maps:to_list(Ratedeck),
                  binary:match(Number, Prefix) =/= 'nomatch'
         ]
    of
        [] -> 'false';
        [Cost] -> {'true', Cost}
    end.

-spec account_id_by_name(model(), ne_binary()) -> api_ne_binary().
account_id_by_name(#kazoo_model{'accounts'=Accounts}, Name) ->
    case maps:get(Name, Accounts, 'undefined') of
        #{'id' := AccountId} -> AccountId;
        _ -> 'undefined'
    end.

-spec does_account_exist(model() | map(), ne_binary()) -> boolean().
does_account_exist(#kazoo_model{'accounts'=Accounts}, AccountId) ->
    does_account_exist(Accounts, AccountId);
does_account_exist(Accounts, AccountId) ->
    'undefined' =/= maps:get(AccountId, Accounts, 'undefined').

-spec is_account_missing(model(), ne_binary()) -> boolean().
is_account_missing(#kazoo_model{}=Model, Id) ->
    not does_account_exist(Model, Id).

-spec does_service_plan_exist(model(), ne_binary()) -> boolean().
does_service_plan_exist(#kazoo_model{'service_plans'=Plans}, PlanId) ->
    'false' =/= lists:keyfind(PlanId, 1, Plans).

-spec is_number_in_account(model(), ne_binary(), ne_binary()) -> boolean().
is_number_in_account(#kazoo_model{}=Model, AccountId, Number) ->
    case number_data(Model, Number) of
        #{'account_id' := AccountId} -> 'true';
        _ -> 'false'
    end.

-spec is_number_missing_in_account(model(), ne_binary(), ne_binary()) -> boolean().
is_number_missing_in_account(#kazoo_model{}=Model, AccountId, Number) ->
    not is_number_in_account(Model, AccountId, Number).

-spec is_rate_missing(model(), ne_binary(), kzd_rate:doc()) -> boolean().
is_rate_missing(#kazoo_model{}=Model, RatedeckId, RateDoc) ->
    Ratedeck = ratedeck(Model, RatedeckId),
    Prefix = kzd_rate:prefix(RateDoc),

    'undefined' =:= maps:get(Prefix, Ratedeck, 'undefined').

-spec does_rate_exist(model(), ne_binary(), kzd_rate:doc()) -> boolean().
does_rate_exist(Model, RatedeckId, RateDoc) ->
    not is_rate_missing(Model, RatedeckId, RateDoc).

-spec does_ratedeck_exist(model(), ne_binary()) -> boolean().
does_ratedeck_exist(#kazoo_model{}=Model, RatedeckId) ->
    Ratedeck = ratedeck(Model, RatedeckId),
    is_map(Ratedeck)
        andalso 0 < maps:size(Ratedeck).

-spec does_ip_exist(model(), ne_binary()) -> boolean().
does_ip_exist(Model, IP) ->
    'undefined' =/= dedicated_ip(Model, IP).

-spec is_ip_missing(model(), ne_binary()) -> boolean().
is_ip_missing(Model, IP) ->
    'undefined' =:= dedicated_ip(Model, IP).

-spec is_ip_assigned(model(), ne_binary(), ne_binary()) -> boolean().
is_ip_assigned(Model, AccountId, IP) ->
    case dedicated_ip(Model, IP) of
        #{'assigned_to' := AccountId} -> 'true';
        _ -> 'false'
    end.

-spec is_ip_unassigned(model(), ne_binary()) -> boolean().
is_ip_unassigned(Model, IP) ->
    IPInfo = dedicated_ip(Model, IP),
    'undefined' =:= maps:get('assigned_to', IPInfo, 'undefined').

-spec add_account(model(), ne_binary(), pqc_cb_api:response()) -> model().
add_account(#kazoo_model{'accounts'=Accounts}=State, Name, APIResp) ->
    ID = {'call', 'pqc_cb_response', 'account_id', [APIResp]},
    State#kazoo_model{'accounts' = Accounts#{Name => #{'id' => ID}
                                            ,ID => new_account(Name)
                                            }}.

-spec add_rate_to_ratedeck(model(), ne_binary(), kzd_rate:doc()) -> model().
add_rate_to_ratedeck(#kazoo_model{'ratedecks'=Ratedecks}=Model, RatedeckId, RateDoc) ->
    Ratedeck = ratedeck(Model, RatedeckId),
    UpdatedDeck = Ratedeck#{kzd_rate:prefix(RateDoc) => kzd_rate:rate_cost(RateDoc)},
    UpdatedDecks = Ratedecks#{RatedeckId => UpdatedDeck},
    Model#kazoo_model{'ratedecks'=UpdatedDecks}.

-spec remove_rate_from_ratedeck(model(), ne_binary(), kzd_rate:doc()) -> model().
remove_rate_from_ratedeck(#kazoo_model{'ratedecks'=Ratedecks}=Model, RatedeckId, RateDoc) ->
    Ratedeck = ratedeck(Model, RatedeckId),
    UpdatedDeck = maps:remove(kzd_rate:prefix(RateDoc), Ratedeck),

    Model#kazoo_model{'ratedecks'=Ratedecks#{RatedeckId => UpdatedDeck}}.

-spec add_number_to_account(model(), ne_binary(), ne_binary(), pqc_cb_api:response()) -> model().
add_number_to_account(#kazoo_model{'numbers'=Numbers}=Model, AccountId, Number, APIResp) ->
    NumberState = {'call', 'pqc_cb_response', 'number_state', [APIResp]},
    NumberData = #{'account_id' => AccountId
                  ,'number_state' => NumberState
                  },
    Model#kazoo_model{'numbers'=Numbers#{Number => NumberData}}.

-spec add_service_plan(model() | service_plans(), kzd_service_plan:doc()) -> model() | service_plans().
-spec add_service_plan(model(), ne_binary(), kzd_service_plan:doc()) -> model().
add_service_plan(#kazoo_model{'service_plans'=Plans}=Model, ServicePlan) ->
    Model#kazoo_model{'service_plans'=add_service_plan(Plans, ServicePlan)};
add_service_plan(Plans, ServicePlan) ->
    Id = kz_doc:id(ServicePlan),
    lists:ukeysort(1, [{Id, ServicePlan} | Plans]).

add_service_plan(Model, AccountId, ServicePlan) ->
    add_service_plan_to_account(add_service_plan(Model, ServicePlan)
                               ,AccountId
                               ,ServicePlan
                               ).

-spec add_service_plan_to_account(model(), ne_binary(), kzd_service_plan:doc()) -> model().
add_service_plan_to_account(#kazoo_model{'accounts'=Accounts}=Model, AccountId, ServicePlan) ->
    #{'service_plans' := Plans}=Account = maps:get(AccountId, Accounts),

    Account1 = Account#{'service_plans' => add_service_plan(Plans, ServicePlan)},
    Accounts1 = Accounts#{AccountId => Account1},
    Model#kazoo_model{'accounts'=Accounts1}.

-spec add_dedicated_ip(model(), ne_binary(), ne_binary(), ne_binary()) ->
                              model().
add_dedicated_ip(#kazoo_model{'dedicated_ips'=IPs}=Model, IP, Host, Zone) ->
    UpdatedIPs =
        case dedicated_ip(Model, IP) of
            #{'host' := Host
             ,'zone' := Zone
             } -> IPs;
            _ ->
                IPs#{IP => #{'host' => Host
                            ,'zone' => Zone
                            }
                    }
        end,
    Model#kazoo_model{'dedicated_ips'=UpdatedIPs}.

-spec assign_dedicated_ip(model(), ne_binary(), ne_binary()) ->
                                 model().
assign_dedicated_ip(#kazoo_model{'dedicated_ips'=IPs}=Model, AccountId, IP) ->
    IPInfo = dedicated_ip(Model, IP),
    Model#kazoo_model{'dedicated_ips'=IPs#{IP => IPInfo#{'assigned_to' => AccountId}}}.

-spec unassign_dedicated_ip(model(), ne_binary()) -> model().
unassign_dedicated_ip(#kazoo_model{'dedicated_ips'=IPs}=Model, IP) ->
    IPInfo = dedicated_ip(Model, IP),
    Model#kazoo_model{'dedicated_ips'=IPs#{IP => IPInfo#{'assigned_to' => 'undefined'}}}.

-spec remove_number_from_account(model(), ne_binary()) -> model().
remove_number_from_account(#kazoo_model{'numbers'=Numbers}=Model
                          ,Number
                          ) ->
    Model#kazoo_model{'numbers'=maps:remove(Number, Numbers)}.

-spec transition_number_state(model(), ne_binary(), pqc_cb_api:response()) -> model().
transition_number_state(#kazoo_model{'numbers'=Numbers}=Model, Number, APIResp) ->
    NumberData = number_data(Numbers, Number),
    NumberState = {'call', 'pqc_cb_response', 'number_state', [APIResp]},
    Model#kazoo_model{
      'numbers'=Numbers#{
                  Number => NumberData#{'number_state' => NumberState}
                 }
     }.

-spec number_data(map() | model(), ne_binary()) -> map() | 'undefined'.
number_data(#kazoo_model{'numbers'=Numbers}, Number) ->
    number_data(Numbers, Number);
number_data(Numbers, Number) ->
    maps:get(Number, Numbers, 'undefined').

-spec new_account(ne_binary()) -> account_data().
new_account(Name) ->
    #{'name' => Name
     ,'service_plans' => []
     }.
