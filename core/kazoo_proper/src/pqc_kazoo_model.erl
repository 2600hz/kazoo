-module(pqc_kazoo_model).

-export([new/1

         %% Getters
        ,api/1
        ,account_id_by_name/2
        ,number_data/2

         %% Predicates
        ,has_accounts/1
        ,does_account_exist/2
        ,is_account_missing/2
        ,is_number_in_account/3
        ,is_number_missing_in_account/3
        ,is_account_rate_missing/3
        ,is_system_rate_missing/2

        ,has_account_rate_matching/3
        ,has_system_rate_matching/2

         %% Model manipulations
        ,add_account/3
        ,add_number_to_account/4
        ,add_rate_to_account/3
        ,add_rate_to_system/2
        ,remove_number_from_account/2
        ,transition_number_state/3
        ]).

-include("kazoo_proper.hrl").

%% #{"prefix" => cost}
-type rate_data() :: #{ne_binary() => integer()}.

-type account_data() :: #{'id' => ne_binary()
                         ,'rates' => rate_data()
                         }.
-type accounts() :: #{ne_binary() => account_data()}.

-type number_data() :: #{'number_state' => atom()
                        ,'account_id' => ne_binary()
                        }.
-type numbers() :: #{ne_binary() => number_data()}.

-record(kazoo_model
       ,{'accounts' = #{} :: accounts()
        ,'numbers' = #{} :: numbers()
        ,'rates' = #{} :: rate_data()
        ,'api' :: pqc_cb_api:state()
        }
       ).
-type model() :: #kazoo_model{}.

-export_type([model/0]).

-spec new(pqc_cb_api:state()) -> model().
new(API) ->
    #kazoo_model{'api'=API}.

-spec api(model()) -> pqc_cb_api:state().
api(#kazoo_model{'api'=API}) -> API.

-spec has_accounts(model()) -> boolean().
has_accounts(#kazoo_model{'accounts'=Accounts}) ->
    0 =:= map_size(Accounts).

-spec has_account_rate_matching(model(), ne_binary(), ne_binary()) -> boolean().
has_account_rate_matching(#kazoo_model{'accounts'=Accounts}, AccountId, DID) ->
    #{'rates' := Ratedeck} = maps:get(AccountId, Accounts),
    has_rate_matching(Ratedeck, DID).

-spec has_system_rate_matching(model(), ne_binary()) -> boolean().
has_system_rate_matching(#kazoo_model{'rates'=Ratedeck}, DID) ->
    has_rate_matching(Ratedeck, DID).

has_rate_matching(Ratedeck, <<"+", Number/binary>>) ->
    has_rate_matching(Ratedeck, Number);
has_rate_matching(Ratedeck, Number) ->
    lists:any(fun(Prefix) -> binary:match(Number, Prefix) =/= 'nomatch' end
             ,maps:keys(Ratedeck)
             ).

-spec account_id_by_name(model(), ne_binary()) -> api_ne_binary().
account_id_by_name(#kazoo_model{'accounts'=Accounts}, Name) ->
    maps:get(Name, Accounts, 'undefined').

-spec does_account_exist(model() | map(), ne_binary()) -> boolean().
does_account_exist(#kazoo_model{'accounts'=Accounts}, AccountId) ->
    does_account_exist(Accounts, AccountId);
does_account_exist(Accounts, AccountId) ->
    'undefined' =/= maps:get(AccountId, Accounts, 'undefined').

-spec is_account_missing(model(), ne_binary()) -> boolean().
is_account_missing(#kazoo_model{}=Model, Id) ->
    not does_account_exist(Model, Id).

-spec is_number_in_account(model(), ne_binary(), ne_binary()) -> boolean().
is_number_in_account(#kazoo_model{}=Model, AccountId, Number) ->
    case number_data(Model, Number) of
        #{'account_id' := AccountId} -> 'true';
        _ -> 'false'
    end.

-spec is_number_missing_in_account(model(), ne_binary(), ne_binary()) -> boolean().
is_number_missing_in_account(#kazoo_model{}=Model, AccountId, Number) ->
    not is_number_in_account(Model, AccountId, Number).

-spec is_account_rate_missing(model(), ne_binary(), kzd_rate:doc()) -> boolean().
is_account_rate_missing(#kazoo_model{'accounts'=Accounts}, AccountId, RateDoc) ->
    case maps:get(AccountId, Accounts, 'undefined') of
        #{'rates' := Ratedeck} ->
            Prefix = kzd_rate:prefix(RateDoc),
            'undefined' =:= maps:get(Prefix, Ratedeck, 'undefined');
        _Account -> 'true'
    end.

-spec is_system_rate_missing(model(), kzd_rate:doc()) -> boolean().
is_system_rate_missing(#kazoo_model{'rates'=Ratedeck}, RateDoc) ->
    Prefix = kzd_rate:prefix(RateDoc),
    'undefined' =:= maps:get(Prefix, Ratedeck, 'undefined').

-spec add_account(model(), ne_binary(), pqc_cb_api:response()) -> model().
add_account(#kazoo_model{'accounts'=Accounts}=State, Name, APIResp) ->
    ID = {'call', 'pqc_cb_response', 'account_id', [APIResp]},
    State#kazoo_model{'accounts' = Accounts#{Name => #{'id' => ID}
                                            ,ID => #{'name' => Name
                                                    ,'rates' => #{}
                                                    }
                                            }}.

-spec add_rate_to_account(model(), ne_binary(), kzd_rate:doc()) -> model().
add_rate_to_account(#kazoo_model{'accounts'=Accounts}=Model, AccountId, RateDoc) ->
    #{'rates':=Ratedeck}=Account = maps:get(AccountId, Accounts),

    Model#kazoo_model{'accounts'=Accounts#{
                                   AccountId => Account#{
                                                  'rates' => Ratedeck#{
                                                               kzd_rate:prefix(RateDoc) => kzd_rate:rate_cost(RateDoc)
                                                              }
                                                 }
                                  }
                     }.

-spec add_rate_to_system(model(), kzd_rate:doc()) -> model().
add_rate_to_system(#kazoo_model{'rates'=Ratedeck}=Model, RateDoc) ->
    Model#kazoo_model{'rates'= Ratedeck#{kzd_rate:prefix(RateDoc) => kzd_rate:rate_cost(RateDoc)}}.

-spec add_number_to_account(model(), ne_binary(), ne_binary(), pqc_cb_api:response()) -> model().
add_number_to_account(#kazoo_model{'numbers'=Numbers}=Model, AccountId, Number, APIResp) ->
    NumberState = {'call', 'pqc_cb_response', 'number_state', [APIResp]},
    NumberData = #{'account_ids' => AccountId
                  ,'numbers_state' => NumberState
                  },
    Model#kazoo_model{'numbers'=Numbers#{Number => NumberData}}.

-spec remove_number_from_account(model(), ne_binary()) -> model().
remove_number_from_account(#kazoo_model{'numbers'=Numbers}=Model
                          ,Number
                          ) ->
    Model#kazoo_model{'numbers'=maps:remove(Number, Numbers)}.

-spec transition_number_state(model(), ne_binary(), pqc_cb_api:response()) -> model().
transition_number_state(#kazoo_model{'numbers'=Numbers}=Model, Number, APIResp) ->
    NumberData = number_data(Numbers, Number),
    NumberState = {'call', 'pqc_cb_response', 'number_state', [APIResp]},
    Model#kazoo_model{'numbers'=Numbers#{Number => NumberData#{'numbers_state' => NumberState}}}.

-spec number_data(map() | model(), ne_binary()) -> map() | 'undefined'.
number_data(#kazoo_model{'numbers'=Numbers}, Number) ->
    number_data(Numbers, Number);
number_data(Numbers, Number) ->
    maps:get(Number, Numbers, 'undefined').
