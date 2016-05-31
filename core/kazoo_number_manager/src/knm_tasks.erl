%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_tasks).
%% behaviour: tasks_provider

-compile({'no_auto_import', [node/0]}).

-export([help/0
        ,for/0
        ,node/0
        ,module/0
        ]).

-export([number/1
        ]).

-export([assign_to/4
        ]).

-include("knm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec for() -> ne_binary().
for() -> <<"number_management">>.

-spec module() -> module().
module() -> ?MODULE.

-spec node() -> node().
node() -> kz_util:to_binary(node()).

-spec help() -> kz_proplist().
help() ->
    [{<<"list">>
     ,kz_json:from_list([{<<"description">>, <<"List all numbers in the system">>}
                        ,{<<"mandatory">>, [
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"assign_to">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-assign numbers to the provided account">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"delete">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-remove numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"reserve">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-move numbers to reserved (adding if missing)">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"add">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-create numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ,<<"module_name">>
                                          ]}
                        ])
     }

    ,{<<"update_features">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-update features of numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ]}
                        ,{<<"optional">>, [<<"cnam.inbound">>
                                          ,<<"cnam.outbound">>
                                          ,<<"e911.street_address">>
                                               %%TODO: exhaustive list
                                          ]}
                        ])
     }
    ].

%% Verifiers

-spec number(ne_binary()) -> boolean().
number(<<"+", _/binary>>) -> 'true';
number(_) -> 'false'.

%% Appliers

-spec assign_to(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> any().
assign_to(Props, Number, AccountId, 'undefined') ->
    AuthBy = props:get_value('auth_account_id', Props),
    assign_to(Props, Number, AccountId, AuthBy);
assign_to(_Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', AuthBy}
              ],
    knm_number:move(Number, AccountId, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% End of Module.
