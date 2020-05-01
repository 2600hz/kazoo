%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_definition_tests).

-include_lib("eunit/include/eunit.hrl").

build_message_test_() ->
    Test = fun(Definition) -> kapi_definition:build_message([], Definition) end,
    Expect = fun(Id) -> {'error', "Proplist failed validation for " ++ kz_term:to_list(Id)} end,

    %% This way they can be converted to numeric types.
    Name = <<"13579">>,
    FriendlyName = <<"24680">>,
    Binding = <<"1234567890">>,
    Invalid = [<<>>, fun(_) -> kz_binary:rand_hex(4) end],
    Casters = [fun kz_term:to_list/1
              ,fun kz_term:to_float/1
              ,fun kz_term:to_integer/1
              ,fun kz_term:to_binary/1
              ,fun(Term) -> kz_term:to_atom(Term, 'true') end
              ,fun(_Term) -> 'true' end %% Mimic to_boolean/1 function.
              ,fun(_Term) -> "<0.123.0>" end %% Mimic to_pid/1 function.
              ],

    [{"Binding will be used if it is not empty and it is supported by kz_term:to_list/1 fun"
     ,[{lists:flatten(io_lib:format("Binding: ~p", [Cast(Binding)]))
       ,?_assertEqual(Expect(Cast(Binding)), Test(new_definition(Name, FriendlyName, Cast(Binding))))
       } || Cast <- Casters
      ]
     }
    ,{"If Binding (B) is invalid FriendlyName (FN) will be used if it is not empty and it is supported by kz_term:to_list/1 fun"
     ,[{lists:flatten(io_lib:format("B: ~p, FN: ~p", [Bind, Cast(FriendlyName)]))
       ,?_assertEqual(Expect(Cast(FriendlyName)), Test(new_definition(Name, Cast(FriendlyName), Bind)))
       } || Bind <- [<<>>, fun(_) -> timer:sleep(0) end], Cast <- Casters
      ]
     }
    ,{"If Binding (B) and FriendlyName (FN) are invalid Name (N) will be used if it is not empty and it is supported by kz_term:to_list/1 fun"
     ,[{lists:flatten(io_lib:format("B: ~p, FN: ~p, N: ~p", [Bind, FName, Cast(Name)]))
       ,?_assertEqual(Expect(Cast(Name)), Test(new_definition(Cast(Name), FName, Bind)))
       } || Bind <- Invalid, FName <- Invalid, Cast <- Casters
      ]
     }
    ,{"At least one value must be not empty and supported by kz_term:to_list/1 fun"
     ,[{lists:flatten(io_lib:format("B: ~p, FN: ~p, N: ~p", [Bind, FName, N]))
       ,?_assertException('error', 'badarg', Test(new_definition(N, FName, Bind)))
       } || Bind <- Invalid, FName <- Invalid, N <- Invalid
      ]
     }
    ].

-spec new_definition(kapi_definition:name(), kapi_definition:friendly_name(), kapi_definition:binding()) -> kapi_definition:api().
new_definition(Name, FriendlyName, Binding) ->
    Setters = [{fun kapi_definition:set_name/2, Name}
              ,{fun kapi_definition:set_friendly_name/2, FriendlyName}
               %% Only test the `false' branch of the case.
              ,{fun kapi_definition:set_validate_fun/2, fun(_) -> 'false' end}
              ,{fun kapi_definition:set_binding/2, Binding}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2, []}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).
