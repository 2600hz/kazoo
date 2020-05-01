%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_schemas_tests).

-include_lib("eunit/include/eunit.hrl").

base_schema_test_() ->
    Test = fun kapi_schemas:base_schema/2,
    KAPI = kz_binary:rand_hex(4),
    API = kz_binary:rand_hex(4),
    APIFun = fun() -> kz_binary:rand_hex(4) end,

    [{"KAPI must be binary"
     ,[?_assertEqual(base_schema(KAPI, API), Test(KAPI, API))
      ,?_assertException('error', 'function_clause', Test(binary_to_list(KAPI), API))
      ]
     }
    ,{"API may be a callback function"
     ,?_assertEqual(base_schema(KAPI, APIFun), Test(KAPI, APIFun))
     }
    ].

-spec base_schema(kz_term:ne_binary(), kapi_definition:name()) -> kz_json:object().
base_schema(<<KAPI/binary>>, <<API/binary>>) ->
    build_base_schema(<<"kapi.", KAPI/binary, ".", API/binary>>
                     ,<<"AMQP API for ", KAPI/binary, ".", API/binary>>
                     );
base_schema(<<KAPI/binary>>, _) ->
    build_base_schema(<<"kapi.", KAPI/binary>>, <<"AMQP API for ", KAPI/binary>>).

-spec build_base_schema(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_json:object().
build_base_schema(Id, Description) ->
    kz_json:from_list([{<<"_id">>, Id}
                      ,{<<"$schema">>, <<"http://json-schema.org/draft-04/schema#">>}
                      ,{<<"description">>, Description}
                      ,{<<"type">>, <<"object">>}
                      ]).
