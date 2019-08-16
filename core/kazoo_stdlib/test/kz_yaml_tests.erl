%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_yaml_tests).

-include_lib("eunit/include/eunit.hrl").

-define(STRING_TYPES_FILE, filename:join([code:lib_dir(kazoo_stdlib)
                                         , "test/fixtures/string_types.yml"
                                         ])).

encode_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_ReturnOfSetup) ->
             [test_string_types()
             ]
     end
    }.

setup() ->
    {ok, _} = application:ensure_all_started(yamerl).

cleanup(_) ->
    application:stop(yamerl).

test_string_types() ->
    {ok, Bin} = file:read_file(?STRING_TYPES_FILE),
    Yaml = kz_yaml:encode(kz_yaml:decode(Bin), #{string_style => 'best'}),
    [{"Encode various type of string scalars"
     ,[?_assertEqual(Bin, Yaml)]
     }
    ].
