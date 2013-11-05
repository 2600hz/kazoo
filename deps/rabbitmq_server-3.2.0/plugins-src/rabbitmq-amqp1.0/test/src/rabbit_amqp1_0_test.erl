%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_amqp1_0_test).

-include("rabbit_amqp1_0.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(rabbit_amqp1_0_util, [serial_add/2, serial_diff/2, serial_compare/2]).

serial_arithmetic_test() ->
    ?assertEqual(1, serial_add(0, 1)),
    ?assertEqual(16#7fffffff, serial_add(0, 16#7fffffff)),
    ?assertEqual(0, serial_add(16#ffffffff, 1)),
    %% Cannot add more than 2 ^ 31 - 1
    ?assertExit({out_of_bound_serial_addition, _, _},
                serial_add(200, 16#80000000)),
    ?assertEqual(1, serial_diff(1, 0)),
    ?assertEqual(2, serial_diff(1, 16#ffffffff)),
    ?assertEqual(-2, serial_diff(16#ffffffff, 1)),
    ?assertExit({indeterminate_serial_diff, _, _},
                serial_diff(0, 16#80000000)),
    ?assertExit({indeterminate_serial_diff, _, _},
                serial_diff(16#ffffffff, 16#7fffffff)),
    passed.
