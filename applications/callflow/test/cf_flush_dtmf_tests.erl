%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2023, 2600Hz
%%% @doc `flush_dtmf' callflow module tests
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @author Daniel Finke <danielfinke2011@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_flush_dtmf_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Verify the cleared DTMF collection is persisted on the call.
%% @end
%%------------------------------------------------------------------------------
persisted_test_() ->
    {'setup'
    ,fun() ->
             meck:new('cf_exe'),
             meck:expect('cf_exe', 'set_call', fun(_Call) -> 'ok' end),
             meck:expect('cf_exe', 'continue', fun(_Call) -> 'ok' end)
     end
    ,fun(_) -> meck:unload() end
    ,[fun() ->
              Call = kapps_call:set_dtmf_collection(<<"1234">>, kapps_call:new()),

              cf_flush_dtmf:handle(kz_json:new(), Call),

              DTMF = kapps_call:get_dtmf_collection(
                       meck:capture(1, 'cf_exe', 'set_call', 1, 1)
                      ),
              ?assertEqual('undefined', DTMF)
      end
     ]
    }.
