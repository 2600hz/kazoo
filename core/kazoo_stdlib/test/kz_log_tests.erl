%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_log_tests).

-include_lib("eunit/include/eunit.hrl").

log_test_() ->
    ST = try throw('just_for_fun')
         catch
             _E:_R:Stack ->
                 Stack
         end,
    [?_assertEqual(ok, kz_log:log_stacktrace(ST))].

put_callid_test_() ->
    ApiCallId = [{<<"Call-ID">>, <<"bla">>}],
    [?_assertEqual(<<"bla">>, begin kz_log:put_callid(<<"bla">>), kz_log:get_callid() end)
    ,?_assertEqual(bla, begin kz_log:put_callid(bla), kz_log:get_callid() end)
    ,?_assertEqual(<<"bla">>, begin kz_log:put_callid(ApiCallId), kz_log:get_callid() end)
    ,?_assertEqual(<<"bla">>, begin kz_log:put_callid(kz_json:from_list(ApiCallId)), kz_log:get_callid() end)
    ,?_assert(is_integer(begin kapps_util:set_startup(), kapps_util:startup() end))
    ].
