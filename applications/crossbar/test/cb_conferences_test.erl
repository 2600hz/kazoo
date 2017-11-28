-module(cb_conferences_test).

-include_lib("eunit/include/eunit.hrl").

dial_test() ->
    Context = cb_context:new(),
    ConferenceId = kz_binary:rand_hex(16),
    Data = kz_json:from_list([]),
    {_Context1, EPs} = cb_conferences:build_valid_endpoints(Context, ConferenceId, Data),
    ?assertEqual([], EPs).
