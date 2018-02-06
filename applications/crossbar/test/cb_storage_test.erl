-module(cb_storage_test).

-include_lib("eunit/include/eunit.hrl").
-include("cb_token_restrictions_test.hrl").

%% =======================================================================================
%% Generators
%% =======================================================================================
maybe_check_storage_settings_test_() ->
    Context = cb_context:new(), %% By default context.resp_status = error.
    SuccessContext = cb_context:set_resp_status(Context, 'success'),

    [{"Skip check if context's `resp_status /= success`"
     ,?_assertEqual(Context, maybe_check(Context, ?HTTP_PUT))
     }
    ,{"Skip check if method == get"
     ,?_assertEqual(SuccessContext, maybe_check(SuccessContext, ?HTTP_GET))
     }
    ,{"Skip check if method == delete"
     ,?_assertEqual(SuccessContext, maybe_check(SuccessContext, ?HTTP_DELETE))
     }
    ].

%% =======================================================================================
%% Private functions
%% =======================================================================================
maybe_check(Context, HTTPVerb) ->
    cb_storage:maybe_check_storage_settings(Context, HTTPVerb).
