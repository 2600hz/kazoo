-module(gen_attachment_test).

-include_lib("eunit/include/eunit.hrl").

%% =======================================================================================
%% Generators
%% =======================================================================================
error_response_test_() ->
    InvalidCode = gen_attachment:error_response("200", <<>>),
    InvalidBody = gen_attachment:error_response(200, #{<<"unencoded">> => <<"map">>}),
    NotErrorResponse = #{<<"some">> => <<"supposed">>, <<"error">> => <<"response">>},
    ValidErrorResponse = gen_attachment:error_response(200, <<>>),

    [{"Invalid `error_code` value type"
     ,?_assertNot(gen_attachment:is_error_response(InvalidCode))
     }
    ,{"Invalid `error_body` value type"
     ,?_assertNot(gen_attachment:is_error_response(InvalidBody))
     }
    ,{"Invalid error_response object"
     ,?_assertNot(gen_attachment:is_error_response(NotErrorResponse))
     }
    ,{"Valid error response"
     ,?_assert(gen_attachment:is_error_response(ValidErrorResponse))
     }
    ].
