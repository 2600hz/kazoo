%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_error_tests).

-include_lib("eunit/include/eunit.hrl").

%% =======================================================================================
%% Generators
%% =======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
error_response_test_() ->
    ErrorReason = <<"unknown error">>,
    WithoutRoutines = kz_att_error:new(ErrorReason),
    WithFetchRoutines = kz_att_error:new(ErrorReason, fetch_routines()),
    WithPutRoutines = kz_att_error:new(ErrorReason, put_routines()),
    FetchExtendedError = #{attachment_name => att_name()
                          ,db_name => db_name()
                          ,document_id => document_id()
                          ,handler_props => []
                          ,resp_code => 500
                          ,resp_body => <<>>
                          ,resp_headers => []
                          },
    PutExtendedError = FetchExtendedError#{handler_props => #{}, options => options()},

    %% Attachment handlers must always return extended errors (3 elements tuple),
    %% e.g: `{error, Reason, ExtendedError}'.
    [{"error_verbosity key not set (without routines)"
     ,?_assertEqual({'error', ErrorReason, #{resp_code => 500
                                            ,resp_body => <<>>
                                            ,resp_headers => []
                                            }
                    }
                   ,WithoutRoutines
                   )
     }
    ,{"error_verbosity key not set (using fetch routines)"
     ,?_assertEqual({'error', ErrorReason, FetchExtendedError}, WithFetchRoutines)
     }
    ,{"error_verbosity key not set (using put routines)"
     ,?_assertEqual({'error', ErrorReason, PutExtendedError}, WithPutRoutines)
     }
    ].

%% =======================================================================================
%% Helpers
%% =======================================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
att_content() -> <<"content">>.
att_name() -> <<"attname">>.
db_name() -> <<"mydb">>.
document_id() -> <<"docid">>.
options() -> [].

fetch_routines() ->
    fetch_routines([]).

fetch_routines(HandlerProps) ->
    kz_att_error:fetch_routines(HandlerProps, db_name(), document_id(), att_name()).

put_routines() ->
    put_routines([]).

put_routines(Options) ->
    kz_att_error:put_routines(#{}, db_name(), document_id(), att_name(), att_content(), Options).
