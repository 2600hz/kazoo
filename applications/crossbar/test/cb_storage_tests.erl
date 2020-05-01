%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_storage_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_fixturedb/include/kz_fixturedb.hrl").
-include("cb_token_restrictions_test.hrl").

-define(ACCOUNT_DB_CURRENT_MONTH
       ,kazoo_modb:get_modb(kzs_util:format_account_id(?FIXTURE_MASTER_ACCOUNT_ID))
       ).

%%%=============================================================================
%%% Fixtures / Generators
%%%=============================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
maybe_check_storage_settings_test_() ->
    {'setup'
    ,fun setup/0
    ,fun cleanup/1
    ,fun (_ReturnOfSetup) ->
             maybe_check_storage_settings()
     end
    }.

%%%=============================================================================
%%% Setup
%%%=============================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Required for s3 attachment handler
    _ = application:ensure_all_started('erlcloud'),
    Pid = kz_fixturedb_util:start_me(),
    meck:new('kz_fixturedb_db', ['unstick', 'passthrough']),
    meck:new('kz_fixturedb_doc', ['unstick', 'passthrough']),
    meck:expect('kz_fixturedb_db', 'db_exists', this_month_db_exists()),
    meck:expect('kz_fixturedb_doc', 'open_doc', fun open_doc/4),
    Pid.

cleanup(Pid) ->
    _ = application:stop('erlcloud'),
    kz_fixturedb_util:stop_me(Pid),
    meck:unload().

%%%=============================================================================
%%% Tests
%%%=============================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
maybe_check_storage_settings() ->
    UUID = kz_binary:rand_hex(16),
    Setters = [{fun cb_context:set_account_id/2, ?FIXTURE_MASTER_ACCOUNT_ID}
              ,{fun cb_context:set_doc/2, kz_json:from_map(s3_storage_plan(UUID))}
              ],
    %% By default context.resp_status = error.
    Context = cb_context:setters(cb_context:new(), Setters),
    SuccessContext = cb_context:set_resp_status(Context, 'success'),
    PutRespContext = maybe_check(SuccessContext, ?HTTP_PUT),

    [{"Skip check if context's `resp_status /= success`"
     ,?_assertEqual(Context, maybe_check(Context, ?HTTP_PUT))
     }
    ,{"Skip check if method == get"
     ,?_assertEqual(SuccessContext, maybe_check(SuccessContext, ?HTTP_GET))
     }
    ,{"Skip check if method == delete"
     ,?_assertEqual(SuccessContext, maybe_check(SuccessContext, ?HTTP_DELETE))
     }
    ,{"If storage settings validation fails, resp context should be /= than req context"
     ,?_assertNotEqual(SuccessContext, PutRespContext)
     }
    ,{"If storage settings validation fails, crossbar should return 400"
     ,?_assertEqual(400, cb_context:resp_error_code(PutRespContext))
     }
    ,{"If storage settings validation fails, crossbar should return an error message"
     ,?_assertEqual(<<"validation error">>, cb_context:resp_error_msg(PutRespContext))
     }
    ,{"If you provide invalid credentials for S3 it should return 403"
     ,?_assertEqual(403, s3_error_code(UUID, PutRespContext))
     }
    ,{"If you provide invalid credentials for S3 it should return an error message"
     ,?_assertEqual(<<"The AWS Access Key Id you provided does not exist in our records.">>,
                    s3_message(UUID, PutRespContext))
     }
    ].

%%%=============================================================================
%%% Helper functions
%%%=============================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
s3_storage_plan(UUID) ->
    #{<<"attachments">> =>
          #{UUID =>
                #{<<"handler">> => <<"s3">>
                 ,<<"name">> => <<"S3 Storage">>
                 ,<<"settings">> =>
                      #{<<"bucket">> => <<"my_S3_BUCKET">>
                       ,<<"key">> => <<"my_AWS_ACCESS_KEY">>
                       ,<<"secret">> => <<"my_AWS_SECRET_KEY">>
                       }
                 }
           }
     ,<<"id">> => ?FIXTURE_MASTER_ACCOUNT_ID
     ,<<"plan">> =>
          #{<<"modb">> =>
                #{<<"types">> =>
                      #{<<"mailbox_message">> =>
                            #{<<"attachments">> =>
                                  #{<<"handler">> => UUID
                                   }
                             }
                       }
                 }
           }
     }.

maybe_check(Context, HTTPVerb) ->
    cb_storage:maybe_check_storage_settings(Context, HTTPVerb).

%%------------------------------------------------------------------------------
%% @doc This is required for voice mail messages which their timestamp is missing
%% from metadata and their private media is missing or both private media and
%% mailbox doesn't have create/modified.
%% @end
%%------------------------------------------------------------------------------
this_month_db_exists() ->
    fun(Server, Db) ->
            %% why meck can't call the original mfa properly?
            %% meck_code_gen:get_current_call/1 returns undefined and it cause bad_match
            %% in meck:passthrough/1
            (kz_datamgr:db_classification(Db) =:= 'modb'
             andalso is_db_under_test(Db, ?ACCOUNT_DB_CURRENT_MONTH))
                orelse erlang:apply('kz_fixturedb_db_meck_original', 'db_exists', [Server, Db])
    end.

is_db_under_test(ThisMonth, ThisMonth) -> 'true';
is_db_under_test(_, _) ->                 'false'.

open_doc(Server, DbName, DocId, Options) ->
    case ?ACCOUNT_DB_CURRENT_MONTH of
        DbName -> {'ok', kz_json:new()};
        _ -> meck:passthrough([Server, DbName, DocId, Options])
    end.

-spec get_s3_key(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) ->
          pos_integer() | atom() | kz_term:ne_binary().
get_s3_key(Key, UUID, Context) ->
    Path = [<<"attachments.", UUID/binary>>, <<"invalid">>, Key],
    ValidationErrors = cb_context:validation_errors(Context),
    kz_json:get_value(Path, ValidationErrors).

-spec s3_error_code(kz_term:ne_binary(), cb_context:context()) ->
          pos_integer() | atom().
s3_error_code(UUID, Context) ->
    get_s3_key(<<"error_code">>, UUID, Context).

-spec s3_message(kz_term:ne_binary(), cb_context:context()) ->
          kz_term:ne_binary().
s3_message(UUID, Context) ->
    get_s3_key(<<"message">>, UUID, Context).
