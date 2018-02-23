-module(cb_storage_test).

-include_lib("eunit/include/eunit.hrl").
-include("cb_token_restrictions_test.hrl").

-define(ACCOUNT_ID_UNDER_TEST, <<"account0000000000000000000000001">>).
-define(ACCOUNT_DB_UNDER_TEST, <<"account%2Fac%2Fco%2Funt0000000000000000000000001">>).

-define(LEGACY_VIEW, <<"vmboxes/legacy_msg_by_timestamp">>).

%% =======================================================================================
%% Fixtures / Generators
%% =======================================================================================
maybe_check_storage_settings_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun (_ReturnOfSetup) ->
             maybe_check_storage_settings()
     end
    }.

%% =======================================================================================
%% Setup
%% =======================================================================================
setup() ->
    ?debugMsg(":: Setting up Kazoo FixtureDB"),
    %% Required for s3 attachment handler
    _ = application:ensure_all_started(erlcloud),
    Pid = kz_fixturedb_util:start_me(),
    meck:new(kz_datamgr, [unstick, passthrough]),
    meck:expect(kz_datamgr, get_results, check_kz_datamgr_history()),
    %% Fix `kz_att_util:format_url/3' issue
    meck:expect(kz_datamgr, open_cache_doc, fun open_cache_doc/2),
    meck:new(kz_fixturedb_db, [unstick, passthrough]),
    meck:expect(kz_fixturedb_db, db_exists, this_month_db_exists()),
    Pid.

cleanup(Pid) ->
    _ = application:stop(erlcloud),
    kz_fixturedb_util:stop_me(Pid),
    meck:unload(),
    ?debugMsg(":: Stopped Kazoo FixtureDB").

%% =======================================================================================
%% Tests
%% =======================================================================================
maybe_check_storage_settings() ->
    UUID = kz_binary:rand_hex(16),
    Setters = [{fun cb_context:set_account_id/2, ?ACCOUNT_ID_UNDER_TEST}
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

%% =======================================================================================
%% Helper functions
%% =======================================================================================
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
     ,<<"id">> => ?ACCOUNT_ID_UNDER_TEST
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

%% this is required for voice mail messages which their timestamp is missing from metadata and
%% their private media is missing or both private media and mailbox doesn't have create/modified
this_month_db_exists() ->
    fun(Server, Db) ->
            %% why meck can't call the original mfa properly?
            %% meck_code_gen:get_current_call/1 returns undefined and it cause bad_match
            %% in meck:passthrough/1
            (kz_datamgr:db_classification(Db) =:= 'modb'
             andalso is_db_under_test(Db, kazoo_modb:get_modb(kz_util:format_account_id(Db))))
                orelse erlang:apply('kz_fixturedb_db_meck_original', db_exists, [Server, Db])
    end.

is_db_under_test(ThisMonth, ThisMonth) ->                                             true;
is_db_under_test(<<"account%2Fac%2Fco%2Funt0000000000000000000000001-201710">>, _) -> true;
is_db_under_test(_, _) ->                                                             false.

-define(GET_LEGACY_CALL, {kz_datamgr, get_results, [?ACCOUNT_DB_UNDER_TEST, ?LEGACY_VIEW, [{limit, 2000}, descending]]}).

%% Checking history calls  to kz_datamgr:get_results to see if any calls happens to
%% get legacy messages, if yes return empty result to stop the process.
check_kz_datamgr_history() ->
    fun(Db, ?LEGACY_VIEW=View, Options) ->
            History = meck:history(kz_datamgr),

            %% meck:history returns:
            %% * {CallerPid, MFA, Result}
            %% * {CallerPid, MFA, ExceptionClass, ExceptionReason, StackTrace}
            case lists:keyfind(?GET_LEGACY_CALL, 2, History) of
                {_Pid, _MFA, {ok, ViewResult}} when length(ViewResult) == 10 ->
                    %% ViewResult length is 10. This should be same as what is inside vmboxes+legacy_msg_by_timestamp.json
                    %% in ?ACCOUNT_DB_UNDER_TEST FixtureDb directory.
                    {ok, []};
                _ ->
                    meck:passthrough([Db, View, Options])
            end;
       (Db, View, Options) ->
            meck:passthrough([Db, View, Options])
    end.

-spec open_cache_doc(kz_term:ne_binary(), kz_term:ne_binary()) ->
                            {ok, kz_json:object()}.
open_cache_doc(_DbName, _DocId) ->
    {ok, kz_json:new()}.

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
