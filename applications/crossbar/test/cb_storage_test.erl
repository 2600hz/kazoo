-module(cb_storage_test).

-include_lib("eunit/include/eunit.hrl").
-include("cb_token_restrictions_test.hrl").


%% =======================================================================================
%% Fixtures
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
    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),
    LinkPid.

cleanup(LinkPid) ->
    _ = erlang:exit(LinkPid, normal),
    _ = application:stop(kazoo_config),
    ?debugMsg(":: Stopped Kazoo FixtureDB").

%% =======================================================================================
%% Tests
%% =======================================================================================
maybe_check_storage_settings() ->
    StoragePlan = s3_storage_plan(),
    ?debugFmt("Generated StoragePlan: ~p", [StoragePlan]),
    %% By default context.resp_status = error.
    Context = cb_context:setters(cb_context:new()
                                ,[{fun cb_context:set_account_id/2, account_id()}
                                 ,{fun cb_context:set_doc/2, kz_json:from_map(StoragePlan)}
                                 ]
                                ),
    SuccessContext = cb_context:set_resp_status(Context, 'success'),
    ?debugFmt("Resp: ~p", [maybe_check(SuccessContext, ?HTTP_PUT)]),

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
%% Helper functions
%% =======================================================================================
account_id() ->
    <<"account0000000000000000000000001">>.

s3_storage_plan() ->
    UUID = kz_binary:rand_hex(16),
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
     ,<<"id">> => account_id()
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
