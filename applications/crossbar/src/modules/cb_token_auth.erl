%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% Token auth module
%%%
%%% This is a simple auth mechanism, once the user has aquired an
%%% auth token this module will allow access.  This module should be
%%% updated to be FAR more robust.
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_token_auth).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ,delete/1
        ,authenticate/1
        ,authorize/1
        ,finish_request/1
        ]).

-include("crossbar.hrl").

-define(LOOP_TIMEOUT,
        kapps_config:get_integer(?APP_NAME, <<"token_auth_expiry">>, ?SECONDS_IN_HOUR)).

-define(PERCENT_OF_TIMEOUT,
        kapps_config:get_integer(?APP_NAME, <<"expiry_percentage">>, 75)).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.token_auth">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.token_auth">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.token_auth">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.token_auth">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.finish_request.*.*">>, ?MODULE, 'finish_request'),
    ok.

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_DELETE, ?HTTP_GET].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), ne_binary()) -> cb_context:context().
validate(Context, ?HTTP_GET) ->
    JObj = crossbar_util:response_auth(
             kz_json:public_fields(cb_context:auth_doc(Context))
            ),
    Setters = [{fun cb_context:set_resp_status/2, 'success'}
              ,{fun cb_context:set_resp_data/2, JObj}
              ],
    cb_context:setters(Context, Setters);
validate(Context, ?HTTP_DELETE) ->
    case cb_context:auth_doc(Context) of
        'undefined' -> Context;
        AuthDoc ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_doc/2, AuthDoc}
                               ])
    end.

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    AuthToken = cb_context:auth_token(Context),
    case kz_datamgr:del_doc(?KZ_TOKEN_DB, AuthToken) of
        {'ok', _} ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_resp_data/2, 'undefined'}
                               ,{fun cb_context:set_doc/2, 'undefined'}
                               ,{fun cb_context:set_auth_doc/2, 'undefined'}
                               ,{fun cb_context:set_auth_token/2, 'undefined'}
                               ,{fun cb_context:set_auth_account_id/2, 'undefined'}
                               ]);
        {'error', _E} ->
            lager:debug("failed to delete auth token ~s: ~p", [AuthToken, _E]),
            Context
    end.

-spec finish_request(cb_context:context()) -> 'ok'.
-spec finish_request(cb_context:context(), api_object()) -> 'ok'.
finish_request(Context) ->
    finish_request(Context, cb_context:auth_doc(Context)).

finish_request(_Context, 'undefined') -> 'ok';
finish_request(Context, AuthDoc) ->
    cb_context:put_reqid(Context),
    maybe_save_auth_doc(AuthDoc).

-spec maybe_save_auth_doc(kz_json:object()) -> any().
maybe_save_auth_doc(OldAuthDoc) ->
    OldAuthModified = kz_doc:modified(OldAuthDoc),
    Now = kz_util:current_tstamp(),

    ToSaveTimeout = (?LOOP_TIMEOUT * ?PERCENT_OF_TIMEOUT) div 100,

    TimeLeft = Now - (OldAuthModified + ToSaveTimeout),

    case TimeLeft > 0 of
        'true' ->
            lager:debug("auth doc is past time (~ps after) to be saved, saving", [TimeLeft]),
            kz_datamgr:ensure_saved(?KZ_TOKEN_DB
                                   ,kz_doc:set_modified(OldAuthDoc, Now)
                                   );
        'false' ->
            lager:debug("auth doc is too new (~ps to go), not saving", [TimeLeft*-1])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    %% if the request is directly for token_auth (only)
    %% then allow GET and DELETE
    [{<<"token_auth">>, []}] =:= cb_context:req_nouns(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
-spec authenticate(cb_context:context(), atom()) ->
                          'false' |
                          {'true' | 'halt', cb_context:context()}.
authenticate(Context) ->
    authenticate(Context, cb_context:auth_token_type(Context)).

authenticate(Context, 'x-auth-token') ->
    _ = cb_context:put_reqid(Context),
    case kz_buckets:consume_tokens(?APP_NAME
                                  ,cb_modules_util:bucket_name(Context)
                                  ,cb_modules_util:token_cost(Context)
                                  )
    of
        'true' ->
            check_auth_token(Context
                            ,cb_context:auth_token(Context)
                            ,cb_context:magic_pathed(Context)
                            );
        'false' ->
            lager:warning("rate limiting threshold hit for ~s!", [cb_context:client_ip(Context)]),
            {'halt', cb_context:add_system_error('too_many_requests', Context)}
    end;
authenticate(_Context, _TokenType) -> 'false'.

-spec check_auth_token(cb_context:context(), api_binary(), boolean()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_auth_token(_Context, <<>>, MagicPathed) -> MagicPathed;
check_auth_token(_Context, 'undefined', MagicPathed) -> MagicPathed;
check_auth_token(Context, AuthToken, _MagicPathed) ->
    Options = [{<<"account_id">>, cb_context:req_header(Context, 'x-auth-account-id')}],
    lager:debug("checking auth token"),
    case crossbar_auth:validate_auth_token(AuthToken, props:filter_undefined(Options)) of
        {'ok', JObj} -> is_expired(Context, JObj);
        {'error', R} ->
            lager:debug("failed to authenticate token auth, ~p", [R]),
            'false'
    end.

-spec is_expired(cb_context:context(), kz_json:object()) ->
                        boolean() |
                        {'halt', cb_context:context()}.
is_expired(Context, JObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, JObj),
    case kz_util:is_account_expired(AccountId) of
        'false' -> check_as(Context, JObj);
        {'true', Expired} ->
            _ = kz_util:spawn(fun kz_util:maybe_disable_account/1, [AccountId]),
            Cause =
                kz_json:from_list(
                  [{<<"message">>, <<"account expired">>}
                  ,{<<"cause">>, Expired}
                  ]
                 ),
            Context1 = cb_context:add_validation_error(<<"account">>, <<"expired">>, Cause, Context),
            {'halt', Context1}
    end.

-spec check_as(cb_context:context(), kz_json:object()) ->
                      boolean() |
                      {'true', cb_context:context()}.
check_as(Context, JObj) ->
    case kz_json:get_value(<<"account_id">>, JObj, 'undefined') of
        'undefined' -> {'true', set_auth_doc(Context, JObj)};
        AccountId -> check_as_payload(Context, JObj, AccountId)
    end.

-spec check_as_payload(cb_context:context(), kz_json:object(), ne_binary()) ->
                              boolean() |
                              {'true', cb_context:context()}.
check_as_payload(Context, JObj, AccountId) ->
    case {kz_json:get_value([<<"as">>, <<"account_id">>], JObj, 'undefined')
         ,kz_json:get_value([<<"as">>, <<"owner_id">>], JObj, 'undefined')
         }
    of
        {'undefined', _} -> {'true', set_auth_doc(Context, JObj)};
        {_, 'undefined'} -> {'true', set_auth_doc(Context, JObj)};
        {AsAccountId, AsOwnerId} -> check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId)
    end.

-spec check_descendants(cb_context:context(), kz_json:object()
                       ,ne_binary() ,ne_binary() ,ne_binary()
                       ) ->
                               boolean() |
                               {'true', cb_context:context()}.
check_descendants(Context, JObj, AccountId, AsAccountId, AsOwnerId) ->
    case get_descendants(AccountId) of
        {'error', _} -> 'false';
        {'ok', Descendants} ->
            case lists:member(AsAccountId, Descendants) of
                'false' -> 'false';
                'true' ->
                    JObj1 = kz_json:set_values(
                              [{<<"account_id">>, AsAccountId}
                              ,{<<"owner_id">>, AsOwnerId}
                              ]
                                              ,JObj
                             ),
                    {'true', set_auth_doc(Context, JObj1)}
            end
    end.

-spec get_descendants(ne_binary()) ->
                             {'ok', ne_binaries()} |
                             kz_data:data_error().
get_descendants(AccountId) ->
    case kz_datamgr:get_results(<<"accounts">>
                               ,<<"accounts/listing_by_descendants">>
                               ,[{'startkey', [AccountId]}
                                ,{'endkey', [AccountId, kz_json:new()]}
                                ]
                               )
    of
        {'error', _}=Error -> Error;
        {'ok', JObjs} ->
            {'ok', [kz_doc:id(JObj) || JObj <- JObjs]}
    end.

-spec set_auth_doc(cb_context:context(), kz_json:object()) ->
                          cb_context:context().
set_auth_doc(Context, JObj) ->
    Setters = [{fun cb_context:set_auth_doc/2, JObj}
              ,{fun cb_context:set_auth_account_id/2
               ,kz_json:get_ne_value(<<"account_id">>, JObj)
               }
              ],
    cb_context:setters(Context, Setters).
