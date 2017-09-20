%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Simple authorization module
%%%
%%% Authenticates tokens if they are accessing the parent or
%%% child account only
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_simple_authz).

-export([init/0
        ,authorize/1
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).
-define(VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(SYS_ADMIN_MODS, [<<"acls">>
                        ,<<"global_provisioner_templates">>
                        ,<<"global_resources">>
                        ,<<"ips">>
                        ,<<"rates">>
                        ,<<"sup">>
                        ,<<"templates">>
                        ]).

%% Endpoints performing their own auth
-define(IGNORE_MODS, []).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    'ok'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    cb_context:put_reqid(Context),
    authorize(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

authorize(Context, Verb, [{?KZ_ACCOUNTS_DB, []}]) ->
    cb_context:is_superduper_admin(Context)
        orelse Verb =:= ?HTTP_PUT;
authorize(_Context, ?HTTP_GET, [{<<"global_provisioner_templates">>,_}|_]) ->
    'true';
authorize(Context, Verb, _Nouns) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSysAdmin = cb_context:is_superduper_admin(AuthAccountId),
    case (not should_ignore(Context)
          andalso (allowed_if_sys_admin_mod(IsSysAdmin, Context)
                   andalso account_is_descendant(IsSysAdmin, Context)
                  )
         )
        orelse (Verb =:= ?HTTP_GET
                andalso cb_context:magic_pathed(Context)
               )
    of
        'true' ->
            lager:debug("authorizing the request"),
            'true';
        'false' ->
            lager:debug("the request can not be authorized by this module"),
            'false'
    end.

-spec should_ignore(cb_context:context()) -> boolean().
should_ignore(Context) ->
    lists:any(fun({Noun, _}) ->
                      lists:member(Noun, ?IGNORE_MODS)
              end, cb_context:req_nouns(Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the requested account id is a descendant or the same
%% as the account id that has been authorized to make the request.
%% @end
%%--------------------------------------------------------------------
-spec account_is_descendant(boolean(), cb_context:context()) -> boolean().
account_is_descendant('true', _Context) -> 'true';
account_is_descendant('false', Context) ->
    account_is_descendant('false', Context, cb_context:auth_account_id(Context)).

account_is_descendant('false', _Context, 'undefined') ->
    lager:debug("not authorizing, auth account id is undefined"),
    'false';
account_is_descendant('false', Context, AuthAccountId) ->
    Nouns = cb_context:req_nouns(Context),
    %% get the accounts/.... component from the URL
    case props:get_value(?KZ_ACCOUNTS_DB, Nouns) of
        %% if the URL did not have the accounts noun then this module denies access
        'undefined' ->
            lager:debug("not authorizing, no accounts in request"),
            'false';
        Params ->
            %% the request that this module process the first element of after 'accounts'
            %% in the URL has to be the requested account id
            ReqAccountId = hd(Params),
            %% we will get the requested account definition from accounts using a view
            %% with a complex key (whose alternate value is useful to use on retrieval)
            lager:debug("checking if account ~s is a descendant of ~s", [ReqAccountId, AuthAccountId]),
            case ReqAccountId =:= AuthAccountId
                orelse kz_account:fetch(ReqAccountId)
            of
                'true' ->
                    lager:debug("authorizing, requested account is the same as the auth token account"),
                    'true';
                %% if the requested account exists, the second component of the key
                %% is the parent tree, make sure the authorized account id is in that tree
                {'ok', JObj} ->
                    Tree = kz_account:tree(JObj),
                    case lists:member(AuthAccountId, Tree) of
                        'true' ->
                            lager:debug("authorizing requested account is a descendant of the auth token"),
                            'true';
                        'false' ->
                            lager:debug("not authorizing, requested account is not a descendant of the auth token"),
                            'false'
                    end;
                %% anything else and they are not allowed
                {'error', _E} ->
                    lager:debug("not authorizing, error during lookup: ~p", [_E]),
                    'false'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true the request is not for a system admin module (as defined
%% by the list above) or if it is and the account is a superduper admin.
%% @end
%%--------------------------------------------------------------------
-spec allowed_if_sys_admin_mod(boolean(), cb_context:context()) -> boolean().
allowed_if_sys_admin_mod(IsSysAdmin, Context) ->
    case is_sys_admin_mod(Context) of
        %% if this is request is not made to a system admin module then this
        %% function doesnt deny it
        'false' ->
            lager:debug("authorizing, the request does not contain any system administration modules"),
            'true';
        %% if this request is to a system admin module then check if the
        %% account has the 'pvt_superduper_admin'
        'true' when IsSysAdmin ->
            lager:debug("authorizing superduper admin access to system administration module"),
            'true';
        'true' ->
            lager:debug("not authorizing, the request contains a system administration module"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_sys_admin_mod(cb_context:context()) -> boolean().
is_sys_admin_mod(Context) ->
    Nouns = cb_context:req_nouns(Context),
    lists:any(fun kz_term:identity/1
             ,[props:get_value(Mod, Nouns) =/= 'undefined' || Mod <- ?SYS_ADMIN_MODS]
             ).
