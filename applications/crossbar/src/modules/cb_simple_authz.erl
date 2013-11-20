%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).
-define(VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(SYS_ADMIN_MODS, [<<"global_resources">>
                         ,<<"templates">>
                         ,<<"rates">>
                         ,<<"acls">>
                         ,<<"global_provisioner_templates">>
                        ]).

%%%===================================================================
%%% API
%%%===================================================================
init() -> crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize').

-spec authorize(cb_context:context()) -> boolean().
authorize(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, []}]
                      ,req_verb=Verb
                     }=Context) ->
    case cb_modules_util:is_superduper_admin(Context) of
        'true' -> 'true';
        'false' -> Verb =:= ?HTTP_PUT
    end;
authorize(#cb_context{req_nouns=[{<<"global_provisioner_templates">>,_}|_]
                      ,req_verb = ?HTTP_GET
                     }) ->
    'true';
authorize(#cb_context{auth_account_id=AuthAccountId}=Context) ->
    IsSysAdmin = cb_modules_util:is_superduper_admin(AuthAccountId),
    case allowed_if_sys_admin_mod(IsSysAdmin, Context)
        andalso account_is_descendant(IsSysAdmin, Context) of
        'true' ->
            lager:debug("authorizing the request"),
            'true';
        'false' ->
            lager:debug("the request can not be authorized by this module"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the requested account id is a descendant or the same
%% as the account id that has been authorized to make the request.
%% @end
%%--------------------------------------------------------------------
-spec account_is_descendant(boolean(), cb_context:context()) -> boolean().
account_is_descendant('true', _) -> 'true';
account_is_descendant('false', #cb_context{auth_account_id='undefined'}) ->
    lager:debug("not authorizing, auth account id is undefined"),
    'false';
account_is_descendant('false', #cb_context{auth_account_id=AuthAccountId
                                           ,req_nouns=Nouns
                                          }) ->
    %% get the accounts/.... component from the URL
    case props:get_value(?WH_ACCOUNTS_DB, Nouns) of
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
            ReqAccountDb = wh_util:format_account_id(ReqAccountId, 'encoded'),
            case ReqAccountId =:= AuthAccountId orelse couch_mgr:open_cache_doc(ReqAccountDb, ReqAccountId) of
                'true' ->
                    lager:debug("authorizing, requested account is the same as the auth token account"),
                    'true';
                %% if the requested account exists, the second component of the key
                %% is the parent tree, make sure the authorized account id is in that tree
                {'ok', JObj} ->
                    Tree = wh_json:get_value(<<"pvt_tree">>, JObj, []),
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
is_sys_admin_mod(#cb_context{req_nouns=Nouns}) ->
    lists:any(fun(E) -> E end
              ,[props:get_value(Mod, Nouns) =/= 'undefined' || Mod <- ?SYS_ADMIN_MODS]
             ).
