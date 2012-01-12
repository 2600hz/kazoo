%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Simple authorization module
%%%
%%% Authenticates tokens if they are accessing the parent or
%%% child account only
%%%
%%% @end
%%% Created : 15 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_simple_authz).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(SYS_ADMIN_MODS, [<<"global_resources">>, <<"limits">>, <<"templates">>]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {ok, ok, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({binding_fired, Pid, <<"v1_resource.authorize">>
                 ,{RD, #cb_context{req_nouns=[{?WH_ACCOUNTS_DB,[]}], req_verb=Verb, auth_account_id=AuthAccountId}=Context}}, State) ->
    case is_superduper_admin(AuthAccountId) of
        true ->
            Pid ! {binding_result, true, {RD, Context}};
        false ->
            Pid ! {binding_result, Verb =:= <<"put">>, {RD, Context}}
    end,
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>, {RD, #cb_context{auth_account_id=AuthAccountId}=Context}}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  IsSysAdmin = is_superduper_admin(AuthAccountId),
                  case allowed_if_sys_admin_mod(IsSysAdmin, Context)
                      andalso account_is_descendant(IsSysAdmin, Context) of
                      true ->
                          ?LOG("authorizing the request"),
                          Pid ! {binding_result, true, {RD, Context}};
                      false ->
                          ?LOG("the request can not be authorized by this module"),
                          Pid ! {binding_result, false, {RD, Context}}
                  end
          end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
     ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec bind_to_crossbar/0 :: () -> no_return().
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.authorize">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the requested account id is a descendant or the same
%% as the account id that has been authorized to make the request.
%% @end
%%--------------------------------------------------------------------
-spec account_is_descendant/2 :: (boolean(), #cb_context{}) -> boolean().
account_is_descendant(true, _) ->
    true;
account_is_descendant(false, #cb_context{auth_account_id=undefined}) ->
    ?LOG("not authorizing, auth account id is undefined"),
    false;
account_is_descendant(false, #cb_context{auth_account_id=AuthAccountId, req_nouns=Nouns}) ->
    %% get the accounts/.... component from the URL
    case props:get_value(?WH_ACCOUNTS_DB, Nouns) of
        %% if the URL did not have the accounts noun then this module denies access
        undefined ->
            ?LOG("not authorizing, no accounts in request"),
            false;
        Params ->
            %% the request that this module process the first element of after 'accounts'
            %% in the URL has to be the requested account id
            ReqAccountId = hd(Params),
            %% we will get the requested account definition from accounts using a view
            %% with a complex key (whose alternate value is useful to use on retrieval)
            ?LOG("checking if account ~s is a descendant of ~s", [ReqAccountId, AuthAccountId]),
            ReqAccountDb = wh_util:format_account_id(ReqAccountId, encoded),
            case ReqAccountId =:= AuthAccountId orelse crossbar_util:open_doc(ReqAccountDb, ReqAccountId) of
                true -> 
                    ?LOG("authorizing, requested account is the same as the auth token account"),
                    true;
                %% if the requested account exists, the second component of the key
                %% is the parent tree, make sure the authorized account id is in that tree
                {ok, JObj} ->
                    Tree = wh_json:get_value(<<"pvt_tree">>, JObj, []),
                    case lists:member(AuthAccountId, Tree) of
                        true ->
                            ?LOG("authorizing requested account is a descendant of the auth token"),
                            true;
                        false ->
                            ?LOG("not authorizing, requested account is not a descendant of the auth token"),
                            false
                    end;
                %% anything else and they are not allowed
                {error, _} ->
                    ?LOG("not authorizing, error during lookup"),
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true the request is not for a system admin module (as defined
%% by the list above) or if it is and the account is a superduper admin.
%% @end
%%--------------------------------------------------------------------
-spec allowed_if_sys_admin_mod/2 :: (boolean(), #cb_context{}) -> boolean().
allowed_if_sys_admin_mod(IsSysAdmin, Context) ->
    case is_sys_admin_mod(Context) of
        %% if this is request is not made to a system admin module then this
        %% function doesnt deny it
        false ->
            ?LOG("authorizing, the request does not contain any system administration modules"),
            true;
        %% if this request is to a system admin module then check if the
        %% account has the 'pvt_superduper_admin'
        true when IsSysAdmin ->
            ?LOG("authorizing superduper admin access to system administration module"),
            true;
        true ->
            ?LOG("not authorizing, the request contains a system administration module"),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_superduper_admin/1 :: (undefined | ne_binary()) -> boolean().
is_superduper_admin(undefined) ->
    false;
is_superduper_admin(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case crossbar_util:open_doc(AccountDb, AccountId) of
        {ok, JObj} ->
            %% more logging was called for
            case wh_json:is_true(<<"pvt_superduper_admin">>, JObj) of
                true ->
                    ?LOG("the requestor is a superduper admin"),
                    true;
                false ->
                    ?LOG("the requestor is not a superduper admin"),
                    false
            end;
        {error, _} ->
            ?LOG("not authorizing, error during lookup"),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_sys_admin_mod/1 :: (#cb_context{}) -> boolean().
is_sys_admin_mod(#cb_context{req_nouns=Nouns}) ->
    lists:any(fun(E) -> E end
              ,[props:get_value(Mod, Nouns) =/= undefined || Mod <- ?SYS_ADMIN_MODS]).
