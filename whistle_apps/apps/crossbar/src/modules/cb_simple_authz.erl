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
-define(ACCOUNTS_DB, <<"accounts">>).
-define(VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(SYS_ADMIN_MODS, [<<"global_resources">>]).

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
                 ,{RD, #cb_context{req_nouns=[{<<"accounts">>,[]}], req_verb=Verb}=Context}}, State) ->
    %% Only sys-admins can do this?
    Pid ! {binding_result, Verb =:= <<"put">>, {RD, Context}},
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.authorize">>, {RD, Context}}, State) ->
    spawn(fun() ->
                  crossbar_util:put_reqid(Context),
                  case account_is_descendant(Context)
                      andalso allowed_if_sys_admin_mod(Context) of
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
%% Returns true if the requested account id is a decendant or the same
%% as the account id that has been authorized to make the request.
%% @end
%%--------------------------------------------------------------------
-spec account_is_descendant/1 :: (Context) -> boolean() when
      Context :: #cb_context{}.
account_is_descendant(#cb_context{auth_doc=AuthDoc, req_nouns=Nouns}) ->
    %% get the accounts/.... component from the URL
    case props:get_value(<<"accounts">>, Nouns) of
        %% if the URL did not have the accounts noun then this module denies access
        undefined ->
            false;
        Params ->
            %% the request that this module process the first element of after 'accounts'
            %% in the URL has to be the requested account id
            ReqAccountId = hd(Params),
            %% get the account id that corresponds to the auth token used
            AuthAccountId = wh_json:get_value(<<"account_id">>, AuthDoc),
            %% we will get the requested account definition from accounts using a view
            %% with a complex key (whose alternate value is useful to use on retrieval)
            ?LOG("checking if account ~s is a decendant of ~s", [ReqAccountId, AuthAccountId]),
            Opts = [{<<"startkey">>, [ReqAccountId]}
                    ,{<<"endkey">>, [ReqAccountId, ?EMPTY_JSON_OBJECT ]}],
            case couch_mgr:get_results(?ACCOUNTS_DB, ?VIEW_SUMMARY, Opts) of
                %% if the requested account doesnt exist, then decline the request
                {ok, []} ->
                    ?LOG("the requested account was not found, not authorizing"),
                    false;
                %% if the requested account exists, the second component of the key
                %% is the parent tree, make sure the authorized account id is in that tree
                {ok, [JObj]} ->
                    [_, Tree] = wh_json:get_value(<<"key">>, JObj),
                    lists:member(AuthAccountId, Tree);
                %% anything else and they are not allowed
                {error, R} ->
                    ?LOG("error during lookup ~p, not authorizing", [R]),
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
-spec allowed_if_sys_admin_mod/1 :: (Context) -> boolean() when
      Context :: #cb_context{}.
allowed_if_sys_admin_mod(#cb_context{auth_doc=AuthDoc, req_nouns=Nouns}) ->
    case is_sys_admin_mod(Nouns) of
        %% if this is request is not made to a system admin module then this
        %% function doesnt deny it
        false ->
            true;
        %% if this request is to a system admin module then check if the
        %% account has the 'pvt_superduper_admin'
        true ->
            ?LOG("the request contains a system administration module, checking if authorized"),
            AccountId = wh_json:get_value(<<"account_id">>, AuthDoc),
            case couch_mgr:open_doc(?ACCOUNTS_DB, AccountId) of
                {ok, JObj} ->
                    wh_json:is_true(<<"pvt_superduper_admin">>, JObj);
                {error, R} ->
                    ?LOG("error during lookup ~p, not authorizing", [R]),
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns true if the request contains a system admin module.
%% @end
%%--------------------------------------------------------------------
-spec is_sys_admin_mod/1 :: (Nouns) -> boolean() when
      Nouns :: proplist().
is_sys_admin_mod(Nouns) ->
    lists:any(fun(E) -> E end
              ,[props:get_value(Mod, Nouns) =/= undefined || Mod <- ?SYS_ADMIN_MODS]).
