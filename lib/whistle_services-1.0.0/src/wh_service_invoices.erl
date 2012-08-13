%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_invoices).

-behaviour(gen_server).

-export([start_link/0]).
-export([run/0]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include_lib("whistle_services/src/whistle_services.hrl").

-record(state, {}).

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
    gen_server:start_link(?MODULE, [], []).

run() ->
    maybe_sync_service().

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
init([]) ->
    {ok, #state{}}.

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
    {reply, {error, not_implemented}, State}.

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
    lager:debug("whistle service invoices terminating: ~p", [_Reason]).

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
-spec maybe_sync_service/0 :: () -> wh_json:json_objects().
maybe_sync_service() ->
    ViewOptions = [{limit, 1}
                   ,include_docs
                  ],
    case couch_mgr:get_results(?WH_SERVICES_DB, <<"services/dirty">>, ViewOptions) of
        {error, _}=E -> E;
        {ok, [JObj]} -> bump_modified(wh_json:get_value(<<"doc">>, JObj));
        {ok, _} -> {error, no_dirty_services}
    end.

-spec bump_modified/1 :: (wh_json:json_object()) -> _.
bump_modified(JObj) ->
    AccountId = wh_json:get_value(<<"pvt_account_id">>, JObj),
    UpdatedJObj = wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj),
    case couch_mgr:save_doc(?WH_SERVICES_DB, UpdatedJObj) of
        {error, _}=E ->
            %% If we conflict or cant save the doc with a new modified timestamp
            %% then another process is probably handling it, move on
            E;
        {ok, NewJObj} ->
            %% If we can change the timestamp then (since the view requires the
            %% modified time to be x mins in the past) we have gain exclusive
            %% control for x mins.... good luck!
            AccountId = wh_json:get_value(<<"pvt_account_id">>, NewJObj),
            maybe_follow_billing_id(AccountId, NewJObj)
    end.

-spec maybe_follow_billing_id/2 :: (ne_binary(), wh_json:json_object()) -> wh_std_return().
maybe_follow_billing_id(AccountId, JObj) ->
    case get_billing_id(AccountId, JObj) of
        AccountId -> sync_services(AccountId, JObj);
        BillingId -> follow_billing_id(AccountId, BillingId)
    end.

-spec follow_billing_id/2 :: (ne_binary(), ne_binary()) -> wh_std_return().
follow_billing_id(AccountId, BillingId) ->
    case mark_dirty(BillingId) of
        {ok, _} -> mark_clean(AccountId);
        {error, _R}=E -> 
            lager:debug("unable to mark billing services ~s dirty: ~p", [BillingId, _R]),
            E
    end.

-spec sync_services/2 :: (ne_binary(), wh_json:json_object()) -> wh_std_return().
sync_services(AccountId, ServiceJObj) ->
    ServiceItems = wh_service_plans:create_items(ServiceJObj),
    %% TODO: support other bookkeepers...
    try wh_bookkeeper_braintree:sync(ServiceItems, AccountId) of
        _ -> mark_clean(AccountId)
    catch
        _E:R ->
            lager:debug("unable to sync services(~p): ~p", [_E, R]),
            {error, R}
    end.

-spec get_billing_id/2 :: (ne_binary(), wh_json:json_object()) -> ne_binary().
get_billing_id(AccountId, JObj) ->
    case wh_json:is_true(<<"pvt_reseller">>, JObj) of
        true -> AccountId;
        false -> wh_json:get_ne_value(<<"billing_id">>, JObj, AccountId)
    end.

-spec mark_dirty/1 :: (ne_binary()) -> wh_std_return().
mark_dirty(AccountId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {error, _}=E -> E;
        {ok, JObj} -> couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"pvt_dirty">>, true, JObj))
    end.

-spec mark_clean/1 :: (ne_binary()) -> wh_std_return().
mark_clean(AccountId) ->
    case couch_mgr:open_doc(?WH_SERVICES_DB, AccountId) of
        {error, _}=E -> E;
        {ok, JObj} -> couch_mgr:save_doc(?WH_SERVICES_DB, wh_json:set_value(<<"pvt_dirty">>, false, JObj))
    end.
