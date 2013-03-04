%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_request).

-behaviour(gen_listener).

%% API
-export([start_link/2
         ,relay_event/2
         ,receive_fax/2
        ]).

%% gen_listener callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("fax.hrl").

-record(state, {
          call :: whapps_call:call()
         ,action = 'receive' :: 'receive' | 'send'
         ,handler :: {pid(), reference()}
         ,owner_id :: ne_binary()
         }).

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
start_link(Call, JObj) ->
    gen_listener:start_link(?MODULE
                          ,[{bindings, [{call, [{callid, whapps_call:call_id(Call)}]}
                                        ,{self, []}
                                       ]}
                            ,{responders, [{{?MODULE, relay_event}, {<<"*">>, <<"*">>}}]}
                           ]
                          ,[Call, JObj]).

-spec relay_event(wh_json:object(), wh_proplist()) -> any().
relay_event(JObj, Props) ->
    case props:get_value(handler, Props) of
        undefined -> ignore;
        {Pid, _} -> whapps_call_command:relay_event(Pid, JObj)
    end.

%%%===================================================================
%%% gen_listener callbacks
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
init([Call, JObj]) ->
    put(callid, whapps_call:call_id(Call)),

    lager:debug("fax request starting"),
    gen_listener:cast(self(), start_action),

    {ok, #state{
       call = Call
       ,action = get_action(JObj)
       ,owner_id = wh_json:get_value(<<"Owner-ID">>, JObj)
      }}.

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
handle_cast(start_action, #state{call=Call, action='receive', owner_id=OwnerId}=State) ->
    {_Pid, _Ref}=Recv = spawn_monitor(?MODULE, receive_fax, [Call, OwnerId]),
    lager:debug("receiving a fax in ~p(~p)", [_Pid, _Ref]),
    {noreply, State#state{handler=Recv}};
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
handle_info({'DOWN',Ref,process,Pid,normal}, #state{handler={Pid, Ref}}=State) ->
    lager:debug("handler ~p down normally, request is done", [Pid]),
    {stop, normal, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

-spec handle_event(wh_json:object(), #state{}) -> {'reply', wh_proplist()}.
handle_event(_JObj, #state{handler=Handler}) ->
    {reply, [{handler,Handler}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{call=Call}) ->
    whapps_call_command:hangup(Call),
    lager:debug("fax request terminating: ~p", [_Reason]).

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
-spec get_action(wh_json:object()) -> 'receive' | 'transmit'.
get_action(JObj) ->
    case wh_json:get_value(<<"Action">>, JObj) of
        <<"transmit">> -> 'transmit';
        _ -> 'receive'
    end.

-spec receive_fax(whapps_call:call(), api_binary()) -> any().
receive_fax(Call, OwnerId) ->
    put(callid, whapps_call:call_id(Call)),

    whapps_call_command:answer(Call),
    case whapps_call_command:b_receive_fax(Call) of
        {ok, RecvJObj} ->
            lager:debug("rxfax resp: ~p", [RecvJObj]),
            whapps_call_command:hangup(Call),

            maybe_store_fax(Call, OwnerId, RecvJObj);
        {failed, JObj} ->
            lager:debug("rxfax failed: ~p", [JObj]);
        {error, channel_hungup} ->
            lager:debug("rxfax hungup prematurely");
        _Resp ->
            lager:debug("rxfax unhandled: ~p", [_Resp])
    end.

maybe_store_fax(Call, OwnerId, RecvJObj) ->
    %% store Fax in DB
    case store_fax(Call, OwnerId, RecvJObj) of
        {ok, FaxId} ->
            lager:debug("storing fax successfully into ~s", [FaxId]),

            wapi_notifications:publish_fax(
              props:filter_empty(
                fax_fields(RecvJObj) ++
                    [{<<"From-User">>, whapps_call:from_user(Call)}
                     ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
                     ,{<<"To-User">>, whapps_call:to_user(Call)}
                     ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
                     ,{<<"Account-DB">>, whapps_call:account_db(Call)}
                     ,{<<"Fax-ID">>, FaxId}
                     ,{<<"Owner-ID">>, OwnerId}
                     ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
                     ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
                     ,{<<"Call-ID">>, whapps_call:call_id(Call)}
                     ,{<<"Fax-Timestamp">>, wh_util:current_tstamp()}
                     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                    ]));
        _E ->
            lager:debug("store fax other resp: ~p", [_E])
    end.

-spec fax_fields(wh_json:object()) -> wh_proplist().
fax_fields(JObj) ->
    [{K,V} || {<<"Fax-", _/binary>> = K, V} <- wh_json:to_proplist(JObj)].

store_fax(Call, OwnerId, JObj) ->
    FaxFile = tmp_file(),
    FaxDocId = create_fax_doc(Call, OwnerId, JObj),
    FaxUrl = attachment_url(Call, FaxFile, FaxDocId),

    lager:debug("storing fax ~s to ~s", [FaxFile, FaxUrl]),

    case whapps_call_command:b_store_fax(FaxUrl, Call) of
        {ok, _JObj} -> {ok, FaxDocId};
        E -> lager:debug("store_fax error: ~p", [E]), E
    end.

create_fax_doc(Call, OwnerId, JObj) ->
    AccountDb = whapps_call:account_db(Call),

    TStamp = wh_util:current_tstamp(),
    {{Y,M,D}, {H,I,S}} = calendar:gregorian_seconds_to_datetime(TStamp),

    Name = list_to_binary(["fax message received at "
                           ,wh_util:to_binary(Y), "-", wh_util:to_binary(M), "-", wh_util:to_binary(D)
                           ," " , wh_util:to_binary(H), ":", wh_util:to_binary(I), ":", wh_util:to_binary(S)
                           ," UTC"
                          ]),

    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"fax document received">>}
             ,{<<"source_type">>, <<"incoming_fax">>}
             ,{<<"timestamp">>, wh_json:get_value(<<"Timestamp">>, JObj)}
             ,{<<"owner_id">>, OwnerId}
             ,{<<"media_type">>, <<"tiff">>}
             ,{<<"call_id">>, whapps_call:call_id(Call)}
             | fax_properties(JObj)
            ],

    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props)
                                       ,AccountDb
                                       ,[{type, <<"private_media">>}]
                                      ),

    {ok, Doc1} = couch_mgr:save_doc(AccountDb, Doc),
    wh_json:get_value(<<"_id">>, Doc1).

-spec fax_properties(wh_json:object()) -> wh_proplist().
fax_properties(JObj) ->
    [{wh_json:normalize_key(K), V} || {<<"Fax-", K/binary>>, V} <- wh_json:to_proplist(JObj)].

attachment_url(Call, File, FaxDocId) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_doc(AccountDb, FaxDocId) of
            {ok, JObj} ->
                case wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())) of
                    [] -> ok;
                    Existing -> [couch_mgr:delete_attachment(AccountDb, FaxDocId, Attach) || Attach <- Existing]
                end;
            {error, _} -> ok
        end,
    Rev = case couch_mgr:lookup_doc_rev(AccountDb, FaxDocId) of
              {ok, R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,
    list_to_binary([wh_couch_connections:get_url(), AccountDb, "/", FaxDocId, "/", File, Rev]).


-spec tmp_file() -> ne_binary().
tmp_file() ->
     <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".tiff">>.
