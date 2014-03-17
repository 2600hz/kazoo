%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%% Helpers for manipulating the #bh_context{} record
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_context).
-export([store/3
         ,fetch/2, fetch/3
         ,req_id/1, put_reqid/1
         ,is_context/1
         ,event_name_tokens/1

         ,setters/2

         ,account_id/1, set_account_id/2
         ,client_ip/1, set_client_ip/2
         ,account_db/1, set_account_db/2
         ,account_doc/1
         ,auth_token/1, set_auth_token/2
         ,auth_doc/1, set_auth_doc/2
         ,auth_account_id/1, set_auth_account_id/2
         ,event_name/1, set_event_name/2
         ,event_data/1, set_event_data/2
         ,api_version/1, set_api_version/2
         ,websocket_session_id/1, set_websocket_session_id/2
         ,websocket_pid/1, set_websocket_pid/2
        ]).

-include("blackhole.hrl").

-type context() :: #bh_context{}.
-type setter_fun_2() :: fun((context(), term()) -> context()).
-type setter_fun_3() :: fun((context(), term(), term()) -> context()).
-export_type([context/0
              ,setter_fun_2/0
              ,setter_fun_3/0
             ]).

-type setter_kv() :: {setter_fun_2(), term()} |
                     {setter_fun_3(), term(), term()}.
-type setters() :: [setter_kv(),...] | [].

-spec is_context(any()) -> boolean().
is_context(#bh_context{}) -> 'true';
is_context(_) -> 'false'.

%% Accessors
-spec account_doc(context()) -> wh_json:object().

account_id(#bh_context{account_id=AcctId}) -> AcctId.
account_db(#bh_context{db_name=AcctDb}) -> AcctDb.
account_doc(#bh_context{}=Context) ->
    AccountId = account_id(Context),
    {'ok', Doc} =
        couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), AccountId),
    Doc.
auth_token(#bh_context{auth_token=AuthToken}) -> AuthToken.
auth_doc(#bh_context{auth_doc=AuthDoc}) -> AuthDoc.
auth_account_id(#bh_context{auth_account_id=AuthBy}) -> AuthBy.
req_id(#bh_context{req_id=ReqId}) -> ReqId.
event_name(#bh_context{event_name=EventName}) -> EventName.
event_data(#bh_context{event_data=EventData}) -> EventData.
api_version(#bh_context{api_version=ApiVersion}) -> ApiVersion.
client_ip(#bh_context{client_ip=ClientIP}) -> ClientIP.
websocket_session_id(#bh_context{websocket_session_id=SessionId}) -> SessionId.
websocket_pid(#bh_context{websocket_pid=SocketPid}) -> SocketPid.

-spec event_name_tokens(context()) -> ne_binaries().
event_name_tokens(#bh_context{event_name=EventName}) ->
    [Token || Token <- binary:split(EventName, <<".">>, ['global', 'trim'])].

%% Setters
-spec setters(context(), setters()) -> context().
setters(#bh_context{}=Context, []) ->
    Context;
setters(#bh_context{}=Context, [_|_]=Setters) ->
    lists:foldl(fun setters_fold/2, Context, Setters).

-spec setters_fold(setter_kv(), context()) -> context().
setters_fold({F, V}, C) -> F(C, V);
setters_fold({F, K, V}, C) -> F(C, K, V).

-spec set_account_id(context(), ne_binary()) -> context().
-spec set_account_db(context(), ne_binary()) -> context().
-spec set_auth_token(context(), ne_binary()) -> context().
-spec set_auth_doc(context(), wh_json:object()) -> context().
-spec set_auth_account_id(context(), ne_binary()) -> context().
-spec set_event_name(context(), ne_binary()) -> context().
-spec set_event_data(context(), wh_json:object()) -> context().
-spec set_api_version(context(), ne_binary()) -> context().
-spec set_websocket_session_id(context(), ne_binary()) -> context().
-spec set_websocket_pid(context(), pid()) -> context().
set_account_id(#bh_context{}=Context, AcctId) -> Context#bh_context{account_id=AcctId}.
set_account_db(#bh_context{}=Context, AcctDb) -> Context#bh_context{db_name=AcctDb}.
set_auth_token(#bh_context{}=Context, AuthToken) -> Context#bh_context{auth_token=AuthToken}.
set_auth_doc(#bh_context{}=Context, AuthDoc) -> Context#bh_context{auth_doc=AuthDoc}.
set_auth_account_id(#bh_context{}=Context, AuthBy) -> Context#bh_context{auth_account_id=AuthBy}.
set_event_name(#bh_context{}=Context, EventName) -> Context#bh_context{event_name=EventName}.
set_event_data(#bh_context{}=Context, EventData) -> Context#bh_context{event_data=EventData}.
set_api_version(#bh_context{}=Context, ApiVersion) -> Context#bh_context{api_version=ApiVersion}.
set_client_ip(#bh_context{}=Context, ClientIP) -> Context#bh_context{client_ip=ClientIP}.
set_websocket_session_id(#bh_context{}=Context, SessionId) -> 
    Context#bh_context{websocket_session_id=SessionId}.
set_websocket_pid(#bh_context{}=Context, SocketPid) ->
    Context#bh_context{websocket_pid=SocketPid}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets a value in the crossbar context for later retrieval during
%% this request.
%% @end
%%--------------------------------------------------------------------
-spec store(context(), term(), term()) -> context().
store(#bh_context{storage=Storage}=Context, Key, Data) ->
    Context#bh_context{storage=[{Key, Data} | props:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec fetch(context(), term()) -> term().
-spec fetch(context(), term(), term()) -> term().

fetch(#bh_context{}=Context, Key) ->
    fetch(Context, Key, 'undefined').

fetch(#bh_context{storage=Storage}, Key, Default) ->
    case props:get_value(Key, Storage) of
        'undefined' -> Default;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid(context()) -> api_binary().
put_reqid(#bh_context{req_id=ReqId}) ->
    put('callid', ReqId).
