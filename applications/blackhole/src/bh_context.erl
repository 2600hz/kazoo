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
         ,put_reqid/1
         ,response/1
         ,has_errors/1
         ,add_system_error/2, add_system_error/3
         ,add_validation_error/4
         ,validate_request_data/2, validate_request_data/3, validate_request_data/4
         ,is_context/1

         %% Getters / Setters
         ,setters/2

         ,account_id/1, set_account_id/2
         ,account_db/1, set_account_db/2
         ,account_doc/1
         ,auth_token/1, set_auth_token/2
         ,auth_doc/1, set_auth_doc/2
         ,auth_account_id/1, set_auth_account_id/2
         ,query_string/1, set_query_string/2         
         ,api_version/1, set_api_version/2

         %% Special accessors
         ,req_value/2, req_value/3
        ]).

-include("./blackhole.hrl").

-type context() :: #bh_context{}.

-spec is_context(any()) -> boolean().
is_context(#bh_context{}) -> 'true';
is_context(_) -> 'false'.

-spec req_value(context(), wh_json:key()) -> wh_json:json_term().
-spec req_value(context(), wh_json:key(), term()) -> wh_json:json_term().
req_value(#cb_context{}=Context, Key) ->
    req_value(Context, Key, 'undefined').
req_value(#cb_context{req_data=ReqData, query_json=QS}, Key, Default) ->
    wh_json:find(Key, [ReqData, QS], Default).

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

