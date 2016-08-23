%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% Helpers for manipulating the #bh_context{} record
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_context).

-export([new/0, new/1
        ,to_json/1
        ,is_context/1
        ]).

-export([setters/2
        ,auth_token/1, set_auth_token/2
        ,auth_account_id/1, set_auth_account_id/2
        ,bindings/1, set_bindings/2
        ,add_binding/2, remove_binding/2, is_bound/2
        ,websocket_pid/1, set_websocket_pid/2
        ,timestamp/1, set_timestamp/2
        ,req_id/1
        ,get_pid/1
        ]).

-include("blackhole.hrl").

-type context() :: #bh_context{}.
-type setter_fun_1() :: fun((context()) -> context()).
-type setter_fun_2() :: fun((context(), any()) -> context()).
-type setter_fun_3() :: fun((context(), any(), any()) -> context()).
-type setter_kv() :: setter_fun_1() |
                     {setter_fun_2(), any()} |
                     {setter_fun_3(), any(), any()}.
-type setters() :: [setter_kv()].

-export_type([context/0]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> context().
new()->
    Setters = [
               fun put_reqid/1
              ,{fun set_timestamp/2, kz_util:current_tstamp()}
              ],
    setters(#bh_context{}, Setters).

new(Pid) ->
    set_websocket_pid(new(), Pid).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(context()) -> kz_json:object().
to_json(Context) ->
    kz_json:from_list(
      props:filter_undefined([{<<"auth_token">>, auth_token(Context)}
                             ,{<<"auth_account_id">>, auth_account_id(Context)}
                             ,{<<"bindings">>, bindings(Context)}
                             ,{<<"timestamp">>, timestamp(Context)}
                             ,{<<"req_id">>, req_id(Context)}
                             ])
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_context(any()) -> boolean().
is_context(#bh_context{}) -> 'true';
is_context(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setters(context(), setters()) -> context().
setters(Context, Setters) ->
    lists:foldl(fun setters_fold/2, Context, Setters).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec auth_token(context()) -> api_binary().
auth_token(#bh_context{auth_token=AuthToken}) ->
    AuthToken.

-spec set_auth_token(context(), ne_binary()) -> context().
set_auth_token(#bh_context{}=Context, AuthToken) ->
    Context#bh_context{auth_token=AuthToken}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec auth_account_id(context()) -> api_binary().
auth_account_id(#bh_context{auth_account_id=AuthBy}) ->
    AuthBy.

-spec set_auth_account_id(context(), ne_binary()) -> context().
set_auth_account_id(#bh_context{}=Context, AuthBy) ->
    Context#bh_context{auth_account_id=AuthBy}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bindings(context()) -> ne_binaries().
bindings(#bh_context{bindings=Bds}) ->
    Bds.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_bindings(context(), ne_binaries()) -> context().
set_bindings(Context, Bindings) ->
    Context#bh_context{bindings=Bindings}.

-spec add_binding(context(), ne_binary()) -> context().
add_binding(#bh_context{bindings=Bds}=Context, Binding) ->
    Context#bh_context{bindings=[Binding|Bds]}.

-spec remove_binding(context(), ne_binary()) -> context().
remove_binding(#bh_context{bindings=Bds}=Context, Binding) ->
    Context#bh_context{bindings=lists:delete(Binding, Bds)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_bound(context(), ne_binary()) -> boolean().
is_bound(#bh_context{bindings=Bds}, Binding) ->
    lists:member(Binding, Bds).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec websocket_pid(context()) -> api_binary().
websocket_pid(#bh_context{websocket_pid=SocketPid}) -> SocketPid.

-spec set_websocket_pid(context(), pid()) -> context().
set_websocket_pid(#bh_context{}=Context, SocketPid) ->
    Context#bh_context{websocket_pid=SocketPid}.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec timestamp(context()) -> gregorian_seconds().
timestamp(#bh_context{timestamp=Timestamp}) ->
    Timestamp.

-spec set_timestamp(context(), gregorian_seconds()) -> context().
set_timestamp(#bh_context{}=Context, Timestamp) ->
    Context#bh_context{timestamp=Timestamp}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec req_id(context()) -> ne_binary().
req_id(#bh_context{req_id=Id}) ->
    Id.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec put_reqid(context()) -> context().
put_reqid(#bh_context{req_id = ReqId} = Context) ->
    kz_util:put_callid(ReqId),
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setters_fold(setter_kv(), context()) -> context().
setters_fold({F, V}, C) -> F(C, V);
setters_fold({F, K, V}, C) -> F(C, K, V);
setters_fold(F, C) when is_function(F, 1) -> F(C).

get_pid(#bh_call{ws_pid=WsPid}) -> WsPid;
get_pid(_) -> undefined.
