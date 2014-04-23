%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc User Subscriptions Management Module.
%% This module implements several utility functions related to subscriptions.

-module(nksip_subscription).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([field/2, fields/2, get_id/1, get_id/2, parse_id/1, dialog_id/1]).
-export([get_subscription/1, get_all/0, get_all/2, subscription_state/1, remote_id/2]).
-export([make_id/1, find/2]).
-export_type([id/0, status/0, subscription_state/0, terminated_reason/0]).

-include("nksip.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%% SIP dialog Event ID (can be <<"event">> or <<"event;id=my_id">>)
-type id() :: 
    binary().

-type field() :: 
    subscription_id | event | parsed_event | class | answered | expires.

-type subscription_state() ::
    {active, undefined|non_neg_integer()} | {pending, undefined|non_neg_integer()}
    | {terminated, terminated_reason(), undefined|non_neg_integer()}.

-type terminated_reason() :: 
    deactivated | {probation, undefined|non_neg_integer()} | rejected |
    timeout | {giveup, undefined|non_neg_integer()} | noresource | invariant | 
    forced | {code, nksip:response_code()}.

%% All dialog event states
-type status() :: 
    init | active | pending | {terminated, terminated_reason()} | binary().



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Gets specific information from the subscription indicated by `SubscriptionSpec'. 
%% The available fields are:
%%  
%% <table border="1">
%%      <tr><th>Field</th><th>Type</th><th>Description</th></tr>
%%      <tr>
%%          <td>`app_id'</td>
%%          <td>{link nksip:app_id()}</td>
%%          <td>SipApp that created the subscription</td>
%%      </tr>
%%      <tr>
%%          <td>`status'</td>
%%          <td>{@link status()}</td>
%%          <td>Status</td>
%%      </tr>
%%      <tr>
%%          <td>`event'</td>
%%          <td>`binary()'</td>
%%          <td>Full <i>Event</i> header</td>
%%      </tr>
%%      <tr>
%%          <td>`parsed_event'</td>
%%          <td>{@link nksip:token()}</td>
%%          <td>Parsed <i>Event</i> header</td>
%%      </tr>
%%      <tr>
%%          <td>`class'</td>
%%          <td>`uac|uas'</td>
%%          <td>Class of the event, as a UAC or a UAS</td>
%%      </tr>
%%      <tr>
%%          <td>`answered'</td>
%%          <td><code>{@link nksip_lib:timestamp()}|undefined</code></td>
%%          <td>Time first NOTIFY was received</td>
%%      </tr>
%%      <tr>
%%          <td>`expires'</td>
%%          <td>integer()</td>
%%          <td>Seconds reamaining to subscription expiration</td>
%%      </tr>
%% </table>

-spec field(nksip:id()|nksip:subscription(), field()) -> 
    term() | error.

field(<<"U_", _/binary>>=Id, Field) -> 
    case fields(Id, [Field]) of
        [{_, Value}] -> Value;
        error -> error
    end;

field(#subscription{}=U, Field) ->
    case Field of
        internal_id -> 
            U#subscription.id;
        status -> 
            U#subscription.status;
        event -> 
            nksip_unparse:token(U#subscription.event);
        parsed_event -> 
            U#subscription.event;
        class -> 
            U#subscription.class;
        answered -> 
            U#subscription.answered;
        expires when is_reference(U#subscription.timer_expire) ->
            round(erlang:read_timer(U#subscription.timer_expire)/1000);
        expires ->
            undefined;
       _ -> 
            invalid_field 
    end.


%% @doc Gets a number of fields from the Subscription as described in {@link field/3}.
-spec fields(nksip:id(), [field()]) -> 
    [{atom(), term()}] | error.
    
fields(Id, Fields) when is_list(Fields) ->
    {_AppId, SubsId, _DialogId, _CallId} = parse_id(Id),
    Fun = fun(Dialog) -> 
        case find(SubsId, Dialog) of
            #subscription{} = Subs ->
                {ok, [{Field, field(Subs, Field)} || Field <- Fields]};
            not_found ->
                error
        end
    end,
    case nksip_call_router:apply_dialog(dialog_id(Id), Fun) of
        {ok, Values} -> Values;
        _ -> error
    end.


%% @doc Get the subscripion's id from a request or response.
-spec get_id(nksip:id()|nksip:request()|nksip:response()) ->
    nksip:id().

get_id(<<"U_", _/binary>>=Id) ->
    Id;

get_id(<<Class, $_, _/binary>>=Id) when Class==$R; Class==$S ->
    Fun = fun(#sipmsg{}=SipMsg) -> {ok, get_id(SipMsg)} end,
    case nksip_call_router:apply_sipmsg(Id, Fun) of
        {ok, Id1} -> Id1;
        {error, _} -> <<>>
    end;

get_id(#sipmsg{app_id=AppId, dialog_id=DialogId, call_id=CallId}=SipMsg) ->
    SubsId = make_id(SipMsg),
    App = atom_to_binary(AppId, latin1), 
    <<$U, $_, SubsId/binary, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>.


%% @private
get_id(#subscription{id=SubsId}, #dialog{app_id=AppId, id=DialogId, call_id=CallId}) ->
    App = atom_to_binary(AppId, latin1), 
    <<$U, $_, SubsId/binary, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>.

% %% @private
% -spec get_id(nksip:token()|undefined, nksip_dialog:id()) ->
%     nksip:id().

% get_id({Type, Opts}, <<"D_", _/binary>>=Id) when is_binary(Type), is_list(Opts) ->
%     {AppId, DialogId, CallId} = nksip_dialog:parse_id(Id),
%     App = atom_to_binary(AppId, latin1), 
%     EventId = nksip_lib:get_binary(<<"id">>, Opts, <<>>),
%     <<$U, $_, App/binary, Type/binary, $_, EventId/binary, $_, DialogId/binary,
%       CallId/binary>>;

% get_id({Type, Opts}, #dialog{}=Dialog) when is_binary(Type), is_list(Opts) ->
%     get_id({Type, Opts}, nksip_dialog:get_id(Dialog));

% get_id(_, _) ->
%     <<>>.


%% @private
-spec parse_id(nksip:id()) ->
    {nksip:app_id(), id(), nksip_dialog:id(), nksip:call_id()}.

parse_id(<<"U_", SubsId:6/binary, $_, DialogId:6/binary, $_, App:7/binary, 
         $_, CallId/binary>>) ->
    AppId = binary_to_existing_atom(App, latin1),
    {AppId, SubsId, DialogId, CallId}. 


%% @private
-spec dialog_id(nksip:id()) ->
    nksip_dialog:id().

dialog_id(Id) ->
    {AppId, _, DialogId, CallId} = parse_id(Id),
    App = atom_to_binary(AppId, latin1), 
    <<$D, $_, DialogId/binary, $_, App/binary, $_, CallId/binary>>.


%% @doc Gets a full subscription record.
-spec get_subscription(nksip:id()) ->
    nksip:dialog() | error.

get_subscription(Id) ->
    {_AppId, SubsId, _DialogId, _CallId} = parse_id(Id),
    DId = dialog_id(Id),
    Fun = fun(#dialog{}=Dialog) ->
        case find(SubsId, Dialog) of
            #subscription{} = Event -> {ok, Event};
            _ -> error
        end
    end,
    case nksip_call_router:apply_dialog(DId, Fun) of
        {ok, Event} -> Event;
        _ -> error
    end.


%% @private
-spec make_id(nksip:request()) ->
    id().

make_id(#sipmsg{class={req, 'REFER'}, cseq={CSeqNum, 'REFER'}}) ->
    nksip_lib:hash({<<"refer">>, nksip_lib:to_binary(CSeqNum)});

make_id(#sipmsg{class={resp, _, _}, cseq={CSeqNum, 'REFER'}}) ->
    nksip_lib:hash({<<"refer">>, nksip_lib:to_binary(CSeqNum)});

make_id(#sipmsg{event={Event, Opts}}) ->
    Id = nksip_lib:get_value(<<"id">>, Opts),
    nksip_lib:hash({Event, Id}).



    % nksip_lib:hash({Event, Id}).


%% @doc Gets all started subscription ids.
-spec get_all() ->
    [nksip:id()].

get_all() ->
    lists:flatten([
        nksip_dialog:field(Id, subscriptions) 
        || Id <- nksip_call_router:get_all_dialogs()
    ]).


%% @doc Finds all existing subscriptions having a `Call-ID'.
-spec get_all(nksip:app_id(), nksip:call_id()) ->
    [nksip:id()].

get_all(AppId, CallId) ->
    lists:flatten([
        nksip_dialog:field(Id, subscriptions)
        || Id <- nksip_call_router:get_all_dialogs(AppId, CallId)
    ]).


%% ===================================================================
%% Private
%% ===================================================================


% %% @private
% -spec id(nksip:request()|nksip:response()) ->
%     nksip:id().

% id(#sipmsg{cseq={CSeq, 'REFER'}, dialog_id=DialogId}) ->
%     Event = {<<"refer">>, [{<<"id">>, nksip_lib:to_binary(CSeq)}]},
%     get_id(Event, DialogId);

% id(#sipmsg{event = Event, dialog_id = <<"D_", _/binary>>=DialogId}) ->
%     get_id(Event, DialogId).



%% @private Finds a event.
-spec find(id()|nksip:request()|nksip:response(), nksip:dialog()) ->
    nksip:subscription() | not_found.

find(Id, #dialog{subscriptions=Subs}) when is_binary(Id) ->
    do_find(Id, Subs);

find(#sipmsg{}=Req, #dialog{subscriptions=Subs}) ->
    do_find(make_id(Req), Subs).

%% @private 
do_find(_, []) -> not_found;
do_find(Id, [#subscription{id=Id}=Subs|_]) -> Subs;
do_find(Id, [_|Rest]) -> do_find(Id, Rest).



%% @private Hack to find the UAS subscription from the UAC and the opposite way
remote_id(Id, App) ->
    {_AppId0, SubsId, _DialogId, CallId} = parse_id(Id),
    RemoteId = nksip_dialog:remote_id(dialog_id(Id), App),
    {AppId1, RemDialogId, CallId} = nksip_dialog:parse_id(RemoteId),
    App1 = atom_to_binary(AppId1, latin1),
    <<$U, $_, SubsId/binary, $_, RemDialogId/binary, $_, App1/binary, $_, CallId/binary>>.


%% @private
-spec subscription_state(nksip:request()) ->
    subscription_state() | invalid.

subscription_state(#sipmsg{}=SipMsg) ->
    try
        case nksip_sipmsg:header(SipMsg, <<"subscription-state">>, tokens) of
            [{Name, Opts}] -> ok;
            _ -> Name = Opts = throw(invalid)
        end,
        case Name of
            <<"active">> -> 
                case nksip_lib:get_integer(<<"expires">>, Opts, -1) of
                    -1 -> Expires = undefined;
                    Expires when is_integer(Expires), Expires>=0 -> ok;
                    _ -> Expires = throw(invalid)
                end,
                 {active, Expires};
            <<"pending">> -> 
                case nksip_lib:get_integer(<<"expires">>, Opts, -1) of
                    -1 -> Expires = undefined;
                    Expires when is_integer(Expires), Expires>=0 -> ok;
                    _ -> Expires = throw(invalid)
                end,
                {pending, Expires};
            <<"terminated">> ->
                case nksip_lib:get_integer(<<"retry-after">>, Opts, -1) of
                    -1 -> Retry = undefined;
                    Retry when is_integer(Retry), Retry>=0 -> ok;
                    _ -> Retry = throw(invalid)
                end,
                case nksip_lib:get_value(<<"reason">>, Opts) of
                    undefined -> 
                        {terminated, undefined, undefined};
                    Reason0 ->
                        case catch binary_to_existing_atom(Reason0, latin1) of
                            {'EXIT', _} -> {terminated, undefined, undefined};
                            probation -> {terminated, probation, Retry};
                            giveup -> {terminated, giveup, Retry};
                            Reason -> {terminated, Reason, undefined}
                        end
                end;
            _ ->
                throw(invalid)
        end
    catch
        throw:invalid -> invalid
    end.

% %% @private
% find_sep([$_|Rest], Acc) -> {list_to_binary(lists:reverse(Acc)), Rest};
% find_sep([], Acc) -> {list_to_binary(lists:reverse(Acc)), []};
% find_sep([Ch|Rest], Acc) -> find_sep(Rest, [Ch|Acc]).


