%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% This module contains predicates (boolean value - returning functions)
%% That are meant to be used primarly with escalus:assert function

-module(escalus_pred).

-export([is_message/1,
         is_chat_message/1,
         is_chat_message/2,
         is_forwarded_received_message/4,
         is_forwarded_sent_message/4,
         is_groupchat_message/1,
         is_groupchat_message/2,
         is_headline_message/3,
         is_0184_request/1,
         is_0184_receipt/2,
         is_0184_receipt/3,
         is_iq/1,
         is_iq/2,
         is_iq/3,
         is_iq_with_ns/2,
         is_iq_set/1,
         is_iq_get/1,
         is_iq_error/1,
         is_iq_result/1,
         is_iq_result/2,
         is_presence/1,
         is_presence_stanza/1, % backwards compatibility
         is_presence_type/2, %% backwards compatibility
         is_presence_with_type/2,
         is_presence_with_show/2,
         is_presence_with_status/2,
         is_presence_with_priority/2,
         is_stanza_from/2,
         is_roster_get/1,
         is_roster_set/1,
         is_roster_result/1,
         is_last_result/1,
         is_private_result/1,
         is_private_error/1,
         is_result/1, %% backwards compatibility
         count_roster_items/2,
         roster_contains/2,
         is_error/3,
         is_stream_error/3,
         is_privacy_set/1,
         has_type/2,
         is_privacy_result/1,
         is_privacy_result_with_active/1,
         is_privacy_result_with_default/1,
         is_privacy_result_with_active/2,
         is_privacy_result_with_default/2,
         is_privacy_list_nonexistent_error/1,
         is_adhoc_response/3,
         has_service/2,
         has_feature/2,
         has_item/2,
         has_no_such_item/2,
         has_identity/3,
         stanza_timeout/1,
         is_stream_start/1,
         is_stream_end/1,
         is_bosh_report/2,
         is_sm_enabled/1, is_sm_enabled/2,
         is_sm_failed/2,
         is_sm_ack/1, is_sm_ack/2,
         is_sm_ack_request/1,
         is_sm_resumed/1, is_sm_resumed/2,
         has_ns/2,
         is_compressed/1,
         is_mam_archived_message/2
        ]).

-export(['not'/1]).

-type stanza_type() :: binary().
-type namespace() :: binary().

-include("escalus.hrl").
-include("escalus_xmlns.hrl").
-include("escalus_deprecated.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("no_binary_to_integer.hrl").

-import(escalus_compat, [bin/1]).

%%--------------------------------------------------------------------
%% Deprecation support
%%--------------------------------------------------------------------

?DEPRECATED1(is_presence_stanza, is_presence).
?DEPRECATED2(is_presence_type, is_presence_with_type).
?DEPRECATED1(is_result, is_iq_result).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec is_presence(exml:element()) -> boolean().
is_presence(#xmlel{name = <<"presence">>}) ->
    true;
is_presence(_) ->
    false.

-spec is_presence_with_type(binary() | undefined, exml:element()) -> boolean().
is_presence_with_type(<<"available">>, Pres) ->
    is_presence_with_type(undefined, Pres);
is_presence_with_type(Type, Pres) ->
    is_presence(Pres)
    andalso
    has_type(Type, Pres).

-spec is_message(exml:element()) -> boolean().
is_message(#xmlel{name = <<"message">>}) ->
    true;
is_message(_) ->
    false.

-spec is_iq(exml:element()) -> boolean().
is_iq(#xmlel{name = <<"iq">>}) ->
    true;
is_iq(_) ->
    false.

-spec is_iq(stanza_type(), exml:element()) -> boolean().
is_iq(Type, Stanza) ->
    is_iq(Stanza)
    andalso
    has_type(Type, Stanza).

-spec is_iq(stanza_type(), namespace(), exml:element()) -> boolean().
is_iq(Type, NS, Stanza) ->
    is_iq_with_ns(NS, Stanza)
    andalso
    has_type(Type, Stanza).

-spec is_iq_with_ns(namespace(), exml:element()) -> boolean().
is_iq_with_ns(NS, Stanza) ->
    is_iq(Stanza)
    andalso
    bin(NS) == exml_query:path(Stanza, [{element, <<"query">>},
                                        {attr, <<"xmlns">>}]).

-spec is_chat_message(exml:element()) -> boolean().
is_chat_message(Stanza) ->
    is_message(Stanza)
    andalso
    has_type(<<"chat">>, Stanza).

-spec is_forwarded_received_message(binary(), binary(), binary(), exml:element()) -> boolean().
is_forwarded_received_message(OriginalFrom, OriginalTo, Msg, Stanza) ->
    has_carbon(<<"received">>, OriginalFrom, OriginalTo, Msg, Stanza).

-spec is_forwarded_sent_message(binary(), binary(), binary(), exml:element()) -> boolean().
is_forwarded_sent_message(OriginalFrom, OriginalTo, Msg, Stanza) ->
    has_carbon(<<"sent">>, OriginalFrom, OriginalTo, Msg, Stanza).

-spec has_carbon(binary(), binary(), binary(), binary(), exml:element()) -> boolean().
has_carbon(Type, From, To, Msg, Stanza) ->
    Carbon = exml_query:subelement(Stanza, Type),
    has_ns(?NS_CARBONS_2, Carbon)
    andalso
    is_forwarded_message(From, To, Msg, exml_query:subelement(Carbon, <<"forwarded">>)).

-spec is_forwarded_message(binary(), binary(), binary(), exml:element()) -> boolean().
is_forwarded_message(From, To, Msg, #xmlel{name = <<"forwarded">>} = Stanza) ->
    has_ns(?NS_FORWARD_0, Stanza)
    andalso
    is_chat_message_from_to(From, To, Msg,
                            exml_query:subelement(Stanza, <<"message">>)).

-spec is_chat_message(binary(), exml:element()) -> boolean().
is_chat_message(Msg, Stanza) ->
    is_chat_message(Stanza)
    andalso
    bin(Msg) == exml_query:path(Stanza, [{element, <<"body">>}, cdata]).

-spec is_chat_message_from_to(binary(), binary(), binary(), exml:element())
                             -> boolean().
is_chat_message_from_to(From, To, Msg, #xmlel{attrs=Attrs} = Stanza) ->
    is_chat_message(Msg, Stanza)
    andalso
    bin(From) == proplists:get_value(<<"from">>, Attrs)
    andalso
    bin(To) == proplists:get_value(<<"to">>, Attrs).

-spec is_groupchat_message(exml:element()) -> boolean().
is_groupchat_message(Stanza) ->
    is_message(Stanza)
    andalso
    has_type(<<"groupchat">>, Stanza).

%% Xep-0313 archived messages
is_mam_archived_message(Msg, #xmlel{} = Stanza) ->
    M = exml_query:path(Stanza, [{element, <<"result">>},
                                 {element, <<"forwarded">>},
                                 {element, <<"message">>}]),
    is_chat_message(Msg,M).


%% TODO: escalus_compat:bin/1 should be deprecated;
%%       let's just use binaries instead of "maybe strings, maybe binaries"
-spec is_groupchat_message(binary(), exml:element()) -> boolean().
is_groupchat_message(Msg, Stanza) ->
    is_groupchat_message(Stanza)
    andalso
    bin(Msg) == exml_query:path(Stanza, [{element, <<"body">>}, cdata]).

-spec is_headline_message(binary(), binary(), exml:element()) -> boolean().
is_headline_message(Subject, Msg, Stanza) ->
    is_message(Stanza)
    andalso
    has_type(<<"headline">>, Stanza)
    andalso
    bin(Msg) == exml_query:path(Stanza, [{element, <<"body">>}, cdata])
    andalso
    bin(Subject) == exml_query:path(Stanza, [{element, <<"subject">>}, cdata]).

-spec has_type(stanza_type() | undefined, exml:element()) -> boolean().
has_type(undefined, Stanza) ->
    undefined == exml_query:attr(Stanza, <<"type">>);
has_type(Type, Stanza) ->
    bin(Type) == bin(exml_query:attr(Stanza, <<"type">>)).

-spec is_0184_request(exml:element()) -> boolean().
is_0184_request(#xmlel{children = Els}) ->
    #xmlel{ name = <<"request">>,
            attrs = [{<<"xmlns">>, <<"urn:xmpp:receipts">>}],
            children = [] } =:= lists:keyfind(<<"request">>, 2, Els).

-spec is_0184_receipt(exml:element(), exml:element()) -> boolean().
is_0184_receipt(#xmlel{ attrs = ReqAttrs } = Request, Receipt) ->
    {_, ReqTo} = lists:keyfind(<<"to">>, 1, ReqAttrs),
    is_0184_receipt(Request, ReqTo, Receipt).

-spec is_0184_receipt(exml:element(), binary(), exml:element()) -> boolean().
is_0184_receipt(#xmlel{ attrs = ReqAttrs } = _Request,
                ProperResFrom,
                #xmlel{ attrs = ResAttrs,
                        children = [#xmlel{ name = <<"received">>,
                                            attrs = SubAttrs}]} = _Receipt) ->
    {_, ResFrom} = lists:keyfind(<<"from">>, 1, ResAttrs),
    {_, ReqID} = lists:keyfind(<<"id">>, 1, ReqAttrs),
    {_, ResID} = lists:keyfind(<<"id">>, 1, SubAttrs),
    {_, ResXmlns} = lists:keyfind(<<"xmlns">>, 1, SubAttrs),
    binary:longest_common_prefix([ProperResFrom, ResFrom]) == byte_size(ProperResFrom)
    andalso
    ReqID == ResID
    andalso
    ResXmlns == <<"urn:xmpp:receipts">>;
is_0184_receipt(Request, ProperResFrom,
                #xmlel{ children = RecChildren } = Receipt)
  when length(RecChildren) > 1 ->
    case lists:keyfind(<<"received">>, #xmlel.name, RecChildren) of
        false ->
            false;
        Received ->
            is_0184_receipt(Request, ProperResFrom,
                            Receipt#xmlel{ children = [Received] })
    end;
is_0184_receipt(_, _, _) ->
    false.

-spec is_iq_set(exml:element()) -> boolean().
is_iq_set(Stanza) -> is_iq(<<"set">>, Stanza).

-spec is_iq_get(exml:element()) -> boolean().
is_iq_get(Stanza) -> is_iq(<<"get">>, Stanza).

-spec is_iq_error(exml:element()) -> boolean().
is_iq_error(Stanza) -> is_iq(<<"error">>, Stanza).

-spec is_iq_result(exml:element()) -> boolean().
is_iq_result(Stanza) -> is_iq(<<"result">>, Stanza).

-spec is_iq_result(exml:element(), exml:element()) -> boolean().
is_iq_result(QueryStanza, ResultStanza) ->
    QueryId = exml_query:attr(QueryStanza, <<"id">>),
    ResultId = exml_query:attr(ResultStanza, <<"id">>),
    is_iq_result(ResultStanza) andalso QueryId == ResultId.

-spec is_presence_with_show(binary(), exml:element()) -> boolean().
is_presence_with_show(Show, Presence) ->
    is_presence(Presence)
    andalso
    bin(Show) == exml_query:path(Presence, [{element, <<"show">>}, cdata]).

-spec is_presence_with_status(binary(), exml:element()) -> boolean().
is_presence_with_status(Status, Presence) ->
    is_presence(Presence)
    andalso
    bin(Status) == exml_query:path(Presence, [{element, <<"status">>}, cdata]).

-spec is_presence_with_priority(binary(), exml:element()) -> boolean().
is_presence_with_priority(Priority, Presence) ->
    is_presence(Presence)
    andalso
    bin(Priority) == exml_query:path(Presence, [{element, <<"priority">>}, cdata]).

-spec is_stanza_from(escalus_utils:jid_spec(), exml:element()) -> boolean().
is_stanza_from(From, Stanza) ->
    ExpectedJid = escalus_utils:jid_to_lower(escalus_utils:get_jid(From)),
    ActualJid = escalus_utils:jid_to_lower(exml_query:attr(Stanza, <<"from">>)),
    escalus_utils:is_prefix(ExpectedJid, ActualJid).

-spec is_roster_get(exml:element()) -> boolean().
is_roster_get(Stanza) ->
    is_iq(<<"get">>, ?NS_ROSTER, Stanza).

-spec is_roster_set(exml:element()) -> boolean().
is_roster_set(Stanza) ->
    is_iq(<<"set">>, ?NS_ROSTER, Stanza).

-spec is_roster_result(exml:element()) -> boolean().
is_roster_result(Stanza) ->
    is_iq(<<"result">>, ?NS_ROSTER, Stanza).

-spec is_last_result(exml:element()) -> boolean().
is_last_result(Stanza) ->
    is_iq(<<"result">>, ?NS_LAST_ACTIVITY, Stanza).

-spec is_private_result(exml:element()) -> boolean().
is_private_result(Stanza) ->
    is_iq(<<"result">>, ?NS_PRIVATE, Stanza).

-spec is_private_error(exml:element()) -> boolean().
is_private_error(Stanza) ->
    is_iq(<<"error">>, ?NS_PRIVATE, Stanza).

-spec roster_contains(escalus_utils:jid_spec(), exml:element()) -> boolean().
roster_contains(Contact, Stanza) ->
    ExpectedJid = escalus_utils:jid_to_lower(escalus_utils:get_jid(Contact)),
    Items = get_roster_items(Stanza),
    lists:any(fun (#xmlel{} = Element) ->
                      ContactJid = escalus_utils:jid_to_lower(exml_query:attr(Element, <<"jid">>)),
                      Pref = escalus_utils:is_prefix(ContactJid, ExpectedJid),
                      if %% TODO: simplify to `ContactJid == ExpectedJid`
                          ContactJid == ExpectedJid ->
                              true;
                          Pref ->
                              escalus_compat:complain("deprecated use of escalus_pred:roster_contains"),
                              true;
                          true ->
                              false
                      end;
                  (_CData) ->
                      false
              end, Items).

-spec count_roster_items(pos_integer(), exml:element()) -> boolean().
count_roster_items(Num, Stanza) ->
    Num == length(get_roster_items(Stanza)).

-spec is_error(stanza_type(), binary(), exml:element()) -> boolean().
is_error(Type, Condition, Stanza) ->
    Error = exml_query:subelement(Stanza, <<"error">>),
    has_type(<<"error">>, Stanza)
    andalso
    exml_query:attr(Error, <<"type">>) == bin(Type)
    andalso
    exml_query:path(Error, [{element, bin(Condition)},
                            {attr, <<"xmlns">>}]) == ?NS_STANZA_ERRORS.

-spec is_stream_error(stanza_type(), binary(), exml:element()) -> boolean().
is_stream_error(Type, Text, Stanza) ->
    (Stanza#xmlel.name =:= <<"stream:error">> orelse
     (Stanza#xmlel.name =:= <<"error">> andalso has_ns(?NS_XMPP, Stanza)))
    andalso
    exml_query:subelement(Stanza, Type) =/= undefined
    andalso
    case Text of
        <<>> -> true;
        _ -> [#xmlcdata{content = Text}]
             =:= (exml_query:subelement(Stanza, <<"text">>))#xmlel.children
    end.

-spec is_privacy_set(exml:element()) -> boolean().
is_privacy_set(Stanza) ->
    is_iq(<<"set">>, ?NS_PRIVACY, Stanza).

-spec is_privacy_result(exml:element()) -> boolean().
is_privacy_result(Stanza) ->
    is_iq(<<"result">>, ?NS_PRIVACY, Stanza).

-spec is_privacy_result_with(binary(), exml:element()) -> boolean().
is_privacy_result_with(Child, Stanza) ->
    is_privacy_result(Stanza)
    andalso
    has_path(Stanza, [{element, <<"query">>},
                      {element, Child}]).

-spec is_privacy_result_with(binary(), binary(), exml:element()) -> boolean().
is_privacy_result_with(Child, ChildName, Stanza) ->
    is_privacy_result(Stanza)
    andalso
    ChildName == exml_query:path(Stanza, [{element, <<"query">>},
                                          {element, Child},
                                          {attr, <<"name">>}]).

-spec is_privacy_result_with_active(exml:element()) -> boolean().
is_privacy_result_with_active(Stanza) ->
    is_privacy_result_with(<<"active">>, Stanza).

-spec is_privacy_result_with_default(exml:element()) -> boolean().
is_privacy_result_with_default(Stanza) ->
    is_privacy_result_with(<<"default">>, Stanza).

-spec is_privacy_result_with_active(binary(), exml:element()) -> boolean().
is_privacy_result_with_active(ActiveListName, Stanza) ->
    is_privacy_result_with(<<"active">>, ActiveListName, Stanza).

-spec is_privacy_result_with_default(binary(), exml:element()) -> boolean().
is_privacy_result_with_default(DefaultListName, Stanza) ->
    is_privacy_result_with(<<"default">>, DefaultListName, Stanza).

-spec is_privacy_list_nonexistent_error(exml:element()) -> boolean().
is_privacy_list_nonexistent_error(Stanza) ->
    is_iq(<<"error">>, ?NS_PRIVACY, Stanza)
    andalso
    has_path(Stanza, [{element, <<"query">>},
                      {element, <<"list">>},
                      {attr, <<"name">>}])
    andalso
    has_path(Stanza, [{element, <<"error">>},
                      {element, <<"item-not-found">>}]).

-spec is_adhoc_response(binary(), binary(), exml:element()) -> boolean().
is_adhoc_response(Node, Status, Stanza) ->
    is_iq(Stanza)
        andalso
        ?NS_ADHOC == exml_query:path(Stanza, [{element, <<"command">>},
                                              {attr, <<"xmlns">>}])
        andalso
        Node == exml_query:path(Stanza, [{element, <<"command">>},
                                         {attr, <<"node">>}])
        andalso
        Status == exml_query:path(Stanza, [{element, <<"command">>},
                                           {attr, <<"status">>}]).

-spec has_service(binary(), exml:element()) -> boolean().
has_service(Service, #xmlel{children = [ #xmlel{children = Services} ]}) ->
    Pred = fun(Item) ->
               exml_query:attr(Item, <<"jid">>) =:= Service
           end,
    lists:any(Pred, Services).

-spec has_feature(binary(), exml:element()) -> boolean().
has_feature(Feature, Stanza) ->
    Features = exml_query:paths(Stanza, [{element, <<"query">>},
                                         {element, <<"feature">>}]),
    lists:any(fun(Item) ->
                      exml_query:attr(Item, <<"var">>) == Feature
              end,
              Features).

-spec has_item(binary(), exml:element()) -> boolean().
has_item(JID, Stanza) ->
    Items = exml_query:paths(Stanza, [{element, <<"query">>},
                                      {element, <<"item">>}]),
    lists:any(fun(Item) ->
                      exml_query:attr(Item, <<"jid">>) == JID
              end,
              Items).

-spec has_no_such_item(binary(), exml:element()) -> boolean().
has_no_such_item(JID, Stanza) ->
    not has_item(JID, Stanza).

-spec has_identity(binary(), stanza_type(), exml:element()) -> boolean().
has_identity(Category, Type, Stanza) ->
    Idents = exml_query:paths(Stanza, [{element, <<"query">>},
                                       {element, <<"identity">>}]),
    lists:any(fun(Ident) ->
                      (exml_query:attr(Ident, <<"category">>) == Category)
                          and (exml_query:attr(Ident, <<"type">>) == Type)
              end,
              Idents).

%% TODO: Remove as duplicates escalus_assert:has_no_stanzas/1 functionality.
stanza_timeout(Arg) ->
    case element(1, Arg) of
        'EXIT' ->
            case element(1, element(2, Arg)) of
                timeout_when_waiting_for_stanza ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

-spec is_stream_start(exml_stream:element()) -> boolean().
is_stream_start(#xmlstreamstart{}) ->
    true;
is_stream_start(#xmlel{name = <<"open">>}) ->
    true;
is_stream_start(_) ->
    false.

-spec is_stream_end(exml_stream:element()) -> boolean().
is_stream_end(#xmlstreamend{}) ->
    true;
is_stream_end(#xmlel{name = <<"close">>}) ->
    true;
is_stream_end(_) ->
    false.

-spec is_bosh_report(pos_integer(), exml:element()) -> boolean().
is_bosh_report(Rid, #xmlel{name = <<"body">>} = Body) ->
    Rid == binary_to_integer(exml_query:attr(Body, <<"report">>))
    andalso
    exml_query:attr(Body, <<"time">>) /= undefined;
is_bosh_report(_, _) ->
    false.

-spec is_sm_enabled([proplists:property()], exml:element()) -> boolean().
is_sm_enabled(Opts, Stanza) ->
    is_sm_enabled(Stanza)
    andalso case proplists:is_defined(resume, Opts) of
                false ->
                    true;
                true ->
                    lists:member(exml_query:attr(Stanza, <<"resume">>),
                                 [<<"true">>, <<"1">>])
            end.

-spec is_sm_enabled(exml:element()) -> boolean().
is_sm_enabled(#xmlel{name = <<"enabled">>} = Stanza) ->
    has_ns(?NS_STREAM_MGNT_3, Stanza);
is_sm_enabled(_) ->
    false.

-spec is_sm_failed(binary(), exml:element()) -> boolean().
is_sm_failed(Type, #xmlel{name = <<"failed">>} = Stanza) ->
    has_ns(?NS_STREAM_MGNT_3, Stanza)
        andalso
        begin
            Subelem = exml_query:subelement(Stanza, Type),
            is_stanza_error(Type, Subelem)
    end;
is_sm_failed(_, _) ->
    false.

-spec is_stanza_error(binary(), exml:element()) -> boolean().
is_stanza_error(Type, #xmlel{name = T} = Stanza) when Type =:= T ->
    has_ns(?NS_STANZA_ERRORS, Stanza);
is_stanza_error(_, _) ->
    false.

-spec is_sm_ack(exml:element()) -> boolean().
is_sm_ack(#xmlel{name = <<"a">>} = Stanza) ->
    has_ns(?NS_STREAM_MGNT_3, Stanza);
is_sm_ack(_) ->
    false.

-spec is_sm_ack(non_neg_integer(), exml:element()) -> boolean().
is_sm_ack(Handled, #xmlel{name = <<"a">>} = Stanza) ->
    is_sm_ack(Stanza)
        andalso
        Handled == binary_to_integer(exml_query:attr(Stanza, <<"h">>));
is_sm_ack(_, _) ->
    false.

-spec is_sm_ack_request(exml:element()) -> boolean().
is_sm_ack_request(#xmlel{name = <<"r">>} = Stanza) ->
    has_ns(?NS_STREAM_MGNT_3, Stanza);
is_sm_ack_request(_) ->
    false.

-spec is_sm_resumed(exml:element()) -> boolean().
is_sm_resumed(#xmlel{name = <<"resumed">>} = Stanza) ->
    %% Less strict checking (no SMID verification)
    has_ns(?NS_STREAM_MGNT_3, Stanza)
    andalso
    exml_query:attr(Stanza, <<"h">>) /= undefined;
is_sm_resumed(_) ->
    false.

-spec is_sm_resumed(binary(), exml:element()) -> boolean().
is_sm_resumed(SMID, #xmlel{name = <<"resumed">>} = Stanza) ->
    is_sm_resumed(Stanza)
        andalso
        SMID == exml_query:attr(Stanza, <<"previd">>)
        andalso
        exml_query:attr(Stanza, <<"h">>) /= undefined;
is_sm_resumed(_, _) ->
    false.

-spec has_ns(namespace(), exml:element()) -> boolean().
has_ns(NS, Stanza) ->
    NS == exml_query:attr(Stanza, <<"xmlns">>).

-spec is_compressed(exml:element()) -> boolean().
is_compressed(#xmlel{name = <<"compressed">>} = Stanza) ->
    has_ns(?NS_COMPRESS, Stanza);
is_compressed(_) ->
    false.

%%--------------------------------------------------------------------
%% Functors
%%--------------------------------------------------------------------

%% Not supported by erlang 15 :(
%%-spec 'not'( fun((...) -> boolean()) ) ->  fun((...) -> boolean()).
'not'(Pred) when is_function(Pred, 1) ->
    fun (Arg) -> not Pred(Arg) end;
'not'(Pred) when is_function(Pred, 2) ->
    fun (Arg1, Arg2) -> not Pred(Arg1, Arg2) end;
'not'(Pred) when is_function(Pred, 3) ->
    fun (Arg1, Arg2, Arg3) -> not Pred(Arg1, Arg2, Arg3) end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec get_roster_items(exml:element()) -> [exml:element()].
get_roster_items(Stanza) ->
    escalus:assert(is_iq_with_ns, [?NS_ROSTER], Stanza),
    Query = exml_query:subelement(Stanza, <<"query">>),
    Query#xmlel.children.

-spec has_path(exml:element(), exml_query:path()) -> boolean().
has_path(Stanza, Path) ->
    exml_query:path(Stanza, Path) /= undefined.
