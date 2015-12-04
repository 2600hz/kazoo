%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides helpers to do message
%% common operations.

-module(exmpp_message).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Message creation.
-export([
	 normal/0,
	 normal/1,
	 normal/2,
	 make_normal/1,
	 make_normal/2,
	 make_normal/3,
	 chat/0,
	 chat/1,
	 chat/2,
	 make_chat/1,
	 make_chat/2,
	 make_chat/3,
	 groupchat/0,
	 groupchat/1,
	 groupchat/2,
	 make_groupchat/1,
	 make_groupchat/2,
	 make_groupchat/3,
	 headline/0,
	 headline/1,
	 headline/2,
	 make_headline/1,
	 make_headline/2,
	 make_headline/3,
	 error/2
	]).

%% Message standard attributes.
-export([
	 is_message/1,
	 get_type/1,
	 set_type/2,
	 get_subject/1,
	 set_subject/2,
	 get_body/1,
	 set_body/2,
	 get_thread/1,
	 set_thread/2
	]).

-define(EMPTY_MESSAGE(NS), #xmlel{ns = NS, name = 'message'}).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-type(messagetype() ::
      normal    |
      chat      |
      groupchat |
      headline  |
      error
     ).

%% --------------------------------------------------------------------
%% Message creation.
%% --------------------------------------------------------------------

%% @spec () -> Message
%% @doc Create an empty message stanza.
%%
%% The default namespace is `jabber:client'.

-spec normal
() -> xmlel().

normal() ->
    make_normal(?NS_JABBER_CLIENT).

%% @spec (Body) -> Message
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a message stanza with a given body.
%%
%% The default namespace is `jabber:client'.

-spec normal
(binary() | string()) -> xmlel().

normal(Body) ->
    make_normal(?NS_JABBER_CLIENT, Body).

%% @spec (Subject, Body) -> Message
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a message stanza with given subject and body.
%%
%% The default namespace is `jabber:client'.

-spec normal
(binary() | string(), binary() | string()) -> xmlel().

normal(Subject, Body) ->
    make_normal(?NS_JABBER_CLIENT, Subject, Body).

%% @spec (NS) -> Message
%%     NS = atom() | string()
%%     Message = exmpp_xml:xmlel()
%% @doc Create an empty message stanza.

-spec make_normal
(xmlname()) -> xmlel().

make_normal(NS) ->
    set_type(?EMPTY_MESSAGE(NS), 'normal').

%% @spec (NS, Body) -> Message
%%     NS = atom() | string()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a message stanza with a given body.

-spec make_normal
(xmlname(), binary() | string()) -> xmlel().

make_normal(NS, Body) ->
    set_body(make_normal(NS), Body).

%% @spec (NS, Subject, Body) -> Message
%%     NS = atom() | string()
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a message stanza with given subject and body.

-spec make_normal
(xmlname(), binary() | string(), binary() | string()) -> xmlel().

make_normal(NS, Subject, Body) ->
    set_body(set_subject(make_normal(NS), Subject), Body).

%% @spec () -> Message
%%     Message = exmpp_xml:xmlel()
%% @doc Create an empty chat message stanza.
%%
%% The default namespace is `jabber:client'.

-spec chat
() -> xmlel().

chat() ->
    make_chat(?NS_JABBER_CLIENT).

%% @spec (Body) -> Message
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a chat message stanza with a given body.
%%
%% The default namespace is `jabber:client'.

-spec chat
(binary() | string()) -> xmlel().

chat(Body) ->
    make_chat(?NS_JABBER_CLIENT, Body).

%% @spec (Subject, Body) -> Message
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a chat message stanza with given subject and body.
%%
%% The default namespace is `jabber:client'.

-spec chat
(binary() | string(), binary() | string()) -> xmlel().

chat(Subject, Body) ->
    make_chat(?NS_JABBER_CLIENT, Subject, Body).

%% @spec (NS) -> Message
%%     NS = atom() | string()
%%     Message = exmpp_xml:xmlel()
%% @doc Create an empty chat message stanza.

-spec make_chat
(xmlname()) -> xmlel().

make_chat(NS) ->
    set_type(?EMPTY_MESSAGE(NS), 'chat').

%% @spec (NS, Body) -> Message
%%     NS = atom() | string()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a chat message stanza with a given body.

-spec make_chat
(xmlname(), binary() | string()) -> xmlel().

make_chat(NS, Body) ->
    set_body(make_chat(NS), Body).

%% @spec (NS, Subject, Body) -> Message
%%     NS = atom() | string()
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a chat message stanza with given subject and body.

-spec make_chat
(xmlname(), binary() | string(), binary() | string()) -> xmlel().

make_chat(NS, Subject, Body) ->
    set_body(set_subject(make_chat(NS), Subject), Body).

%% @spec () -> Message
%% @doc Create an empty groupchat message stanza.
%%
%% The default namespace is `jabber:client'.

-spec groupchat
() -> xmlel().

groupchat() ->
    make_groupchat(?NS_JABBER_CLIENT).

%% @spec (Body) -> Message
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a groupchat message stanza with a given body.
%%
%% The default namespace is `jabber:client'.

-spec groupchat
(binary() | string()) -> xmlel().

groupchat(Body) ->
    make_groupchat(?NS_JABBER_CLIENT, Body).

%% @spec (Subject, Body) -> Message
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a groupchat message stanza with given subject and body.
%%
%% The default namespace is `jabber:client'.

-spec groupchat
(binary() | string(), binary() | string()) -> xmlel().

groupchat(Subject, Body) ->
    make_groupchat(?NS_JABBER_CLIENT, Subject, Body).

%% @spec (NS) -> Message
%%     NS = atom() | string()
%%     Message = exmpp_xml:xmlel()
%% @doc Create an empty groupchat message stanza.

-spec make_groupchat
(xmlname()) -> xmlel().

make_groupchat(NS) ->
    set_type(?EMPTY_MESSAGE(NS), 'groupchat').

%% @spec (NS, Body) -> Message
%%     NS = atom() | string()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a groupchat message stanza with a given body.

-spec make_groupchat
(xmlname(), binary() | string()) -> xmlel().

make_groupchat(NS, Body) ->
    set_body(make_groupchat(NS), Body).

%% @spec (NS, Subject, Body) -> Message
%%     NS = atom() | string()
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a groupchat message stanza with given subject and body.

-spec make_groupchat
(xmlname(), binary() | string(), binary() | string()) -> xmlel().

make_groupchat(NS, Subject, Body) ->
    set_body(set_subject(make_groupchat(NS), Subject), Body).

%% @spec () -> Message
%% @doc Create an empty headline message stanza.
%%
%% The default namespace is `jabber:client'.

-spec headline
() -> xmlel().

headline() ->
    make_headline(?NS_JABBER_CLIENT).

%% @spec (Body) -> Message
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a headline message stanza with a given body.
%%
%% The default namespace is `jabber:client'.

-spec headline
(binary() | string()) -> xmlel().

headline(Body) ->
    make_headline(?NS_JABBER_CLIENT, Body).

%% @spec (Subject, Body) -> Message
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a headline message stanza with given subject and body.
%%
%% The default namespace is `jabber:client'.

-spec headline
(binary() | string(), binary() | string()) -> xmlel().

headline(Subject, Body) ->
    make_headline(?NS_JABBER_CLIENT, Subject, Body).

%% @spec (NS) -> Message
%%     NS = atom() | string()
%%     Message = exmpp_xml:xmlel()
%% @doc Create an empty headline message stanza.

-spec make_headline
(xmlname()) -> xmlel().

make_headline(NS) ->
    set_type(?EMPTY_MESSAGE(NS), 'headline').

%% @spec (NS, Body) -> Message
%%     NS = atom() | string()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a headline message stanza with a given body.

-spec make_headline
(xmlname(), binary() | string()) -> xmlel().

make_headline(NS, Body) ->
    set_body(make_headline(NS), Body).

%% @spec (NS, Subject, Body) -> Message
%%     NS = atom() | string()
%%     Subject = string() | binary()
%%     Body = string() | binary()
%%     Message = exmpp_xml:xmlel()
%% @doc Create a headline message stanza with given subject and body.

-spec make_headline
(xmlname(), binary() | string(), binary() | string()) -> xmlel().

make_headline(NS, Subject, Body) ->
    set_body(set_subject(make_headline(NS), Subject), Body).

%% @spec (Message, Error) -> New_Message
%%     Message = exmpp_xml:xmlel()
%%     Error = exmpp_xml:xmlel() | atom()
%%     New_Message = exmpp_xml:xmlel()
%% @doc Prepare a message stanza to notify an error.
%%
%% If `Error' is an atom, it must be a standard condition defined by
%% XMPP Core.

-spec error
(xmlel(), xmlel() | atom()) -> xmlel().

error(Message, Condition) when is_atom(Condition) ->
    Error = exmpp_stanza:error(Message#xmlel.ns, Condition),
    error(Message, Error);
error(Message, Error) when ?IS_MESSAGE(Message) ->
    exmpp_stanza:reply_with_error(Message, Error).

%% --------------------------------------------------------------------
%% Message standard attributes.
%% --------------------------------------------------------------------

%% @spec (El) -> bool
%%     El = exmpp_xml:xmlel()
%% @doc Tell if `El' is a message.
%%
%% You should probably use the `IS_MESSAGE(El)' guard expression.

-spec is_message
(xmlel()) -> boolean().

is_message(Message) when ?IS_MESSAGE(Message) -> true;
is_message(_El)                               -> false.

%% @spec (Message) -> Type
%%     Message = exmpp_xml:xmlel()
%%     Type = chat | groupchat | headline | normal | error
%% @doc Return the type of the given `<message/>'.

-spec get_type
(xmlel()) -> messagetype().

get_type(Message) when ?IS_MESSAGE(Message) ->
    case exmpp_stanza:get_type(Message) of
        <<"chat">>      -> 'chat';
        <<"groupchat">> -> 'groupchat';
        <<"headline">>  -> 'headline';
        <<"normal">>    -> 'normal';
        <<"error">>     -> 'error';
        _               -> 'normal'
    end.

%% @spec (Message, Type) -> New_Message
%%     Message = exmpp_xml:xmlel()
%%     Type = chat | groupchat | headline | normal | error | binary()
%%     New_Message = exmpp_xml:xmlel()
%% @doc Set the type of the given `<message/>'.
%%
%% If `Type' isn't a valid, the type is set to `normal'.

-spec set_type
(xmlel(), messagetype() | binary() | string()) -> xmlel().

set_type(Message, Type) when is_binary(Type) ->
    set_type(Message, binary_to_list(Type));
set_type(Message, Type) when is_list(Type) ->
    set_type(Message, list_to_atom(Type));

set_type(Message, Type) when ?IS_MESSAGE(Message), is_atom(Type) ->
    Type_B = case Type of
		 'chat'      -> <<"chat">>;
		 'groupchat' -> <<"groupchat">>;
		 'headline'  -> <<"headline">>;
		 'normal'    -> <<"normal">>;
		 'error'     -> <<"error">>;
		 _           -> <<"normal">>
				    end,
    exmpp_stanza:set_type(Message, Type_B).

%% @spec (Message) -> Subject | undefined
%%     Message = exmpp_xml:xmlel()
%%     Subject = binary()
%% @doc Return the subject of the message.

-spec get_subject
(xmlel()) -> binary() | undefined.

get_subject(#xmlel{ns = NS} = Message) when ?IS_MESSAGE(Message) ->
    case exmpp_xml:get_element(Message, NS, 'subject') of
        undefined ->
            undefined;
        Subject_El ->
            exmpp_xml:get_cdata(Subject_El)
    end.

%% @spec (Message, Subject) -> New_Message
%%     Message = exmpp_xml:xmlel()
%%     Subject = string() | binary()
%%     New_Message = exmpp_xml:xmlel()
%% @doc Set the `<subject/>' field of a message stanza.
%%
%% If `Subject' is an empty string (or an empty binary), the previous
%% subject is removed.

-spec set_subject
(xmlel(), binary() | string()) -> xmlel().

set_subject(#xmlel{ns = NS} = Message, <<>>) when ?IS_MESSAGE(Message) ->
    exmpp_xml:remove_element(Message, NS, 'subject');
set_subject(#xmlel{ns = NS} = Message, "") when ?IS_MESSAGE(Message) ->
    exmpp_xml:remove_element(Message, NS, 'subject');
set_subject(#xmlel{ns = NS} = Message, Subject) when ?IS_MESSAGE(Message) ->
    New_Subject_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'subject'},
					 Subject),
    case exmpp_xml:get_element(Message, NS, 'subject') of
        undefined ->
            exmpp_xml:append_child(Message, New_Subject_El);
        Subject_El ->
            exmpp_xml:replace_child(Message, Subject_El, New_Subject_El)
    end.

%% @spec (Message) -> Body | undefined
%%     Message = exmpp_xml:xmlel()
%%     Body = binary()
%% @doc Return the body of the message.

-spec get_body
(xmlel()) -> binary() | undefined.

get_body(#xmlel{ns = NS} = Message) when ?IS_MESSAGE(Message) ->
    case exmpp_xml:get_element(Message, NS, 'body') of
        undefined ->
            undefined;
        Body_El ->
            exmpp_xml:get_cdata(Body_El)
    end.

%% @spec (Message, Body) -> New_Message
%%     Message = exmpp_xml:xmlel()
%%     Body = string() | binary()
%%     New_Message = exmpp_xml:xmlel()
%% @doc Set the `<body/>' field of a message stanza.
%%
%% If `Body' is an empty string (or an empty binary), the previous
%% body is removed.

-spec set_body
(xmlel(), binary() | string()) -> xmlel().

set_body(#xmlel{ns = NS} = Message, <<>>) when ?IS_MESSAGE(Message) ->
    exmpp_xml:remove_element(Message, NS, 'body');
set_body(#xmlel{ns = NS} = Message, "") when ?IS_MESSAGE(Message) ->
    exmpp_xml:remove_element(Message, NS, 'body');
set_body(#xmlel{ns = NS} = Message, Body) when ?IS_MESSAGE(Message) ->
    New_Body_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'body'},
				      Body),
    case exmpp_xml:get_element(Message, NS, 'body') of
        undefined ->
            exmpp_xml:append_child(Message, New_Body_El);
        Body_El ->
            exmpp_xml:replace_child(Message, Body_El, New_Body_El)
    end.

%% @spec (Message) -> Thread | undefined
%%     Message = exmpp_xml:xmlel()
%%     Thread = binary()
%% @doc Return the thread of the message.

-spec get_thread
(xmlel()) -> binary() | undefined.

get_thread(#xmlel{ns = NS} = Message) when ?IS_MESSAGE(Message) ->
    case exmpp_xml:get_element(Message, NS, 'thread') of
        undefined ->
            undefined;
        Thread_El ->
            exmpp_xml:get_cdata(Thread_El)
    end.

%% @spec (Message, Thread) -> New_Message
%%     Message = exmpp_xml:xmlel()
%%     Thread = string() | binary()
%%     New_Message = exmpp_xml:xmlel()
%% @doc Set the `<thread/>' field of a message stanza.
%%
%% If `Thread' is an empty string (or an empty binary), the previous
%% thread is removed.

-spec set_thread
(xmlel(), binary()) -> xmlel().

set_thread(#xmlel{ns = NS} = Message, <<>>) when ?IS_MESSAGE(Message) ->
    exmpp_xml:remove_element(Message, NS, 'thread');
set_thread(#xmlel{ns = NS} = Message, "") when ?IS_MESSAGE(Message) ->
    exmpp_xml:remove_element(Message, NS, 'thread');
set_thread(#xmlel{ns = NS} = Message, Thread) when ?IS_MESSAGE(Message) ->
    New_Thread_El = exmpp_xml:set_cdata(#xmlel{ns = NS, name = 'thread'},
					Thread),
    case exmpp_xml:get_element(Message, NS, 'thread') of
        undefined ->
            exmpp_xml:append_child(Message, New_Thread_El);
        Thread_El ->
            exmpp_xml:replace_child(Message, Thread_El, New_Thread_El)
    end.
