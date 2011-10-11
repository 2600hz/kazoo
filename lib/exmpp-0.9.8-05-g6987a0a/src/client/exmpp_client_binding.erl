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
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of Resource Binding.

-module(exmpp_client_binding).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 announced_support/1
	]).

%% Resource binding.
-export([
	 bind/0,
	 bind/1,
	 bounded_jid/1
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Features_Announcement) -> bool()
%%     Features_Announcement = exmpp_xml:xmlel()
%% @throws {resource_binding, announced_support, invalid_feature, Feature}
%% @doc Tell if the Resource Binding feature is supported.

announced_support(#xmlel{ns = ?NS_XMPP, name = 'features'} = El) ->
    case exmpp_xml:get_element(El, ?NS_BIND, 'bind') of
        undefined -> false;
        Child     -> announced_support2(Child)
    end.

announced_support2(#xmlel{children = []}) ->
    true;
announced_support2(Feature) ->
    throw({resource_binding, announced_support, invalid_feature, Feature}).

%% --------------------------------------------------------------------
%% Resource binding.
%% --------------------------------------------------------------------

%% @spec () -> Bind
%%     Bind = exmpp_xml:xmlel()
%% @doc Prepare a Resource Binding request.

bind() ->
    bind(undefined).

%% @spec (Resource) -> Bind
%%     Bind = exmpp_xml:xmlel()
%% @doc Prepare a Resource Binding request for the given `Resource'.

bind(Resource) ->
    Children = case Resource of
		   undefined ->
		       [];
		   "" ->
		       [];
		   _ ->
		       El = #xmlel{
			 ns = ?NS_BIND,
			 name = 'resource'
			},
		       [exmpp_xml:set_cdata(El, Resource)]
	       end,
    Bind = #xmlel{
      ns = ?NS_BIND,
      name = 'bind',
      children = Children
     },
    exmpp_iq:set(?NS_JABBER_CLIENT, Bind, exmpp_utils:random_id("bind")).

%% @spec (Bind) -> Jid
%%     Bind = exmpp_xml:xmlel()
%%     Jid = exmpp_jid:jid()
%% @throws {resource_binding, bounded_jid, invalid_bind, Stanza} |
%%         {resource_binding, bounded_jid, no_jid, IQ} |
%%         {resource_binding, bounded_jid, bind_error, Condition}
%% @doc Extract the JID given by the server.

bounded_jid(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'result' ->
            case exmpp_iq:get_result(IQ) of
                #xmlel{ns = ?NS_BIND, name = 'bind'} = Bind ->
                    case exmpp_xml:get_element(Bind,
					       ?NS_BIND, 'jid') of
                        #xmlel{} = Jid_El ->
                            Jid = exmpp_xml:get_cdata(Jid_El),
                            exmpp_jid:parse(Jid);
                        _ ->
                            throw({resource_binding, bounded_jid, no_jid, IQ})
                    end;
                _ ->
                    throw({resource_binding, bounded_jid, no_jid, IQ})
            end;
        'error' ->
            Condition = exmpp_stanza:get_condition(IQ),
            throw({resource_binding, bounded_jid, bind_error, Condition})
    end;
bounded_jid(Stanza) ->
    throw({resource_binding, bounded_jid, invalid_bind, Stanza}).
