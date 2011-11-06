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
%% @author Karim Gemayel <kgemayel@process-one.net>

-module(exmpp_client_pubsub).

-include("exmpp.hrl").

-define(PUBSUB(NS, Children), (
  #xmlel{ns = NS, name = 'pubsub', children = Children}
)).

-export([
   get_subscriptions/1,
   get_subscriptions/2,
   get_affiliations/1,
   get_affiliations/2,
	 create_node/2,
	 create_node/3,
	 create_instant_node/1,
	 create_instant_node/2,
	 create_and_configure_node/3,
	 create_and_configure_node/4,
	 get_node_configuration/2,
	 get_node_configuration/3,
	 set_node_configuration/3,
	 set_node_configuration/4,
	 get_default_configuration/1,
	 get_default_configuration/2,
	 purge_items/2,
	 purge_items/3,
	 get_owner_subscriptions/2,
	 get_owner_subscriptions/3,
	 set_owner_subscriptions/3,
	 set_owner_subscriptions/4,
	 get_owner_affiliations/2,
	 get_owner_affiliations/3,
	 set_owner_affiliations/3,
	 set_owner_affiliations/4,
	 delete_node/2,
	 delete_node/3,
	 subscribe/3,
	 subscribe/4,
	 unsubscribe/3,
	 unsubscribe/4,
	 get_subscriptions_options/3,
	 get_subscriptions_options/4,
   set_subscriptions_options/4,
   set_subscriptions_options/5,
   subscribe_and_configure/4,
   subscribe_and_configure/5,
   get_items/2,
   get_items/3,
   get_items_by_id/3,
   get_items_by_id/4,
   get_items_max/3,
   get_items_max/4,
   publish/2,
	 publish/3,
	 publish/4,
	 retract/3,
	 retract/4
	]).

%% --------------------------------------------------------------------
%% Publish/subscribe containers.
%% --------------------------------------------------------------------

%% @spec (Service) -> Pubsub_Iq
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving user subscriptions.
%%
%% The stanza `id' is generated automatically.

get_subscriptions(Service) ->
    get_subscriptions(pubsub_id(), Service).

%% @spec (Id, Service) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving user subscriptions.

get_subscriptions(Id, Service) ->
    Subscriptions = #xmlel{ns = ?NS_PUBSUB, name = 'subscriptions'},
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Subscriptions]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service) -> Pubsub_Iq
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving user affiliations.
%%
%% The stanza `id' is generated automatically.

get_affiliations(Service) ->
    get_affiliations(pubsub_id(), Service).

%% @spec (Id, Service) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving user affiliations.

get_affiliations(Id, Service) ->
    Affiliations = #xmlel{ns = ?NS_PUBSUB, name = 'affiliations'},
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Affiliations]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

create_node(Service, Node) ->
    create_node(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating a node on a pubsub service.

create_node(Id, Service, Node) ->
    %% Make the <create/> element.
    Create = exmpp_xml:set_attributes(
	       #xmlel{ns = ?NS_PUBSUB, name = 'create'},
	       [{<<"node">>, Node}]),
    %% Prepare the final <iq/>.
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Create]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service) -> Pubsub_Iq
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating an instant node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

create_instant_node(Service) ->
    create_instant_node(pubsub_id(), Service).

%% @spec (Id, Service) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating an instant node on a pubsub service.

create_instant_node(Id, Service) ->
    Create = #xmlel{ns = ?NS_PUBSUB, name = 'create'},
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Create]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node, Options) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Options = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating a node on a pubsub service with configuration.
%%
%% The stanza `id' is generated automatically.

create_and_configure_node(Service, Node, Options) ->
    create_and_configure_node(pubsub_id(), Service, Node, Options).

%% @spec (Id, Service, Node, Options) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Options = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating a node on a pubsub service with configuration.

create_and_configure_node(Id, Service, Node, Options) ->
    Configure = exmpp_xml:append_child(
	     #xmlel{ns = ?NS_PUBSUB, name = 'configure'},
	     Options),
    Create = exmpp_xml:append_child(
	     exmpp_xml:set_attributes(
	     #xmlel{ns = ?NS_PUBSUB, name = 'create'}, [
	     {<<"node">>, Node}]),
	     Configure),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Create]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for deleting a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

delete_node(Service, Node) ->
    delete_node(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for deleting a node on a pubsub service.

delete_node(Id, Service, Node) ->
    %% Make the <delete/> element.
    Delete = exmpp_xml:set_attributes(
	       #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'delete'},
	       [{<<"node">>, Node}]),
    %% Prepare the final <iq/>.
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Delete]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (From, Service, Node) -> Pubsub_Iq
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for subscribing to a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

subscribe(From, Service, Node) ->
    subscribe(pubsub_id(), From, Service, Node).

%% @spec (Id, From, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating a node on a pubsub service.

subscribe(Id, From, Service, Node) ->
    %% Make the <subscribe/> element.
    Subscribe = exmpp_xml:set_attributes(
		  #xmlel{ns = ?NS_PUBSUB, name = 'subscribe'},
		  [{<<"node">>, Node},
		   {<<"jid">>, From}]),
    %% Prepare the final <iq/>.
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Subscribe]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (From, Service, Node) -> Pubsub_Iq
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for unsubscribing from a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

unsubscribe(From, Service, Node) ->
    unsubscribe(pubsub_id(), From, Service, Node).

%% @spec (Id, From, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for unsubscribing from a node on a pubsub service.

unsubscribe(Id, From, Service, Node) ->
    %% Make the <subscribe/> element.
    Unsubscribe = exmpp_xml:set_attributes(
		  #xmlel{ns = ?NS_PUBSUB, name = 'unsubscribe'},
		  [{<<"node">>, Node},
		   {<<"jid">>, From}]),
    %% Prepare the final <iq/>.
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Unsubscribe]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (From, Service, Node) -> Pubsub_Iq
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving subscriptions options.
%%
%% The stanza `id' is generated automatically.

get_subscriptions_options(From, Service, Node) ->
    get_subscriptions_options(pubsub_id(), From, Service, Node).

%% @spec (Id, From, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving subscriptions options.

get_subscriptions_options(Id, From, Service, Node) ->
    Options = exmpp_xml:set_attributes(
            #xmlel{ns = ?NS_PUBSUB, name = 'options'},
            [{<<"node">>, Node},
	     {<<"jid">>, From}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Options]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (From, Service, Node, DataForm) -> Pubsub_Iq
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     DataForm = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving subscriptions options.
%%
%% The stanza `id' is generated automatically.

set_subscriptions_options(From, Service, Node, DataForm) ->
    set_subscriptions_options(pubsub_id(), From, Service, Node, DataForm).

%% @spec (Id, From, Service, Node, DataForm) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     DataForm = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving subscriptions options.

set_subscriptions_options(Id, From, Service, Node, DataForm) ->
    Options = exmpp_xml:set_attributes(
            #xmlel{ns = ?NS_PUBSUB, name = 'options',
	    children = [DataForm]},
	    [{<<"node">>, Node},
	     {<<"jid">>, From}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Options]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (From, Service, Node, DataForm) -> Pubsub_Iq
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     DataForm = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving subscriptions options.
%%
%% The stanza `id' is generated automatically.

subscribe_and_configure(From, Service, Node, DataForm) ->
    subscribe_and_configure(pubsub_id(), From, Service, Node, DataForm).

%% @spec (Id, From, Service, Node, DataForm) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     DataForm = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving subscriptions options.

subscribe_and_configure(Id, From, Service, Node, DataForm) ->
    Options = #xmlel{ns = ?NS_PUBSUB, name = 'options'},
    Subscribe = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB, name = 'subscribe',
	    children = [DataForm]},
	    [{<<"node">>, Node},
	     {<<"jid">>, From}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Options, Subscribe]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving default configuration options.
%%
%% The stanza `id' is generated automatically.

get_node_configuration(Service, Node) ->
    get_node_configuration(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving default configuration options.

get_node_configuration(Id, Service, Node) ->
    Configure = exmpp_xml:set_attribute(
            #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'configure'},
            <<"node">>, Node),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Configure]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node, Options) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Options = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for setting configuration options.
%%
%% The stanza `id' is generated automatically.

set_node_configuration(Service, Node, Options) ->
    set_node_configuration(pubsub_id(), Service, Node, Options).

%% @spec (Id, Service, Node, Options) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Options = exmpp_xml:xmlel()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for setting configuration options.

set_node_configuration(Id, Service, Node, Options) ->
    Configure = exmpp_xml:append_child(
	       exmpp_xml:set_attribute(
	       #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'configure'}, <<"node">>, Node),
	       Options),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Configure]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service) -> Pubsub_Iq
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for getting default configuration options.
%%
%% The stanza `id' is generated automatically.

get_default_configuration(Service) ->
    get_default_configuration(pubsub_id(), Service).

%% @spec (Id, Service) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for getting default configuration options.

get_default_configuration(Id, Service) ->
    Default = #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'default'},
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Default]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for purging all items from a pubsub node.
%%
%% The stanza `id' is generated automatically.

purge_items(Service, Node) ->
    purge_items(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for purging all items from a pubsub node.

purge_items(Id, Service, Node) ->
    Purge = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'purge'}, [
	    {<<"node">>, Node}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Purge]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving list of subscriptions.
%%
%% The stanza `id' is generated automatically.

get_owner_subscriptions(Service, Node) ->
    get_owner_subscriptions(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving list of subscriptions.

get_owner_subscriptions(Id, Service, Node) ->
    Subscriptions = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscriptions'}, [
	    {<<"node">>, Node}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Subscriptions]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node, Subscribers) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Subscribers = [{Jid, State}]
%%     Jid = string()
%%     State = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for setting list of subscriptions.
%%
%% The stanza `id' is generated automatically.

set_owner_subscriptions(Service, Node, Subscribers) ->
    set_owner_subscriptions(pubsub_id(), Service, Node, Subscribers).

%% @spec (Id, Service, Node, Subscribers) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Subscribers = [{Jid, State}]
%%     Jid = string()
%%     State = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for setting list of subscriptions.

set_owner_subscriptions(Id, Service, Node, Subscribers) ->
    SetSubscriptions = fun({Jid, State}) ->
                           exmpp_xml:set_attributes(
                           #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscription'}, [
                           {<<"jid">>, Jid},
                           {<<"subscription">>, State}])
                       end,
    Subscriptions = exmpp_xml:append_children(
	    exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'subscriptions'}, [
	    {<<"node">>, Node}]),
	    lists:map(SetSubscriptions, Subscribers)),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Subscriptions]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for getting list of affiliations.
%%
%% The stanza `id' is generated automatically.

get_owner_affiliations(Service, Node) ->
    get_owner_affiliations(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for getting list of affiliations.

get_owner_affiliations(Id, Service, Node) ->
    Affiliations = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'affiliations'}, [
	    {<<"node">>, Node}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Affiliations]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).


%% @spec (Service, Node, Affiliates) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Affiliates = [{Jid, Affiliation}]
%%     Jid = string()
%%     Affiliation = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for setting list of affiliations.
%%
%% The stanza `id' is generated automatically.

set_owner_affiliations(Service, Node, Affiliates) ->
    set_owner_affiliations(pubsub_id(), Service, Node, Affiliates).

%% @spec (Id, Service, Node, Affiliates) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Affiliates = [{Jid, Affiliation}]
%%     Jid = string()
%%     Affiliation = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for setting list of affiliations.

set_owner_affiliations(Id, Service, Node, Affiliates) ->
    SetAffiliations = fun({Jid, Affiliation}) ->
                           exmpp_xml:set_attributes(
                           #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'affiliation'}, [
                           {<<"jid">>, Jid},
                           {<<"affiliation">>, Affiliation}])
                      end,
    Affiliations = exmpp_xml:append_children(
	    exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB_OWNER, name = 'affiliations'}, [
	    {<<"node">>, Node}]),
	    lists:map(SetAffiliations, Affiliates)),
    Pubsub = ?PUBSUB(?NS_PUBSUB_OWNER, [Affiliations]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving pubsub items.
%%
%% The stanza `id' is generated automatically.

get_items(Service, Node) ->
    get_items(pubsub_id(), Service, Node).

%% @spec (Id, Service, Node) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving pubsub items.

get_items(Id, Service, Node) ->
    Items = exmpp_xml:set_attribute(
            #xmlel{ns = ?NS_PUBSUB, name = 'items'},
            <<"node">>, Node),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Items]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub). 

%% @spec (Service, Node, ItemsID) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     ItemsID = [ItemID]
%%     ItemID = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving pubsub items by id.
%%
%% The stanza `id' is generated automatically.

get_items_by_id(Service, Node, ItemsID) ->
    get_items_by_id(pubsub_id(), Service, Node, ItemsID).

%% @spec (Id, Service, Node, ItemsID) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     ItemsID = [ItemID]
%%     ItemID = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving pubsub items by id.

get_items_by_id(Id, Service, Node, ItemsID) ->
    Items = exmpp_xml:set_attribute(
            #xmlel{ns = ?NS_PUBSUB, name = 'items'},
            <<"node">>, Node),
    SetItemsID = fun(ItemID) ->
                     exmpp_xml:set_attribute(
                     #xmlel{ns = ?NS_PUBSUB, name = 'item'},
                     <<"id">>, ItemID)
                 end,
    Items1 = exmpp_xml:append_children(Items,
             lists:map(SetItemsID, ItemsID)),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Items1]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub). 

%% @spec (Service, Node, Max) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Max = integer()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving pubsub 'n' items.
%%
%% The stanza `id' is generated automatically.

get_items_max(Service, Node, Max) ->
    get_items_max(pubsub_id(), Service, Node, Max).

%% @spec (Id, Service, Node, Max) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     Max = integer()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for retrieving pubsub 'n' items.

get_items_max(Id, Service, Node, Max) ->
    Items = exmpp_xml:set_attributes(
            #xmlel{ns = ?NS_PUBSUB, name = 'items'}, [
            {<<"node">>, Node},
            {<<"max_items">>, integer_to_list(Max)}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Items]),
    Iq = ?IQ_GET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Node, Items) -> Pubsub_Iq
%%     Node = string()
%%     Items = [exmpp_xml:xmlel() | exmpp_xml:xmlcdata()]
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for publishing an item to a node on a pubsub service as PEP (i.e. without 'to' and 'from').
%%
%% The stanza `id' is generated automatically.

publish(Node, Item_Children) ->
    Item = #xmlel{ns = ?NS_PUBSUB, name = 'item',
	    children = Item_Children},
    Publish = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB, name = 'publish',
	    children = [Item]}, [
	    {<<"node">>, Node}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Publish]),
    Iq = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_JABBER_CLIENT, name = 'iq'}, [
	    {<<"type">>, "set"}]),
    exmpp_xml:append_child(Iq, Pubsub). 

%% @spec (Service, Node, Items) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     Items = [exmpp_xml:xmlel() | exmpp_xml:xmlcdata()]
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for publishing an item to a node on a pubsub service.
%%
%% The stanza `id' is generated automatically.

publish(Service, Node, Item_Child) when is_tuple(Item_Child) ->
    publish(pubsub_id(), Service, Node, [Item_Child]);

publish(Service, Node, Item_Children) ->
    publish(pubsub_id(), Service, Node, Item_Children).

%% @spec (Id, Service, Node, Items) -> Pubsub_Iq
%%     Id = string()
%%     From = string()
%%     Service = string()
%%     Node = string()
%%     Items = [exmpp_xml:xmlel() | exmpp_xml:xmlcdata()]
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for creating a node on a pubsub service.

publish(Id, Service, Node, Item_Child) when is_tuple(Item_Child) ->
    publish(Id, Service, Node, [Item_Child]);

publish(Id, Service, Node, Item_Children) ->
    %% Prepare item.
    Item = #xmlel{ns = ?NS_PUBSUB, name = 'item',
		  children = Item_Children},
    %% Make the <publish/> element.
    Publish = exmpp_xml:set_attributes(
		#xmlel{ns = ?NS_PUBSUB, name = 'publish',
		       children = [Item]},
		[{<<"node">>, Node}]),
    %% Prepare the final <iq/>.
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Publish]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub).

%% @spec (Service, Node, ItemID) -> Pubsub_Iq
%%     Service = string()
%%     Node = string()
%%     ItemID = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for deleting a pubsub item.
%%
%% The stanza `id' is generated automatically.

retract(Service, Node, ItemID) ->
    retract(pubsub_id(), Service, Node, ItemID).

%% @spec (Id, Service, Node, ItemID) -> Pubsub_Iq
%%     Id = string()
%%     Service = string()
%%     Node = string()
%%     ItemID = string()
%%     Pubsub_Iq = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for deleting a pubsub item.

retract(Id, Service, Node, ItemID) ->
    Item = exmpp_xml:set_attribute(
	   #xmlel{ns = ?NS_PUBSUB, name = 'item'},
	   {<<"id">>, ItemID}),
    Retract = exmpp_xml:set_attributes(
	    #xmlel{ns = ?NS_PUBSUB, name = 'retract',
	    children = [Item]}, [
	    {<<"node">>, Node}]),
    Pubsub = ?PUBSUB(?NS_PUBSUB, [Retract]),
    Iq = ?IQ_SET(Service, Id),
    exmpp_xml:append_child(Iq, Pubsub). 

%% @spec () -> Pubsub_ID
%%     Pubsub_ID = string()
%% @doc Generate a random pubsub iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

pubsub_id() ->
    "pubsub-" ++ integer_to_list(random:uniform(65536 * 65536)).

