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
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of SASL authentication.
%%
%% <p>
%% Note that it doesn't implement SASL, only feature negotiation at the
%% XMPP level.
%% </p>

-module(exmpp_server_sasl).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 feature/1
	]).

%% SASL exchange.
-export([
	 challenge/1,
	 success/0,
	 success/1,
	 failure/0,
	 failure/1,
	 failure/2,
	 next_step/1
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Mechanisms) -> Feature
%%     Mechanisms = [binary() | string()]
%%     Feature = exmpp_xml:xmlel()
%% @throws {sasl, feature_announcement, invalid_mechanisms_list, []} |
%%         {sasl, feature_announcement, invalid_mechanism, Mechanism}
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature(Mechanisms) ->
    Children = mechanisms_list(Mechanisms),
    #xmlel{ns = ?NS_SASL,
	   name = 'mechanisms',
	   children = Children
	  }.

mechanisms_list([]) ->
    throw({sasl, feature_announcement, invalid_mechanisms_list, []});
mechanisms_list(Mechanisms) ->
    mechanisms_list2(Mechanisms, []).

mechanisms_list2([Mechanism | Rest], Children) ->
    case io_lib:deep_char_list(Mechanism) of
        true ->
            Child = #xmlel{ns = ?NS_SASL,
			   name = 'mechanism'
			  },
            mechanisms_list2(Rest,
			     [exmpp_xml:set_cdata(Child, Mechanism)
			      | Children]);
        false ->
            throw({sasl, feature_announcement, invalid_mechanism, Mechanism})
    end;
mechanisms_list2([], Children) ->
    lists:reverse(Children).

%% --------------------------------------------------------------------
%% SASL exchange.
%% --------------------------------------------------------------------

standard_conditions() ->
    [
     {'aborted'},
     {'incorrect-encoding'},
     {'invalid-authzid'},
     {'invalid-mechanism'},
     {'mechanism-too-weak'},
     {'not-authorized'},
     {'temporary-auth-failure'},
     %% rfc3920bis
     {'account-disabled'},
     {'credentials-expired'},
     {'encryption-required'},
     {'malformed-request'},
     {'undefined-condition'}
    ].

%% @spec (Challenge) -> Challenge_El
%%     Challenge = string() | none
%%     Challenge_El = exmpp_xml:xmlel()
%% @doc Prepare a `<challenge/>' element with the given challenge.
%%
%% `Challenge' will be Base64-encoded by this function.

challenge(none) ->
    #xmlel{ns = ?NS_SASL,
	   name = 'challenge'
	  };
challenge(Challenge) ->
    El = #xmlel{ns = ?NS_SASL,
		name = 'challenge'
	       },
    exmpp_xml:set_cdata(El, base64:encode_to_string(Challenge)).

%% @spec () -> Success_El
%%     Success_El = exmpp_xml:xmlel()
%% @doc Prepare a `<success/>' element.

success() ->
    success(none).

%% @spec (Data) -> Success_El
%%     Data = string() | none
%%     Success_El = exmpp_xml:xmlel()
%% @doc Prepare a `<success/>' element with supplied XML character data.
%% `Data' will be Base64-encoded by this function.

success(none) ->
    #xmlel{ns = ?NS_SASL,
	   name = 'success'
	  };
success(Data) ->
    El = #xmlel{ns = ?NS_SASL,
		name = 'success'
	       },
    exmpp_xml:set_cdata(El, base64:encode_to_string(Data)).

%% @spec () -> Failure
%%     Failure = exmpp_xml:xmlel()
%% @doc Prepare a `<failure/>' element.

failure() ->
    #xmlel{ns = ?NS_SASL,
	   name = 'failure'
	  }.

%% @spec (Condition) -> Failure
%%     Condition = atom()
%%     Failure = exmpp_xml:xmlel()
%% @doc Prepare a `<failure/>' element with a defined condition.

failure(Condition) ->
    case lists:keymember(Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({sasl, failure, invalid_condition, Condition})
    end,
    Condition_El = #xmlel{ns = ?NS_SASL,
			  name = Condition
			 },
    exmpp_xml:append_child(failure(), Condition_El).

%% @spec (Condition, Text) -> Failure
%%     Condition = atom()
%%     Text = string()
%%     Failure = exmpp_xml:xmlel()
%% @doc Prepare a `<failure/>' element with a defined condition and text.

failure(Condition, "") ->
    failure(Condition);
failure(Condition, Text) ->
    case lists:keymember(Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({sasl, failure, invalid_condition, Condition})
    end,
    Condition_El = #xmlel{ns = ?NS_SASL,
			  name = Condition
			 },
    Text_El = #xmlel{ns = ?NS_SASL,
			  name = text,
			  children = exmpp_xml:cdata(Text)
			 },
    exmpp_xml:append_children(failure(), [Condition_El, Text_El]).

%% @spec (El) -> Type
%%     El = exmpp_xml:xmlel()
%%     Type = Auth | Response | Abort
%%     Auth = {auth, Mechanism, none | string()}
%%     Mechanism = string()
%%     Response = {response, string()}
%%     Abort = abort
%% @throws {sasl, next_step, unexpected_element, El}
%% @doc Extract the response that the initiating entity sent.
%%
%% Any response data is Base64-decoded.

next_step(#xmlel{ns = ?NS_SASL, name = 'auth'} = El) ->
    Mechanism = exmpp_xml:get_attribute_as_list(El, <<"mechanism">>, undefined),
    case exmpp_utils:strip(exmpp_xml:get_cdata_as_list(El)) of
        ""      -> {auth, Mechanism, none};
        "="     -> {auth, Mechanism, ""};
        Encoded -> {auth, Mechanism, base64:decode_to_string(Encoded)}
    end;
next_step(#xmlel{ns = ?NS_SASL, name = 'response'} = El) ->
    Encoded = exmpp_xml:get_cdata_as_list(El),
    {response, base64:decode_to_string(Encoded)};
next_step(#xmlel{ns = ?NS_SASL, name = 'abort'}) ->
    abort;
next_step(El) ->
    throw({sasl, next_step, unexpected_element, El}).
