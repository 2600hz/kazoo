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

%% @author Karim Gemayel <karim.gemayel@pocess-one.net>

%% @doc
%% The module <strong>{@module}</strong> provides helpers to generate
%% entities capabilities hash based on sha-1.
%%
%% @reference Depends on
%% <a href="http://www.erlang.org/doc/man/crypto.html">crypto</a>.


-module(exmpp_caps).

-include("exmpp.hrl").

-export([
	 make/1,
	 make/2,
	 make/3
	]).


%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type ecaps() = {ecaps, Identities, Features, Forms}
%%     Identities = identity() | [identity()]
%%     Features   = [ns()]     | []
%%     Forms      = form()     | [form()] | [].
%% Record representing a Caps.


%% --------------------------------------------------------------------
%% Hash creation.
%% --------------------------------------------------------------------

%% @spec (Caps) -> Hash
%%     Caps = ecaps() | identity() | [identity()]
%%     Hash = hash()
%% @doc Generate a hash from a Caps record.

-spec(make/1 :: (ecaps() | identity() | [identity()]) -> hash()).

make(#identity{} = Identity) ->
    make(Identity, []);
make([]) ->
    throw({exmpp_caps, make, 'Identity : at least one identity() required'});
make(Identities) when is_list(Identities) ->
    make(Identities, []);
make(#ecaps{identities = []}) ->
    throw({exmpp_caps, make, 'Caps : at least one identity() required'});
make(#ecaps{identities = Identities, features = Features, forms = []}) ->
    make(Identities, Features);
make(#ecaps{identities = Identities, features = Features, forms = Forms}) ->
    make(Identities, Features, Forms);
make(_) ->
    throw({exmpp_caps, make, 'Caps : ecaps() | identity() | [identity()]'}).


%% @spec (Identities, Features) -> Hash
%%     Identities = identity() | [identity()]
%%     Features   = [ns()]
%%     Hash       = hash()
%% @doc Generate a hash from an identity record or from a list of records
%%      and a list of namespaces .

-spec(make/2 :: (identity() | [identity()], [ns()]) -> hash()).

make(_, Features) when not is_list(Features) ->
    throw({exmpp_caps, make, 'Features : [ns()]'});
make(#identity{} = Identity, []) ->
    hash_caps(identities(Identity));
make(#identity{} = Identity, Features) ->
    hash_caps(identities(Identity) ++ features(Features));
make(Identities, _) when not is_list(Identities) ->
    throw({exmpp_caps, make, 'Identities : identity() | [identity()]'});
make([], _) ->
    throw({exmpp_caps, make, 'Identities : at least one identity() required'});
make(Identities, []) ->
    hash_caps(identities(Identities));
make(Identities, Features) ->
    hash_caps(identities(Identities) ++ features(Features)).


%% @spec (Identities, Features, Forms) -> Hash
%%     Identities = identity() | [identity()]
%%     Features   = [ns()]
%%     Forms      = form()     | [form()]
%%     Hash       = hash()
%% @doc Generate a hash from an identity record or from a list of identity records,
%%      a list of namespaces, and a form or a list of forms .

-spec(make/3 :: (identity() | [identity()], [ns()], form() | [form()]) -> hash()).

make(_, Features, _) when not is_list(Features) ->
    throw({exmpp_caps, make, 'Features : [ns()]'});
make(Identities, Features, #form{} = Form) ->
    make(Identities, Features, [Form]);
make(_, _, Forms) when not is_list(Forms) ->
    throw({exmpp_caps, make, 'Forms : form() | [form()]'});
make(#identity{} = Identity, Features, Forms) ->
    make([Identity], Features, Forms);
make(Identities, _, _) when not is_list(Identities) ->
    throw({exmpp_caps, make, 'Identities : identity() | [identity()]'});
make([], _, _) ->
    throw({exmpp_caps, make, 'Identities : at least one identity() required'});
make(Identities, [], []) ->
    hash_caps(identities(Identities));
make(Identities, Features, []) ->
    hash_caps(identities(Identities) ++ features(Features));
make(Identities, [], Forms) ->
    hash_caps(identities(Identities) ++ forms(Forms));
make(Identities, Features, Forms) ->
    hash_caps(identities(Identities) ++ features(Features) ++ forms(Forms)).


%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

%% @spec (Forms) -> String
%%     Forms  = form()  | [form()]
%%     String = string()

-spec(forms/1 :: (form()  | [form()]) -> string()).

forms(#form{type = Type, fields = Fields}) ->
    Type ++ "<" ++ fields(Fields);
forms(Forms) when is_list(Forms) ->
    lists:foldl(
      fun(#form{} = Form, String) -> String ++ forms(Form) end,
      "", lists:sort(Forms));
forms(_) ->
    throw({exmpp_caps, forms, 'Forms : form() | [form()]'}).


%% @spec (Fields) -> String
%%     Fields = field() | [field()]
%%     String = string()

-spec(fields/1 :: (field()  | [field()]) -> string()).

fields(#field{var = Var, values = Values}) ->
    Var ++ "<" ++ values(Values);
fields(Fields) when is_list(Fields) ->
    lists:foldl(
      fun(#field{} = Field, String) -> String ++ fields(Field) end,
      "", lists:sort(Fields));
fields(_) ->
    throw({exmpp_caps, fields, 'Fields : field() | [field()]'}).


%% @spec (Values) -> String
%%     Values = [value()]
%%     String = string()

-spec(values/1 :: ([value()]) -> string()).

values(Values) when is_list(Values) ->
    lists:foldl(
      fun(Value, String) ->
	      Data = case is_binary(Value) of
			 true  -> binary_to_list(Value);
			 false -> Value
		     end,
	      String ++ Data ++ "<"
      end,
      "", lists:sort(Values));
values(_) ->
    throw({exmpp_caps, values, 'Values : [value()]'}).


%% @spec (Features) -> String
%%     Features = [ns()]
%%     String   = string()

-spec(features/1 :: ([ns()]) -> string()).

features(Features) when is_list(Features) ->
    values(Features);
features(_) ->
    throw({exmpp_caps, features, 'Features : [ns()]'}).


%% @spec (Identities) -> String
%%     Identities = identity() | [identity()]
%%     String     = string()

-spec(identities/1 :: (identity() | [identity()]) -> string()).

identities(#identity{category = Category, type = Type, lang = Lang, name = Name}) ->
    Category ++ "/" ++ Type ++ "/" ++ Lang ++ "/" ++ Name ++ "<";
identities(Identities) when is_list(Identities) ->
    lists:foldl(
      fun(#identity{} = Identity, String) -> String ++ identities(Identity) end,
      "", lists:sort(Identities));
identities(_) ->
    throw({exmpp_caps, identities, 'Identities : identity() | [identity()]'}).


%% @spec (String) -> Hash
%%     String = string()
%%     Hash   = hash()

-spec(hash_caps/1 :: (string()) -> hash()).

hash_caps(String) when is_list(String)->
    base64:encode(crypto:sha(unicode:characters_to_list(String)));
hash_caps(_) ->
    throw({exmpp_caps, hash_caps, 'String : string()'}).
