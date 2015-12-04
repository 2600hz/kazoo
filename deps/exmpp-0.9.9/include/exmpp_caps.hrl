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


% Records to represent Caps.
%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type ns() = string() | binary()

-type(ns() :: string() | binary()).


%% @type value() = string() | binary()

-type(value() :: string() | binary()).


%% @type xvar() = string()

-type(xvar() :: string()).


%% @type field() = {field, Var, Values}
%%     Var    = xvar()
%%     Values = [value()].
%% Record representing a field.

-record(field,
	{
	  var    = "" :: xvar(),
	  values = [] :: [value()]
			 }).

-type(field() :: #field{}).


%% @type formtype() = string()

-type(formtype() :: string()).


%% @type form() = {form, Type, Fields}
%%     Type   = formtype()
%%     Fields = field() | [field()].
%% Record representing a form.

-record(form,
	{
	  type   = "" :: formtype(),
	  fields = [] :: field() | [field()]
			 }).

-type(form() :: #form{}).


%% @type identitytype() = string()

-type(identitytype() :: string()).


%% @type identitycategory() = string()

-type(identitycategory() :: string()).


%% @type identitylang() = string()

-type(identitylang() :: string()).


%% @type identityname() = string()

-type(identityname() :: string()).


%% @type identity() = {identity, Category, Type, Lang, Name}
%%     Category = identitycategory()
%%     Type     = identitytype()
%%     Lang     = identitylang()
%%     Name     = identityname().
%% Record representing an identity.

-record(identity,
	{
	  category = "" :: identitycategory(),
	  type     = "" :: identitytype(),
	  lang     = "" :: identitylang(),
	  name     = "" :: identityname()
			   }).

-type(identity() :: #identity{}).


%% @type ecaps() = {ecaps, Identities, Features, Forms}
%%     Identities = identity() | [identity()]
%%     Features   = [ns()]     | []
%%     Forms      = form()     | [form()] | [].
%% Record representing a Caps.

-record(ecaps,
	{
	  identities = [] :: identity() | [identity()],
	  features   = [] :: [ns()],
	  forms      = [] :: form()     | [form()]
			     }).

-type(ecaps() :: #ecaps{}).


%% @type hash() = binary()

-type(hash() :: binary()).
