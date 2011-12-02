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

%% @author Jean-Sebastien Pedron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides functions which
%% doesn't deserve a dedicated module.

-module(exmpp_utils).

%% Binary and string helpers.
-export([
	 any_to_list/1,
	 any_to_binary/1,
	 strip/1,
	 strip/2
	]).

%% Utils.
-export([
	 random_id/0,
	 random_id/1
	]).

%% --------------------------------------------------------------------
%% Binary and string helpers.
%% --------------------------------------------------------------------

%% @spec (Any) -> String
%%     Any = binary() | string() | atom() | integer()
%%     String = string()
%% @doc Convert any type to its `string()' form.
%%
%% For an atom, {@link erlang:atom_to_list/1} is used. For an integer,
%% {@link erlang:integer_to_list/1} is used. For a binary, {@link
%% erlang:binary_to_list/1} is used. A string is returned as is.

-spec(any_to_list/1 ::
      (binary() | string() | integer() | atom()) -> string()).

any_to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
any_to_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);

any_to_list(String) when is_list(String) ->
    String;
any_to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).

%% @spec (Any) -> Binary
%%     Any = binary() | string() | atom() | integer()
%%     Binary = binary()
%% @doc Convert any type to its `binary()' form.
%%
%% For an atom, {@link erlang:atom_to_list/1} is used. For an integer,
%% {@link erlang:integer_to_list/1} is used. For a string, {@link
%% erlang:list_to_binary/1} is used. A binary is returned as is.

-spec(any_to_binary/1 ::
      (binary() | string() | integer() | atom()) -> binary()).

any_to_binary(Atom) when is_atom(Atom) ->
    any_to_binary(atom_to_list(Atom));
any_to_binary(Integer) when is_integer(Integer) ->
    any_to_binary(integer_to_list(Integer));

any_to_binary(String) when is_list(String) ->
    list_to_binary(String);
any_to_binary(Binary) when is_binary(Binary) ->
    Binary.

%% @spec strip(Stream) -> Stripped
%%     Stream = binary() | string()
%%     Stripped = binary() | string()
%% @doc Strip leading and trailing blanks.
%%
%% @see strip/3.

-spec(strip/1 ::
      (binary()) -> binary();
      (string()) -> string()
		       ).

strip(Stream) ->
    strip(Stream, both).

%% @spec strip(Stream, Direction) -> Stripped
%%     Stream = binary() | string()
%%     Direction = left | right | both
%%     Stripped = binary() | string()
%% @doc Strip leading and/or trailing blanks, depending on the `Direction'.
%%
%% Blanks characters are `\s', `\t', `\n' and `\r'.
%%
%% The binary version was made by Christopher Faulet in his
%% <a href="http://www.capflam.org/?p=9">stream module</a>.
%%
%% @see strip/3.

-spec(strip/2 ::
      (binary(), left | right | both) -> binary();
      (string(), left | right | both) -> string()
					    ).

strip(Stream, left) ->
    strip_left(Stream);
strip(Stream, right) ->
    strip_right(Stream);
strip(Stream, both) ->
    strip_right(strip_left(Stream)).

strip_left(<<C:8, Rest/binary>>) when C == $\s; C == $\t; C == $\n; C == $\r ->
    strip_left(Rest);
strip_left([C | Rest]) when C == $\s; C == $\t; C == $\n; C == $\r ->
    strip_left(Rest);
strip_left(Stripped) ->
    Stripped.

strip_right(<<C:8, Rest/binary>>) when C == $\s; C == $\t; C == $\n; C == $\r ->
    case strip_right(Rest) of
        <<>> -> <<>>;
        T    -> <<C:8, T/binary>>
		    end;
strip_right(<<C:8, Rest/binary>>) ->
    T = strip_right(Rest),
    <<C:8, T/binary>>;
strip_right(<<>>) ->
    <<>>;
strip_right([C | Rest]) when C == $\s; C == $\t; C == $\n; C == $\r ->
    case strip_right(Rest) of
        [] -> [];
        T  -> [C | T]
    end;
strip_right([C | Rest]) ->
    [C | strip_right(Rest)];
strip_right([]) ->
    [].

%% --------------------------------------------------------------------
%% Utils.
%% --------------------------------------------------------------------

%% @spec () -> ID
%%     ID = string()
%% @doc Generate a random ID.
%%
%% Use the `exmpp' prefix.
%%
%% @see random_id/1.

-spec(random_id/0 :: () -> string()).

random_id() ->
    random_id("exmpp").

%% @spec (Prefix) -> ID
%%     Prefix = string()
%%     ID = string()
%% @doc Generate a random stanza ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.
%%
%% The ID is not guaranted to be unique.

-spec(random_id/1 :: (string() | undefined) -> string()).

random_id(undefined) ->
    integer_to_list(random:uniform(65536 * 65536));
random_id("") ->
    random_id(undefined);
random_id(Prefix) when is_atom(Prefix) ->
    random_id(atom_to_list(Prefix));
random_id(Prefix) when is_list(Prefix) ->
    Prefix ++ "-" ++ random_id(undefined).
