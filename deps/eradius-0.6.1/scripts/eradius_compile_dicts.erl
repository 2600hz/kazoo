#!/usr/bin/env escript
-include("../include/eradius_dict.hrl").

-define(PRIV_DIR, "priv").
-define(PRIV_DICTS_DIR, "priv/dictionaries").
-define(INCLUDE_DIR, "include").
-define(CURR_DIR, ".").

-define(IN_DIR, ?PRIV_DICTS_DIR).
-define(OUT_DIR, ?INCLUDE_DIR).

main([String]) ->
    try
        run(String)
    catch
    _:_ ->
        erlang:display(erlang:get_stacktrace())
    end.

get_all_dict_files() ->
    FunGenName = fun (F) -> re:replace(F, "\\.", "_", [{return, list}]) end,
    [{filename:join(?IN_DIR, F), F, filename:join(?OUT_DIR, FunGenName(F)), FunGenName(F)} 
        || F <- filelib:wildcard("*", ?IN_DIR)].

run("build_dicts") ->
    lists:foreach(fun ({InFQN, _InF, _OutFQN, OutF}) -> mk_dict(InFQN, OutF) end, get_all_dict_files());
run("clean_dicts") ->
    filelib:fold_files(?PRIV_DIR, "\\.map$", false, fun(F, _) -> file:delete(F) end, []),
    filelib:fold_files("priv/couchdb/dicts", "\\.json$", false, fun(F, _) -> file:delete(F) end, []),
    filelib:fold_files(?INCLUDE_DIR, "dictionary.*\\.hrl", false, fun(F, _) -> file:delete(F) end, []).

%%% --------------------------------------------------------------------
%%% Dictionary making
%%% --------------------------------------------------------------------

mk_dict(InFile, OutFile) ->
    Res = parse_dict(InFile),
    mk_outfiles(Res, ?CURR_DIR, o2u(OutFile)).

mk_outfiles(Res, Dir, File) ->
    {Hrl, Map, Json} = open_files(Dir, File),
    io:format(Json, "[\n", []),
    emit(Res, Hrl, Map, Json),
    io:format(Json, "]\n", []),
    close_files(Hrl, Map).

add_comma([_|_]) ->
    ',';
add_comma([_]) ->
    ',';
add_comma([]) ->
    "".

emit([A|T], Hrl, Map, Json) when is_record(A, attribute) ->
    io:format(Hrl, "-define( ~s , ~w ).~n",
	      [d2u(A#attribute.name), A#attribute.id]),
    io:format(Map, "~w.~n", [A]),
    case A#attribute.id of
        {Vid, Id} ->
            io:format(Json, "  {\"attr\": {\"vid\":~w, \"id\":~w, \"type\":\"~p\", \"name\":~p, \"enc\":\"~p\"}}~s~n",
                [Vid, Id, A#attribute.type, d2u(A#attribute.name), A#attribute.enc, add_comma(T)]);
        Id ->
            io:format(Json, "  {\"attr\": {\"id\":~w, \"type\":\"~p\", \"name\":~p, \"enc\":\"~p\"}}~s~n",
                [Id, A#attribute.type, d2u(A#attribute.name), A#attribute.enc, add_comma(T)])
    end,
    emit(T, Hrl, Map, Json);

emit([V|T], Hrl, Map, Json) when is_record(V, vendor) ->
    io:format(Hrl, "-define( ~s , ~w ).~n",
	      [d2u(V#vendor.name), V#vendor.type]),
    io:format(Map, "~w.~n", [V]),
    io:format(Json, "  {\"vendor\": {~p:~w}}~s~n", [d2u(V#vendor.name), V#vendor.type, add_comma(T)]),
    emit(T, Hrl, Map, Json);
emit([V|T], Hrl, Map, Json) when is_record(V, value) ->
    io:format(Map, "~w.~n", [V]),
    case V#value.id of
        {{Vid, Id}, Val} ->
            io:format(Json, "  {\"val\": {\"vid\":~w, \"id\":~w, \"val\":~w, \"name\":~p}}~s~n", [Vid, Id, Val, V#value.name, add_comma(T)]);
        {Id, Val} ->
            io:format(Json, "  {\"val\": {\"id\":~w, \"val\":~w, \"name\":~p}}~s~n", [Id, Val, V#value.name, add_comma(T)])
    end,
    emit(T, Hrl, Map, Json);
emit([_|T], Hrl, Map, Json) ->
    emit(T, Hrl, Map, Json);
emit([], _, _, _) ->
    true.

open_files(Dir, File) ->
    [Name|_] = lists:reverse(string:tokens(File, "/")),
    Hfile = Dir ++ "/include/" ++ Name ++ ".hrl",
    {ok,Hrl} = file:open(Hfile, [write]),
    Mfile = Dir ++ "/priv/" ++ Name ++ ".map",
    {ok,Map} = file:open(Mfile, [write]),
    JSfile = Dir ++ "/priv/couchdb/dicts/" ++ Name ++ ".json",
    {ok,Json} = file:open(JSfile, [write]),
    io:format("Creating files: ~n  <~s>~n  <~s>~n  <~s>~n", [Hfile, Mfile, JSfile]),
    {Hrl, Map, Json}.

close_files(Hrl, Map) ->
    file:close(Hrl),
    file:close(Map).

parse_dict(File) when is_list(File) ->
    {ok,B} = file:read_file(File),
    F = fun(Line,{undefined = Vendor, AccList}) ->
                case pd(string:tokens(Line,"\s\t\r")) of
                    {ok,E} -> {Vendor, [E|AccList]};
                    {begin_vendor, VendId} -> {{vendor, VendId}, AccList};
                    _      -> {Vendor, AccList}
                end;
           (Line, {{vendor, VendId} = Vendor, AccList}) ->
                case pd(string:tokens(Line, "\s\t\r"), VendId) of
                    {end_vendor} -> {undefined, AccList};
                    {ok,E} -> {Vendor, [E|AccList]};
                    _ -> {Vendor, AccList}
                end
	end,
    {_, L} = lists:foldl(F,{undefined, []},string:tokens(b2l(B),"\n")),
    L.

paa(Attr = #attribute{type = integer}, ["has_tag"]) ->
    Attr#attribute{ type = {tagged, integer24} };
paa(Attr, ["has_tag"]) ->
    Attr#attribute{ type = {tagged, Attr#attribute.type} };

paa(Attr, ["encrypt", Enc]) ->
    case l2i(Enc) of
	1 -> Attr#attribute{ enc = scramble };
	2 -> Attr#attribute{ enc = salt_crypt };
	_ -> Attr
    end;

paa(Attr, [Vendor]) ->
    case get({vendor, Vendor}) of
	undefined -> Attr;
	VendId -> Attr#attribute{ id = {VendId, Attr#attribute.id} }
    end.

parse_attribute_attrs(Attr, []) ->
    Attr;

parse_attribute_attrs(Attr, [ [$# | _ ] | _]) ->
    Attr;

parse_attribute_attrs(Attr, [Attribute|Tail]) ->
    [Token | Rest] = string:tokens(Attribute, ","),
    NewAttr = paa(Attr, string:tokens(Token, "=")),
    parse_attribute_attrs(NewAttr, Rest ++ Tail).

pd(["BEGIN-VENDOR", Name]) ->
    case get({vendor, Name}) of
	undefined -> {begin_vendor, Name};
	VendId -> {begin_vendor, VendId}
    end;

pd(["VENDOR", Name, Id]) ->
    put({vendor,Name}, l2i(Id)),
    {ok, #vendor{type = l2i(Id), name = Name}};

pd(["ATTRIBUTE", Name, Id, Type | Tail]) ->
    Attr = parse_attribute_attrs(#attribute{name = Name, id = id2i(Id), type = l2a(Type)}, Tail),
    put({attribute, Attr#attribute.name}, Attr#attribute.id),
    {ok, Attr};

pd(["VALUE", Attr, Name, Id]) ->
    case get({attribute, Attr}) of
	undefined ->
	    io:format("missing: ~p~n", [Attr]),
	    false;
	AttrId ->
	    {ok,#value{id = {AttrId, id2i(Id)}, name = Name}}
    end;
pd(_X) ->
    %%io:format("Skipping: ~p~n", [X]),
    false.

pd(["END-VENDOR", _Name], _VendId) ->
    {end_vendor};

pd(["ATTRIBUTE", Name, Id, Type | Tail], VendId) ->
    Attr = parse_attribute_attrs(#attribute{name = Name, id = {VendId, id2i(Id)}, type = l2a(Type)}, Tail),
    put({attribute, Attr#attribute.name}, Attr#attribute.id),
    {ok, Attr};

pd(["VALUE", Attr, Name, Id], _VendId) ->
    case get({attribute, Attr}) of
	undefined ->
	    io:format("missing: ~p~n", [Attr]),
	    false;
	AttrId ->
	    {ok,#value{id = {AttrId, id2i(Id)}, name = Name}}
    end;
pd(_X, _VendId) ->
    false.

id2i(Id) ->
    case catch l2i(Id) of
	I when is_integer(I) -> I;
	{'EXIT', _} ->
	    hex2i(Id)
    end.

hex2i("0x" ++ L) -> erlang:list_to_integer(L, 16).

b2l(B) when is_binary(B) -> binary_to_list(B);
b2l(L) when is_list(L)   -> L.

l2i(L) when is_list(L)    -> list_to_integer(L);
l2i(I) when is_integer(I) -> I.

l2a(L) when is_list(L) -> list_to_atom(L);
l2a(A) when is_atom(A) -> A.

%%% Replace all dashes with underscore characters.
d2u(L) when is_list(L) ->
    repl(L, $-, $_).

%%% Replace all dots with underscore characters.
o2u(L) when is_list(L) ->
    repl(L, $., $_).

repl(L,X,Y) when is_list(L) ->
    F = fun(Z) when Z == X -> Y;
	   (C) -> C
	end,
    lists:map(F,L).
