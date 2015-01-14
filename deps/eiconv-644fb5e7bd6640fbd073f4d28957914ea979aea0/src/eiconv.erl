%%
%%
%%

-module(eiconv).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([open/2, conv/2, close/1]).

% easy api one shot convert api
-export([convert/2, convert/3]).

-on_load(init/0).

% @doc Load the nif
%
init() ->
    ok = erlang:load_nif(code:priv_dir(eiconv) ++ "/eiconv_nif", 0).

% @doc Open a new encoder which can be used to convert text from FromCode into ToCode.
%
open(_ToCode, _FromCode) ->
    exit(nif_library_not_loaded).

% @doc Convert Input into the requested encoding.
%
conv(_Cd, _Input) ->
    exit(nif_library_not_loaded).

% @doc Close the encoder - dummy function, close will be done by the garbage collector.
%
close(_Cd) ->
    ok.


% @doc Convert input FromEncoding to utf-8
%
convert(FromEncoding, Input) ->
    convert(FromEncoding, "utf-8", Input).

% @doc Convert input which is in FromEncoding to ToEncoding.
%
convert(FromEncoding, ToEncoding, Input) ->
    case open(ToEncoding, FromEncoding) of
        {ok, Cd} ->
            conv(Cd, Input);
        {error, _}=Error ->
            Error
    end.

