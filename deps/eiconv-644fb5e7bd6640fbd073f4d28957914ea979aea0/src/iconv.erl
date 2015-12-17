%%
%%
%%

-module(iconv).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([open/2, conv/2, close/1]).

open(ToCode, FromCode) -> 
    eiconv:open(ToCode, FromCode).

conv(Cd, Input) -> 
    eiconv:conv(Cd, Input).

close(Cd) -> 
    eiconv:close(Cd).
