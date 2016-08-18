%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%% Uri Parse/Unparse
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kzsip_uri).

-include("kazoo_sip.hrl").

-export([uris/1
        ,ruri/1
        ,uri/1
        ]).

-export_type([uri/0, uris/0]).

-spec uris(binary() | string() | #uri{}) ->
                  [#uri{}] | error.
uris(Uri) -> nklib_parse_uri:uris(Uri).


-spec ruri(uri()) -> binary().
ruri(#uri{scheme='undefined'}=Uri) -> nklib_unparse:uri3(Uri#uri{scheme='sip'});
ruri(Uri) -> nklib_unparse:uri3(Uri).

-spec uri(uri()) -> binary().
uri(#uri{scheme='undefined'}=Uri) -> nklib_unparse:uri(Uri#uri{scheme='sip'});
uri(Uri) -> nklib_unparse:uri(Uri).
