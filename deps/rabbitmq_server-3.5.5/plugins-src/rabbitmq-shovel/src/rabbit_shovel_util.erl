%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_shovel_util).

-export([update_headers/5]).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

-define(ROUTING_HEADER, <<"x-shovelled">>).

update_headers(Prefix, Suffix, SrcURI, DestURI,
               Props = #'P_basic'{headers = Headers}) ->
    Table = Prefix ++ [{<<"src-uri">>,  SrcURI},
                       {<<"dest-uri">>, DestURI}] ++ Suffix,
    Headers2 = rabbit_basic:prepend_table_header(
                 ?ROUTING_HEADER, [{K, longstr, V} || {K, V} <- Table],
                 Headers),
    Props#'P_basic'{headers = Headers2}.
