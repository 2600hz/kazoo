%%%-------------------------------------------------------------------
%%% File    : openid.hrl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : OpenID-related record definitions
%%%
%%% Created : 18 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).
-define(XRI_GCTX_SYMBOLS, [$=, $@, $+, $$, $!, $(]).

-record(openid_xrdservice, {
  types,
  uris,
  localID
}).

-record(openid_xrds, {
  origID, 
  claimedID, 
  canonicalID, 
  isXRI, 
  services
}).

-record(openid_authreq, {
   opURLs,
   version,
   claimedID=none,
   localID=none,
   assoc=none
}).

-record(openid_assoc, {
  handle,
  created,
  expiresIn,
  servPublic,
  mac
}).
