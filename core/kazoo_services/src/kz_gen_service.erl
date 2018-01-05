%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz, INC
%%% @doc
%%% A generic Kazoo service module
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(kz_gen_service).

-callback reconcile(kz_services:services()) ->
    kz_services:services().
