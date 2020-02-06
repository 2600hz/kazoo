%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_pipe).

%% Accessors
-export([failed/1, set_failed/2, set_failed/3
        ,options/1, set_options/2
        ,quotes/1, set_quotes/2
        ,succeeded/1, set_succeeded/2
        ,todo/1, set_todo/2

        ,id/1
        ,add_succeeded/2
        ]).

%% Monads
-export([do/2
        ,pipe/2
        ]).

%% Monad Utilities
-export([attempt/2
        ,new/2, new/3, new/4
        ,merge_okkos/1, merge_okkos/2
        ,to_json/1
        ]).

-include("knm.hrl").

%% {{{ Type definitions
-type success() :: knm_phone_number:record().
-type succeeded() :: [success()].

-type reason() :: knm_errors:error() | atom().
-type reasons() :: [reason()].
-type failed() :: #{kz_term:ne_binary() => reason()}.

-type set_failed() :: kz_term:ne_binary() |
                      kz_term:ne_binaries() |
                      knm_phone_number:record() |
                      knm_phone_number:records().

-type quotes() :: kz_term:api_objects().

-type collection() :: collection(succeeded()).
-type collection(Succeeded) :: collection(Succeeded, Succeeded).
-type collection(Succeeded, TODOs) :: #{'failed' => failed()
                                       ,'options' => knm_number_options:options()
                                       ,'quotes' => quotes() %% defined in knm_phone_number.hrl
                                       ,'succeeded' => Succeeded
                                       ,'todo' => kz_term:ne_binary() | TODOs
                                       }.

-type applier() :: fun((collection()) -> collection()).
-type appliers() :: [applier()].
%% }}}

-export_type([collection/0
             ,failed/0
             ,quotes/0
             ,reason/0, reasons/0
             ,success/0, succeeded/0
             ]).

%%%=============================================================================
%%% Accessors
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec failed(collection()) -> failed().
failed(#{'failed' := Failed}) -> Failed.

-spec set_failed(collection(), failed()) -> collection().
set_failed(Collection, Failed) -> Collection#{'failed' => Failed}.

-spec set_failed(collection(), set_failed(), reason()) -> collection().
set_failed(Collection, <<Num/binary>>, Reason) ->
    lager:debug("number ~s error: ~p", [Num, Reason]),
    FailedAcc = maps:get('failed', Collection),
    Collection#{'failed' => FailedAcc#{Num => Reason}};
set_failed(Collection0, Nums, Reason) when is_list(Nums) ->
    F = fun (Num, ColAcc) -> set_failed(ColAcc, Num, Reason) end,
    lists:foldl(F, Collection0, Nums);
set_failed(Collection, PN, Reason) ->
    Num = knm_phone_number:number(PN),
    ?LOG_DEBUG("number ~s state: ~s", [Num, knm_phone_number:state(PN)]),
    set_failed(Collection, Num, Reason).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec options(collection()) -> knm_number_options:options().
options(#{'options' := Options}) -> Options.

-spec set_options(collection(), knm_number_options:options()) -> collection().
set_options(Collection, Options) -> Collection#{'options' => Options}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec quotes(collection()) -> quotes().
quotes(#{'quotes' := Quotes}) -> Quotes.

-spec set_quotes(collection(), quotes()) -> collection().
set_quotes(Collection, Quotes) -> Collection#{'quotes' => Quotes}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec succeeded(collection()) -> succeeded().
succeeded(#{'succeeded' := Succeeded}) -> Succeeded.

-spec set_succeeded(collection(), success() | succeeded()) -> collection().
set_succeeded(Collection, Succeeded) when is_list(Succeeded) -> Collection#{'succeeded' => Succeeded};
set_succeeded(Collection, Success) when not is_list(Success) ->
    Collection#{'succeeded' => [Success | maps:get('succeeded', Collection)]}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec todo(collection()) -> kz_term:ne_binaries() | succeeded().
todo(#{'todo' := ToDo}) -> ToDo.

-spec set_todo(collection(), kz_term:ne_binaries() | succeeded()) -> collection().
set_todo(Collection, ToDo) -> Collection#{'todo' => ToDo}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec add_succeeded(collection(), succeeded()) -> collection().
%% FIXME: unify with succeeded/2.
add_succeeded(Collection=#{'succeeded' := Succeeded}, Numbers) when is_list(Numbers) ->
    Collection#{'succeeded' => Numbers ++ Succeeded}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new(knm_number_options:options(), kz_term:ne_binaries()) ->
          collection(succeeded(), kz_term:ne_binaries()).
new(Options, ToDos) -> new(Options, ToDos, []).

-spec new(knm_number_options:options(), kz_term:ne_binaries(), kz_term:ne_binaries()) ->
          collection(succeeded(), kz_term:ne_binaries()).
new(Options, ToDos, FailedNums) -> new(Options, ToDos, FailedNums, 'not_reconcilable').

-spec new(knm_number_options:options(), kz_term:ne_binaries(), kz_term:ne_binaries(), reason()) ->
          collection(succeeded(), kz_term:ne_binaries()).
new(Options, ToDos, FailedNums, Reason) ->
    #{'failed' => maps:from_list([{Num, Reason} || Num <- FailedNums])
     ,'options' => Options
     ,'quotes' => 'undefined'
     ,'succeeded' => []
     ,'todo' => ToDos
     }.

%%------------------------------------------------------------------------------
%% @doc Apply something to `todo' if not empty.
%%
%% If empty use `ok' as the new `todo'.
%% If `ok' is empty, return.
%% @end
%%------------------------------------------------------------------------------
-spec pipe(collection(), appliers()) -> collection().
pipe(Collection, []) -> Collection;
pipe(Collection=#{'todo' := [], 'succeeded' := []}, _) -> Collection;
pipe(Collection=#{'todo' := [], 'succeeded' := NewToDos}, Fs) ->
    NewCollection = Collection#{'todo' => NewToDos, 'succeeded' => []},
    pipe(NewCollection, Fs);
pipe(Collection, [F|Fs]) ->
    case do(F, Collection) of
        NewCollection=#{'succeeded' := []} -> NewCollection;
        NewCollection -> pipe(NewCollection, Fs)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec do(applier(), collection()) -> collection().
do(_, #{'todo' := [], 'succeeded' := []}=Collection) -> Collection;
do(F, #{'todo' := [], 'succeeded' := NewToDos}=Collection) ->
    %% For calls not from pipe/2
    do(F, Collection#{'todo' => NewToDos, 'succeeded' => []});
do(F, Collection) ->
    lager:debug("applying ~p", [F]),
    NewCollection = F(Collection),
    NewCollection#{'todo' => []}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec merge_okkos(collection(), collection()) -> collection().
merge_okkos(#{'succeeded' := OKa, 'failed' := KOa}
           ,#{'succeeded' := OKb, 'failed' := KOb} = B) ->
    B#{'succeeded' => OKa ++ OKb
      ,'failed' => maps:merge(KOa, KOb)
      }.

-spec merge_okkos([collection()]) -> collection().
merge_okkos([]) ->
    lager:error("merge_okkos/1 argument is empty list"),
    throw({'error', 'internal_error'});
merge_okkos([T]) -> T;
merge_okkos([T0|Ts]) ->
    lists:foldl(fun merge_okkos/2, T0, Ts).

-spec id(collection()) -> collection().
id(T=#{'todo' := Todo}) -> set_succeeded(T, Todo).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_json(collection()) -> kz_json:object().
to_json(#{'succeeded' := PNs, 'failed' := Failed}) ->
    Successes = [{knm_phone_number:number(PN), knm_phone_number:to_public_json(PN)}
                 || PN <- PNs
                ],
    kz_json:from_list(
      props:filter_empty(
        [{<<"success">>, kz_json:from_list(Successes)}
        ,{<<"error">>, kz_json:from_map(Failed)}
        ])).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attempt(fun(), list()) ->
          knm_number:return() |
          {'ok', knm_phone_number:record()} |
          'true'.
attempt(Fun, Args) ->
    try apply(Fun, Args) of
        {'ok', _}=OK -> OK;
        {'error', _}=Error -> Error;
        'true' -> 'true';
        Resp ->
            case knm_phone_number:is_phone_number(Resp) of
                'true' -> {'ok', Resp};
                'false' ->
                    lager:error("invalid return in ~s:attempt/1: ~p", [?MODULE, Resp]),
                    {'error', knm_errors:to_json('internal_error', 'undefined', 'invalid_return_type')}
            end
    catch
        'throw':{'error', Reason} ->
            {'error', knm_errors:to_json(Reason)};
        'throw':{'error', Reason, Number} ->
            {'error', knm_errors:to_json(Reason, num_to_did(Number))};
        'throw':{'error', Reason, Number, Cause} ->
            {'error', knm_errors:to_json(Reason, num_to_did(Number), Cause)}
    end.

-spec num_to_did(kz_term:api_ne_binary() | knm_phone_number:record()) -> kz_term:api_ne_binary().
num_to_did('undefined') -> 'undefined';
num_to_did(?NE_BINARY = DID) -> DID;
num_to_did(PhoneNumber) -> knm_phone_number:number(PhoneNumber).
