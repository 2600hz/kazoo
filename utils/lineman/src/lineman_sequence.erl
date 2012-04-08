%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_sequence).

-export([start_link/2]).
-export([run_sequence_steps/1]).
-export([prepare_tools/1]).

-include_lib("lineman/src/lineman.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link(_Parent, #xmlElement{content=Steps}) ->
    spawn_link(?MODULE, run_sequence_steps, [Steps]).
                       
run_sequence_steps(Steps) ->
    prepare_tools(Steps),
    timer:sleep(30000).

prepare_tools([#xmlElement{}=Step|Steps]) ->
    {xmlObj, string, Tool} = xmerl_xpath:string("name()", Step),
    lineman_toolbag_sup:prepare(Tool, Step),
    prepare_tools(Steps);
prepare_tools([_|Steps]) ->
    prepare_tools(Steps);
prepare_tools([]) -> ok.
