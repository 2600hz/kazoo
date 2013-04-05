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
-export([run_sequence_steps/2]).
-export([prepare_tools/2]).

-include("lineman.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link/2 :: (xml_el(), lineman_workorder:workorder()) -> pid().
start_link(Sequence, Workorder) ->
    spawn_link(?MODULE, run_sequence_steps, [Sequence, Workorder]).

-spec run_sequence_steps/2 :: (xml_el(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
run_sequence_steps(Sequence, Workorder) ->
    
    Steps = [fun(W) -> prepare_tools(Sequence, W) end
             ,fun(W) -> execute_tools(Sequence, W) end
            ],
    lists:foldl(fun(F, W) -> F(W) end, Workorder, Steps).

-spec prepare_tools/2 :: (xml_el(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
prepare_tools(#xmlElement{content=Content}, Workorder) ->
    lists:foldl(fun(#xmlElement{}=Xml, W) ->
                        {xmlObj, string, Tool} = xmerl_xpath:string("name()", Xml),
                        lineman_toolbag_sup:prepare(Tool, Xml, W);
                   (_, W) -> W
                end, Workorder, Content).

-spec execute_tools/2 :: (xml_el(), lineman_workorder:workorder()) -> lineman_workorder:workorder().
execute_tools(#xmlElement{content=Content}, Workorder) ->
    lists:foldl(fun(#xmlElement{}=Xml, W) ->
                        {xmlObj, string, Tool} = xmerl_xpath:string("name()", Xml),
                        lineman_toolbag_sup:execute(Tool, Xml, W);
                   (_, W) -> W
                end, Workorder, Content).
