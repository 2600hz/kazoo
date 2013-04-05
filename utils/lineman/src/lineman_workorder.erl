%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%------------------------------------------------------------------
-module(lineman_workorder).

-export([empty/0]).
-export([read/1]).
-export([toolbag/1]).
-export([name/1]).
-export([start/1]).
-export([sequences/1]).
-export([sequence_order/1]).
-export([sequence_rate/1]).
-export([sequence_period/1]).
-export([max_sequence_executions/1]).
-export([max_running_sequences/1]).
-export([running_sequences/1]).
-export([add_running_sequence/3]).
-export([remove_running_sequence/3]).
-export([display_period/1]).
-export([completed_sequences/1]).
-export([failed_sequences/1]).

-include("lineman.hrl").

-record(workorder, {workorder
                    ,file
                    ,start = now()
                    ,name = <<"Unknown">>
                    ,display_period = 2000
                    ,sequence_rate = 1
                    ,sequence_period = 1000
                    ,max_running_sequences = 100
                    ,max_sequence_executions = 10
                    ,deadcall_wait = 5000
                    ,sequence_order = simultanous
                    ,running_sequences = sets:new()
                    ,toolbag = []
                    ,sequences = []
                    ,completed_sequences = 0
                    ,failed_sequences = 0
                   }).

-opaque workorder() :: #workorder{}.
-export_type([workorder/0]).

-spec empty/0 :: () -> workorder().
empty() ->
    #workorder{}.

-spec read/1 :: (string()) -> {'ok', xml_el() | xml_els()} | {'error', atom()}.
read(File) ->
    try xmerl_scan:file(File) of
        {error, _}=E -> E;
        {Xml, []} -> 
            {ok, populate_workorder(#workorder{file=File}, Xml)};
        {_, _} -> {error, non_xml_content}
    catch
        _:_ -> {error, parse_failed}
    end.

-spec name/1 :: (workorder()) -> ne_binary().
name(#workorder{name=Name}) ->
    Name.

-spec toolbag/1 :: (workorder()) -> xml_el() | xml_els().
toolbag(#workorder{toolbag=Toolbag}) ->
    Toolbag.

-spec start/1 :: (workorder()) -> ne_binary().
start(#workorder{start=Start}) ->
    Start.

-spec sequences/1 :: (workorder()) -> xml_el() | xml_els().
sequences(#workorder{sequences=Sequences}) ->
    Sequences.

-spec sequence_order/1 :: (workorder()) -> ne_binary().
sequence_order(#workorder{sequence_order=SequenceOrder}) ->
    SequenceOrder.

-spec sequence_rate/1 :: (workorder()) -> pos_integer().
sequence_rate(#workorder{sequence_rate=SequenceRate}) ->
    SequenceRate.

-spec sequence_period/1 :: (workorder()) -> pos_integer().
sequence_period(#workorder{sequence_period=SequencePeriod}) ->
    SequencePeriod.

-spec max_sequence_executions/1 :: (workorder()) -> non_neg_integer().
max_sequence_executions(#workorder{max_sequence_executions=Max}) ->
    Max.

-spec max_running_sequences/1 :: (workorder()) -> non_neg_integer().
max_running_sequences(#workorder{max_running_sequences=Max}) ->
    Max.

-spec running_sequences/1 :: (workorder()) -> non_neg_integer().
running_sequences(#workorder{running_sequences=Running}) ->
    sets:size(Running).

-spec add_running_sequence/3 :: (pid(), list(), workorder()) -> workorder.
add_running_sequence(Pid, Sequences, #workorder{running_sequences=Running}=Workorder) ->
    Workorder#workorder{running_sequences=sets:add_element(Pid, Running), sequences=Sequences}.

-spec remove_running_sequence/3 :: (pid(), boolean(), workorder()) -> workorder.
remove_running_sequence(Pid, true, #workorder{running_sequences=Running, completed_sequences=Completed}=Workorder) ->
    Workorder#workorder{running_sequences=sets:del_element(Pid, Running), completed_sequences=Completed + 1};
remove_running_sequence(Pid, false, #workorder{running_sequences=Running, failed_sequences=Failed}=Workorder) ->
    Workorder#workorder{running_sequences=sets:del_element(Pid, Running), failed_sequences=Failed + 1}.

-spec display_period/1 :: (workorder()) -> pos_integer().
display_period(#workorder{display_period=DisplayPeriod}) ->
    DisplayPeriod.

-spec completed_sequences/1 :: (workorder()) -> non_neg_integer().
completed_sequences(#workorder{completed_sequences=Completed}) ->
    Completed.

-spec failed_sequences/1 :: (workorder()) -> non_neg_integer().
failed_sequences(#workorder{failed_sequences=Failed}) ->
    Failed.

-spec populate_workorder/2 :: (workorder(), xml_el() | xml_els()) -> workorder().
populate_workorder(#workorder{file=File}=Workorder, Xml) ->
    Workorder#workorder{workorder=Xml
                        ,name = lineman_util:xml_binary_value("/workorder/parameters/name", Xml, File)
                        ,max_running_sequences = lineman_util:xml_integer_value("/workorder/parameters/max-running-sequences", Xml, 100)
                        ,max_sequence_executions = lineman_util:xml_integer_value("/workorder/parameters/max-sequence-executions", Xml, 100)
                        ,deadcall_wait = lineman_util:xml_integer_value("/workorder/parameters/deadcall-wait", Xml, 5000)
                        ,sequence_order = lineman_util:xml_binary_value("/workorder/parameters/sequence-order", Xml, <<"sequential">>)
                        ,sequence_rate = lineman_util:xml_integer_value("/workorder/parameters/sequence-rate", Xml, 1)
                        ,sequence_period = lineman_util:xml_integer_value("/workorder/parameters/sequence-period", Xml, 1000)
                        ,display_period = lineman_util:xml_integer_value("/workorder/parameters/display-period", Xml, 2000)
                        ,file = File
                        ,toolbag = xmerl_xpath:string("/workorder/toolbag/*", Xml)
                        ,sequences = xmerl_xpath:string("/workorder/sequences/*", Xml)
                       }.
