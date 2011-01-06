%% Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(expanded_logging).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
	Forms1 = expand_forms(Forms),
	%io:format("forms: ~n~p~n", [Forms1]),
	Forms1.
    	
expand_forms({attribute,_,module,Module}=Attr) ->
	put(module, Module),
	Attr;
	
expand_forms({call,L,{remote,_,{atom,_,error_logger},{atom,_,MSG}},Args}=Form) when MSG==debug_msg; 
																					MSG==info_msg; 
																					MSG==warning_msg; 
																					MSG==error_msg; 
																					MSG==critical_msg ->
	case Args of
		[{string,_,_}=FormatStr] ->
			{call,L,{remote,L,{atom,L,log_roller_logger},{atom,L,do_log}},[
				{atom,L,MSG},{atom,L,get(module)},{atom,L,list_to_atom(lists:concat([get(module), '_logger']))},{integer,L,L},FormatStr,{nil,L}
			]};
		[{string,_,_}=FormatStr, Cons] ->
			{call,L,{remote,L,{atom,L,log_roller_logger},{atom,L,do_log}},[
				{atom,L,MSG},{atom,L,get(module)},{atom,L,list_to_atom(lists:concat([get(module), '_logger']))},{integer,L,L},FormatStr,Cons
			]};
		_ ->
			Form
	end;
	 
expand_forms({call,L,{remote,_,{atom,_,error_logger},{atom,_,REPORT}},Args}=Form) when REPORT==debug_report;
																					   REPORT==info_report; 
																					   REPORT==warning_report; 
																					   REPORT==error_report;
																					   REPORT==critical_report ->
	case Args of
		[A] ->
			{call,L,{remote,L,{atom,L,log_roller_logger},{atom,L,do_log}},[
				{atom,L,REPORT},{atom,L,get(module)},{atom,L,list_to_atom(lists:concat([get(module), '_logger']))},{integer,L,L},A
			]};
		_ ->
			Form
	end;
	
expand_forms(Forms) when is_list(Forms) ->
	[expand_forms(Form) || Form <- Forms];
	
expand_forms(Form) when is_tuple(Form) ->
	Expanded = [expand_forms(F) || F <- tuple_to_list(Form)],
	list_to_tuple(Expanded);
	
expand_forms(Form) -> 
	Form.