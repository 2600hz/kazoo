-module(sources_parser_unittests).

-export([run_tests/0]).

tests() ->
    [
	{"trans", [
                    {"block with no trans", <<"<html>{% block main %} {% endblock %}</html>">>, []},

                    {"block with trans", <<"<html>{% block main %} {% trans \"Hello\" %} {% endblock %}</html>">>, [{"Hello",{"dummy_path",1,33}}]},

                    {"for with trans", <<"<html>{% block main %} {%for thing in things %}{% trans \"Hello inside a for\" %}  {% endfor %} {% endblock %}</html>">>, [{"Hello inside a for",{"dummy_path",1,57}}]},

		    {"if with trans", <<"<html>{% block content %}{% if thing %} {% trans \"Hello inside an if\" %} {% endif %} {% endblock %}</html>">>, [{"Hello inside an if",{"dummy_path",1,50}}]},

		    {"if with trans inside a for", <<"<html>{% block content %}{%for thin in things %}{% if thing %} {% trans \"Hello inside an if inside a for\" %} {% endif %} {% endfor %}{% endblock %}</html>">>, [{"Hello inside an if inside a for",{"dummy_path",1,73}}]},

		    {"if and else both with trans", <<"<html>{% block content %}{% if thing %} {% trans \"Hello inside an if\" %} {% else %} {% trans \"Hello inside an else\" %} {% endif %} {% endblock %}</html>">>, [ {"Hello inside an else",{"dummy_path",1,94}}, {"Hello inside an if",{"dummy_path",1,50}}]}  
	]}
    ].

run_tests() ->
    io:format("Running unit tests...~n"),
    Failures = lists:foldl(
        fun({Group, Assertions}, GroupAcc) ->
                io:format(" Test group ~p...~n", [Group]),
                lists:foldl(fun({Name, Content, Output}, Acc) -> 
				process_unit_test(Content, Output, Acc, Group, Name)                              
                            end, GroupAcc, Assertions)
        end, [], tests()),
    
    io:format("Unit test failures: ~p~n", [Failures]).

process_unit_test(Content, Output,Acc, Group, Name) ->
	Tokens = sources_parser:process_content("dummy_path", Content),
	%%io:format("Tokens are: ~p~n", [Tokens]),
	case Tokens of
		Output -> Acc;
		_ -> 	[{Group, Name, 'binary', {expected, Output}, {found, Tokens} } | Acc]
	end.
