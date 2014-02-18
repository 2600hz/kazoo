-module(erlydtl_example_variable_storage).
-compile(export_all).

% fake pmod

new(SomeVar) ->
    {?MODULE, SomeVar}.

some_var({?MODULE, SomeVar}) ->
    SomeVar.
