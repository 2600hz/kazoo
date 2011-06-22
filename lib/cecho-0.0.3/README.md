cecho
=====
An ncurses library for Erlang

Introduction
------------
Cecho is an ncurses library for Erlang which enabled Erlang applications to create terminal based GUIs. It aims to have an API as close as possible to the original API of ncurses so that a person familiar with ncurses API should be able to immediately use it without any introduction.

Usage
-----
The intention of this library is to have an API as close as possible to the ncurses library. Something things can't be translated and other things return values instead of taking pointer arguments etc. but overall the API should be similar enough to make use of the ncurses documentation that is out on the web.

The following sequence is more or less standard when using the library:
1) Start the cecho application
2) Set flags and attributes, create windows and initialize data
3) Move cursor, add text and refresh window
3.1) Listen for input to be able to terminate the program
4) Stop the cecho application gracefully

The following is an example hello world which bounces "Hello world" across the screen and terminates when 'q' is pressed. It can be found in the cecho_example.erl file in the src/ directory.

    %%
    %% helloworld - bounce "Hello World!" on the end of the screen
    %%
    helloworld() ->
        %% Start application
    	application:start(cecho),
    	%% Set attributes
    	cecho:cbreak(),
    	cecho:noecho(),
    	cecho:curs_set(?ceCURS_INVISIBLE),
    	%% Write initial string...
    	cecho:mvaddstr(0, 0, "Hello World!"),
    	cecho:refresh(),
    	%% Start the process that will "move" the string
    	Mover = spawn(fun() -> mvhello() end),
    	ctrl(Mover).

    ctrl(Mover) ->
        %% get key-input
    	C = cecho:getch(),
    	case C of
	    $q -> 
	        %% If we get a 'q' then exit the mover and stop cecho
	    	exit(Mover, normal),
	    	application:stop(cecho),
	    	erlang:halt();
	    _ ->
	        %% ignore anything else
	        ctrl(Mover)
        end.

    %% start the mover
    mvhello() -> mvhello(0, 0, 1, 1).

    %% take previous pos and direction and print out new string
    mvhello(PrevY, PrevX, DirY, DirX) ->
        %% "erase" previous position
    	cecho:mvaddstr(PrevY, PrevX, "            "),
    	%% calculate new position and direction
    	{NewY, NewX, NewDirY, NewDirX} =
	    calc_new_pos(PrevY, PrevX, DirY, DirX),
    	%% "move" the text to new position
    	cecho:mvaddstr(NewY, NewX, "Hello World!"),
    	%% update the screen to show the change
    	cecho:refresh(),
    	%% do it again!
    	timer:sleep(50),
   	 mvhello(NewY, NewX, NewDirY, NewDirX).

    calc_new_pos(Py, Px, Dy, Dx) ->
        %% get max coords of the screen
        {My, Mx} = cecho:getmaxyx(),
        %% calc new vertical position and new direction
        {NewPy, NewDy} =
	    if (Py+(Dy) >= My) orelse (Py+(Dy) < 0) ->
	           {Py+(Dy*-1), Dy*-1};
	       true ->
	           {Py+(Dy), Dy}
            end,
        %% calc new horizontal position and new direction
    	%% take string length into account
    	{NewPx, NewDx} =
	    if (Px+(Dx)+12 >= Mx) orelse (Px+(Dx) < 0) ->
	           {Px+(Dx*-1), Dx*-1};
	       true ->
                   {Px+(Dx), Dx}
	    end,
        {NewPy, NewPx, NewDy, NewDx}.

There are several ways to run the examples and own scripts/app with cecho. There are essentially two _intended_ ways which are recommend; either running the whole thing as a script or from a module using the '-eval' flag to erl.

Examples on how to run the helloworld example (make sure you have compiled cecho first):

From the cecho directory do:

    $> erl -noinput -pa ../cecho/ebin/ -eval 'cecho_example:helloworld()' +A 50

Or create an escript file as follows:
   
    #!/usr/bin/env escript
    %%! -noinput -pa ../cecho/ebin +A 50
    -include_lib("cecho/include/cecho.hrl").
    main(_) -> cecho_example:helloworld().

and save it, then make it executable and run it from the cecho directory:

    $> chmod +x ./helloworld.escript
    $> ./helloworld.escript

IMPORTANT:
If input will be used (cecho:getch/0) then the two flags '-noinput' and +A <N> _must_ be used. The reason is that if '-noinput' is not specified the erlang VM will interfere with the reading of the keys. The second reason is that in order to asynchronously read the input Erlang need more io threads; however it doesn't seem to be enough to specify e.g. 10 in some cases and the reason is not known. If a "high enough number" is specified then the input will work very well.


Contribute
----------
Should you find yourself using cecho and have issues, comments or feedback please [create an issue!] [1]

[1]: http://github.com/mazenharake/cecho/issues "cecho issues"
