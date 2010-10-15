LIBDIR		= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
APP		= fs_toolkit
VERSION		= 0.0.1
CC  		= erlc
ERL     	= erl
EBIN		= ebin
CFLAGS  	= -I include -pa $(EBIN)
COMPILE		= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS 	= $(wildcard deps/*/ebin)

all: ebin compile
all_boot: all make_boot
start: all start_all

compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:		
	@echo Generating $(APP) documentation from srcs
	@erl -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt	
	
make_boot:
	(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts test_app)

start_all:
	(cd ebin; erl -pa ebin -noshell -sname test_app -boot test_app)

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script doc/*.html doc/*.css doc/erlang.png doc/edoc-info