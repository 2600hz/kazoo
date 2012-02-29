# Copyright 2010-2011 Manolis Papadakis <manopapad@gmail.com>,
#                     Eirini Arvaniti <eirinibob@gmail.com>
#                 and Kostis Sagonas <kostis@cs.ntua.gr>
#
# This file is part of PropEr.
#
# PropEr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# PropEr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

# Author:      Manolis Papadakis
# Description: Instructions for make

.PHONY: default all compile dialyze check_escripts tests doc clean distclean rebuild retest

default: compile

all: compile doc

include/compile_flags.hrl:
	./write_compile_flags $@

compile:
	./rebar compile

dialyze: compile
	./rebar dialyze

check_escripts:
	./check_escripts.sh make_doc write_compile_flags

tests: compile
	./rebar eunit

doc:
	./make_doc

clean:
	./clean_temp.sh

distclean: clean
	rm include/compile_flags.hrl
	./rebar clean

rebuild: distclean include/compile_flags.hrl
	./rebar compile

retest: compile
	rm -rf .eunit
	./rebar eunit
