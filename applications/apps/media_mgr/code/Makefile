.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = a b abc area_server_final allocator\
       benchmark_assoc benchmark_mk_assoc attrs chat_multi chat_socket\
       chat_cluster chat_file_transfer \
       chat_secure checker clock ctemplate\
       dist_demo edemo1 edemo2 ets_test event_handler extract fac fac1\
       geometry id3_v1 hello m1 upcase\
       id3_tag_lengths \
       lib_find lib_io_widget lib_files_find lib_lin\
       lib_filenames_dets lib_primes \
       lib_auth_cs  lib_md5 lib_misc\
       lib_rsa lib_tcp_server math1 math2 math3 my_fac_server name_server\
       name_server1\
       area_server area_server1 area_server0 motor_controller\
       mp3_manager my_bank  area_server1 area_server2\
       monitor1 monitor2 monitor3 my_alarm_handler\
       new_name_server broadcast convert1 convert2 convert3\
       convert4 convert5 mylists lists1\
       counter1 counter2 counter3 counter4 error1 cookbook_examples\
       misc mp3_sync phofs prime_server processes ptests registrar\
       sellaprime_app sellaprime_supervisor shop \
       scavenge_urls shop1 shop2 shop3 socket_examples shout stimer\
       server1 server2 server3 server4 server5\
       status test_mapreduce test_mnesia tracer_test try_test\
       udp_test update_binary_file update_file user_default vfs wordcount

ERL = erl -boot start_clean 

compile: ${MODS:%=%.beam} subdirs trigramsOS.tab
	@echo "make clean - clean up"

shoutcast: compile
	erl -s shout start

subdirs:
	cd socket_dist; make compile
	cd escript-4.1; make 

counter1.beam: counter1.erl
	erlc -W0 counter1.erl

m1.beam: m1.erl
	erlc -Ddebug m1.erl

all: compile 


trigramsOS.tab: 354984si.ngl.gz lib_trigrams.beam
	@erl -noshell -boot start_clean -s lib_trigrams make_tables\
                                        -s init stop

timer_tests:
	@erl -noshell -boot start_clean -s lib_trigrams timer_tests\
                                        -s init stop	
clean:	
	rm -rf *.beam lists.ebeam erl_crash.dump 
	rm -rf trigramsOS.tab trigramsS.tab trigrams.dict 
	cd ets_trigrams; make_clean
	cd socket_dist; make_clean

