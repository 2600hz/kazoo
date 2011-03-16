.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS = indexer_checkpoint  indexer_filenames_dets indexer_porter\
       indexer_words indexer_dir_crawler indexer_server\
       indexer indexer_misc indexer_trigrams

ERL = erl -boot start_clean 

compile: ${MODS:%=%.beam} trigramsS.tab
	@echo "make clean - clean up"

all: compile 

trigramsS.tab: ../354984si.ngl.gz indexer_trigrams.beam
	@erl -noshell -boot start_clean -s indexer_trigrams make_tables\
                                        -s init stop

clean:	
	rm -rf *.beam erl_crash.dump 
	rm -rf trigramsS.tab 

