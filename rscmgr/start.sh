#!/bin/sh


cd `dirname $0`
exec erl -heart -detached -set_cookie ClueCon -pa $PWD/ebin -pa $PWD/deps/*/ebin -boot start_sasl -name rscmgr -s rscmgr
