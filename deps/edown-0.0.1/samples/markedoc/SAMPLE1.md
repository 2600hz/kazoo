# SAMPLE 1: Emysql Readme of Jan 2011
```
 --------------------------------------------------------------
| THIS TEXT IS USED AS A SAMPLE TO ILLUSTRATE MARKEDOC USAGE.  |
| If you see this in your browser, you succeeded compiling it  |
| from markdown into an edoc. As you see it's quite complex    |
| and there is no 'cheating' involved.                         |
 --------------------------------------------------------------
'''

Erlang MySQL driver, based on a rewrite at Electronic Arts(tm). Supports prepared statements and stored procedures. For [samples][] and [docs][] see below.

While you can use mysql via ODBC, using a driver like Emysql should perform better. 

This is a continuation fork of [emysql][1] with [fixes][], [updates][], more [docs][] and [samples][]. [emysql][1] is a clean rewrite of [erlang-mysql-driver][2]. 

<hr/>

 **<<Which fork should I use?>>** See [history][].  
 **<<Who used this fork?>>** Electronic Arts.  
 **<<How do I ...?>>** See [samples][].  

 **Download:** <https://github.com/Eonblast/Emysql/archives/master>  
 **Repository:** <https://github.com/Eonblast/Emysql>  
 **Docs:** <http://eonblast.github.com/Emysql/>  

<hr/>

## Contents

* [History][]
* [Usage][]
* [Samples][]
* [Links][]
* [Todo][]
* [License][]

<hr/>

## History

Open Source Erlang MySQL driver efforts are currently a fractured matter, at least for the higher functionality. There are four main choices:

* **Yxa**: The first Erlang MySQL driver seems to have been written in 2005 by [Magnus Ahltorp][ma] at the [Royal Institute of Technology][3]. It is the basis for the following two. The [original mysql driver source][4] is stable since at least 2007, it is available as part of the SIP proxy [Yxa 1.0][5] (hosted [on github][6]).

* **ejabberd**: Already in 2006, a [fork][7] was created by [Mickael Remond][mr] at [Process One][8] to become part of the successful instant messaging server [ejabberd][9] (also hosted [at github][10]). It can be assumed to be as stable as the Yxa branch, and it didn't change anything in the lowest level, mysql_recv.erl. The differences to the original Yxa branch mainly consists of added inspection functions that help using query results, and an [independent adoption][11] to the MySQL 4.1 client-server protocol. Also, the original Yxa branch has meanwhile adopted edoc comment format. Find a diff [here][12], one ignoring comment lines [here][13].

* **erlang-mysql-driver**: in 2006/07 [Yariv Sadan][ys] created a fork from the ejabberd branch, made it a standalone project, gave it the name that stuck, and hosted it at [Google Code][15]. Before he moved on to work at Facebook, he had added higher level handling of prepared statements and transactions, and stated that he had improved connection pooling and logging. There were changes both in the original Yxa and the ejabberd branch after the forking off that seem to never have made their way into the erlang-mysql-driver branch, which now lies dormant since Oct '07. Docs were somewhat unsatisfying, as much as for the earlier branches. In Feb '10, Dave Smith started making some
[updates][15] and put them on github, were the driver is now enjoying a couple of [active forks][16] that make for a convincing case in favor of the github Network graph.

* **Emysql** was started from scratch in 2009 by [Jacob Vorreuter][jv] and [Bill Warnecke][bw] at Electronic Arts, who rewrote the erlang-mysql-driver code because they felt it had been touched by so many people that it had become more complicated than necessary. In a way, the third layer module, mysql.erl, had over time started to become badly entangled with the second, mysql_conn.erl. According to Jacob, Emysql is pretty stable and ran without issue in a production environment at Electronic Arts. This fork is a continuation of [their work][1], including all their commits and adding [documentation][docs], [samples], [fixes][] and extensions.   
  
[Vitaliy Batichko][vb] and
[Chris Rempel][cr] have contributed updates to this branch. Thank you!

[1]: http://github.com/JacobVorreuter/emysql "emysql"  
[2]: http://github.com/dizzyd/erlang-mysql-driver "erlang-mysql-driver"   
[3]: http://www.kth.se/ "Royal Institure of Technology"   
[4]: https://github.com/fredrikt/yxa/tree/master/src/mysql "Yxa mysql driver"   
[5]: http://www.stacken.kth.se/project/yxa/index.html "Yxa Home"   
[6]: https://github.com/fredrikt/yxa "Yxa repository at github"   
[7]: http://svn.process-one.net/ejabberd-modules/mysql/trunk/   
    "ejabberd mysql driver"  
[8]: https://support.process-one.net "Process One Home"  
[9]: http://www.process-one.net/en/ejabberd/ "ejabberd Home"  
[10]: https://github.com/processone/ejabberd/ "ejabberd repository at github"  
[11]: https://support.process-one.net/doc/display/CONTRIBS/Yxa   
     "ejabberd MySQL 4.1. patch"  
[12]: https://github.com/Eonblast/Emysql/tree/master/doc/diff-ejabberd-yxa.txt  
     "Diff of Yxa and ejabberd mysql drivers"  
[13]: https://github.com/Eonblast/Emysql/tree/master/doc/diff-ejabberd-yxa-2.txt  
     "Diff of Yxa and ejabberd mysql drivers ignoring comment changes"  
[14]: http://code.google.com/p/erlang-mysql-driver/   
     "original erlang-mysql-driver"  
[15]: http://github.com/dizzyd/erlang-mysql-driver   
     "Dave Smith's erlang-mysql-driver at github, currently not maintained"  
[16]: https://github.com/dizzyd/erlang-mysql-driver/network   
     "Fork graph of erlang-mysql-driver at github"  

[ma]: ahltorp@nada.kth.se                   "Magnus Ahltorp"  
[ys]: http://yarivsblog.blogspot.com/         
[bw]: bill@rupture.com  
[jv]: https://github.com/JacobVorreuter  
[vb]: https://github.com/bva
[cr]: https://github.com/csrl               "Chris Rempel"  
[hd]: hd2010@eonblast.com                   "Henning Diedrich"  
[mr]: mickael.remond@process-one.net        "Mickael Remond"  

[fixes]:   https://github.com/Eonblast/Emysql/issues/closed  
          "Emysql fixes"  
[docs]:    http://eonblast.github.com/Emysql/  
          "Emysql online docs"  

## Usage 

#### Start the application

	crypto:start(),
	application:start(emysql).

#### Add a pool
	% emysql:add_pool(PoolName, PoolSize, Username, Password, Host, Port, Database, Encoding) ->
	%	 ok | {error, pool_already_exists}  
	% PoolName = atom()  
	% PoolSize = integer()  
	% Username = string()  
	% Password = string()  
	% Host = string()  
	% Port = integer()  
	% Database = string()  
	% Encoding = atom()  
	
	emysql:add_pool(mypoolname, 1, "username", "mypassword", "localhost", 3306, "mydatabase", utf8).

#### Record Types
	-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).
	-record(error_packet, {seq_num, code, msg}).
	-record(result_packet, {seq_num, field_list, rows, extra}).

#### Executing SQL statements
	% emysql:execute(PoolName, Statement) -> result_packet() | ok_packet() | error_packet()  
	% PoolName = atom()  
	% Statement = string() | binary()  
	
	emysql:execute(mypoolname, <<"SELECT * from mytable">>).
	#result_packet{field_list=[...], rows=[...]}
	
	emysql:execute(mypoolname, <<"UPDATE mytable SET bar = 'baz' WHERE id = 1">>).
	#ok_packet{affected_rows=1}

#### Executing prepared statements
	% emysql:prepare(StmtName, Statement) -> ok  
	% StmtName = atom()  
	% Statement = binary() | string()  

	emysql:prepare(my_stmt, <<"SELECT * from mytable WHERE id = ?">>).
	ok
	
	% emysql:execute(PoolName, StmtName, Args) -> result_packet() | ok_packet() | error_packet()  
	% StmtName = atom()  
	% Args = [term()]  

	emysql:execute(mypoolname, my_stmt, [1]).
	#result_packet{field_list=[...], rows=[...]}

#### Executing stored procedures

	% emysql:execute(PoolName, StmtName, Args) -> result_packet() | ok_packet() | error_packet()  
	% StmtName = atom()  
	% Args = [term()]  

	emysql:execute(hello_pool,
		<<"create procedure sp_hello() begin select * from hello_table; end">>).
	{ok_packet,1,0,0,2,0,[]}

	emysql:execute(hello_pool, <<"call sp_hello();">>).
	[{result_packet,6,
	                [{field,2,<<"def">>,<<"hello_database">>,<<"hello_table">>,
	                        <<"hello_table">>,<<"hello_text">>,<<"hello_text">>,
	                        254,<<>>,33,60,0,0}],
	                [[<<"Hello World!">>],[<<"Hello World!">>]],
	                <<>>},
	{ok_packet,7,0,0,34,0,[]}]
 
#### Converting Row Data To Records
	% emysql_util:as_record(ResultPacket, RecordName, Fields) -> Result  
	% ResultPacket = result_packet()  
	% RecordName = atom() (the name of the record to generate)  
	% Fields = [atom()] (the field names to generate for each record)  
	% Result = [record()]  
	
	-module(fetch_example).
	-record(foo, {bar, baz, bat}).
	
	fetch_foo() ->
	   Result = emysql:execute(pool1, <<"select bar, baz, bat from foo">>),
	   Recs = emysql_util:as_record(Result, foo, record_info(fields, foo)),
	   [begin
		  io:format("foo: ~p, ~p, ~p~n", [Foo#foo.bar, Foo#foo.baz, Foo#foo.bat])
	    end || Foo <- Recs].

## Getting Emysql

	$ git clone git://github.com/Eonblast/Emysql.git Emysql
	
## Samples

#### Hello World(*)

This is a hello world program. Follow the three steps below to try it out.
	
	-module(a_hello).
	-export([run/0]).
	
	run() ->
	
		crypto:start(),
		application:start(emysql),
	
		emysql:add_pool(hello_pool, 1,
			"hello_username", "hello_password", "localhost", 3306,
			"hello_database", utf8),
	
		emysql:execute(hello_pool,
			<<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),
	
	    Result = emysql:execute(hello_pool,
			<<"select hello_text from hello_table">>),
	
		io:format("~n~p~n", [Result]).


We come back to that source, but first:

#### Building Emysql

Build emysql.app, using make:

	$ cd Emysql
	$ make


#### Sample database

For the above sample, create a local mysql database. You should have a mysql server installed and running:
	
	$ mysql [-u<user> -p]
	mysql> create database hello_database;
	mysql> use hello_database;
	mysql> create table hello_table (hello_text char(20));
	mysql> grant all privileges on hello_database.* to hello_username@localhost identified by 'hello_password';

#### Run Hello

Be sure to have ./ebin in your Erlang path. Now copy the Hello World source above at '(*)' into a file hello.erl and run it (in the Emysql root directory):

	$ erlc hello.erl
	$ erl -pa ../ebin -s hello run -s init stop -noshell

See more sample programms, below.

#### Running the Samples
Sample programs are in ./samples. 

* [a_hello](http://github.com/Eonblast/Emysql/blob/master/samples/a_hello.erl) - Hello World
* [a_hello2](http://github.com/Eonblast/Emysql/blob/master/samples/a_hello2.erl) - Hello World somewhat rawer
* [b_rows_as_records](http://github.com/Eonblast/Emysql/blob/master/samples/b_rows_as_records.erl) - Using Erlang records to access result rows
* [c_stored_procedure](http://github.com/Eonblast/Emysql/blob/master/samples/c_stored_procedure.erl) - Using Stored procedures

To run the samples, create the database as listed above at localhost, and:

	$ cd samples
	$ ./a_hello
	$ ./b_raw
	$ ./d_rows_as_records
	$ ./e_prepared_statement
	$ ./e_stored_procedure
	
or make emysql.app and start a_hello etc. manually along these lines (but
first create the database as listed above):

	$ make
	$ cd samples
	$ erlc a_hello.erl
	$ erl -pa ../ebin -s a_hello run -s init stop -noshell

## Links

* [Emysql on Github](http://github.com/Eonblast/Emysql)
* [Original Yxa](https://github.com/fredrikt/yxa/tree/master/src/mysql) mysql driver
* [ejabberd fork](http://svn.process-one.net/ejabberd-modules/mysql/trunk/)
* ['erlang-mysql-driver'](http://code.google.com/p/erlang-mysql-driver/)
* [Dave Smith's erlang-mysql-driver fork](http://github.com/dizzyd/erlang-mysql-driver)
* [A maintained erlang-mysql-driver](https://github.com/JoelPM/erlang-mysql-driver)  fork
* [Another maintained&#134; erlang-mysql-driver](https://github.com/chernomor/erlang-mysql-driver)  fork
* [MySQL Client Server Protocol](http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol)
* [MySQL 5.5 Source](ftp://ftp.fu-berlin.de/unix/databases/mysql/Downloads/MySQL-5.5/mysql-5.5.8.tar.gz)

&#134;maintained at the time of writing, Jan 2011.

## TODO
* decrementing pool size could close sockets that are in use
* spawn individual conn_mgr gen_server processes for each pool
* allow row results to be returned as binary

## License

Copyright (c) 2009-2011
Bill Warnecke <bill@rupture.com>,
Jacob Vorreuter <jacob.vorreuter@gmail.com>,
Henning Diedrich <hd2010@eonblast.com>,
Eonblast Corporation [http://www.eonblast.com].

Permission is  hereby  granted,  free of charge,  to any person
obtaining  a copy of this software and associated documentation
files  (the  "Software"),  to  deal  in  the  Software  without 
restriction,  including  without limitation  the rights to use,
copy, modify,  merge,  publish, distribute,  sublicense, and/or 
sell  copies of the  Software,  and to permit  persons  to whom
the  Software  is furnished to do so,  subject to the following 
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.