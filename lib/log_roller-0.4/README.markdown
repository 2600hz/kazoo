## Highlights
* distributed nodes broadcast log messages to a central disk log
* configure the max size and number of files in your rotating disk log
* filter log messages into partitioned "buckets" based on node names, log types, and regexp matching
* full featured web application for log monitoring and greping
* extendable via event_handler registration for custom log routing

## ABOUT
Log_roller is based on a publisher/subscriber model. It integrates
seamlessly with existing systems without the need to change code.
Nodes designated as publishers (these are your app nodes running
the log_roller application) broadcast log messages sent to their
error_logger to nodes designated as subscribers (these are nodes
running the log_roller_server application that collect log messages
and write them to a rotating disk log).

Publishers and subscribers perform an auto-discovery step upon booting.
During startup, nodes of either type (publisher or subscriber)
will attempt to connect. Make sure to have matching ~/.erlang.cookie 
files hosted on the servers running log_roller and log_roller_server. 
The nodes will not be able to see each other otherwise.

## Dependencies

mochiweb <http://github.com/clones/mochiweb>

## Installation
Either clone the repository github.com/JacobVorreuter/log_roller
or download the tarball from jacobvorreuter.github.com/log_roller
and run 

	make
	sudo make install

## Integration
To begin logging messages with log_roller, simply start the
log_roller application. This can be done either in the app file of your
application:

	{application, my_app, [
	    {description, "my_app"},
	    {vsn, "0.4"},
	    {modules, []},
	    {registered, []},
	    {mod, {my_app, []}},
	    {applications, [kernel, stdlib, log_roller]}
	]}.

-or-

you can just start log_roller from somewhere in your app:

log_roller:start()

Nodes with log_roller enabled will automatically ping visible
log_roller_server nodes to establish a connection.

******

To start a subscriber node, you must either use the log_roller_server
boot script or start the log_roller_server application.

/etc/init.d/log_roller start server

-or-

erl -name server@`hostname` -boot log_roller_server

-or-

	{application, my_app, [
	    {description, "my_app"},
	    {vsn, "0.4"},
	    {modules, []},
	    {registered, []},
	    {mod, {my_app, []}},
	    {applications, [kernel, stdlib, log_roller_server]}
	]}.

-or-

log_roller_server:start()

## CONFIGURATION
There is a config file, "log_roller.config" located in the priv 
directory.  This will be installed to the priv directory in the
log_roller_server lib directory inside your otp root directory. For
instance: /usr/lib/erlang/lib/log_roller_server-0.3/priv/log_roller.config.
This contains the path to the log directory as well as max file size
and number of rotating log files.

__Simple Example__

	[{log_roller_server, [
		{cache_size, 65536},
		{maxbytes, 10485760},
		{maxfiles, 10}
	]}].
	
__Filtering to Multiple Buckets__

	[{log_roller_server, [
		{logs, [
			{first, [
				{cache_size, 65536},
				{maxbytes, 10485760},
				{maxfiles, 10},
				{filters, [
					{nodes, ['foo@MyDogJesusMac.local', 'ack@MyDogJesusMac.local']}
				]}
			]},
			{second, [
				{cache_size, 65536},
				{maxbytes, 10485760},
				{maxfiles, 10},
				{filters, [
					{nodes, ['bar@MyDogJesusMac.local']},
					{types, [error, warning]}
				]}
			]},
			{third, [
				{cache_size, 65536},
				{maxbytes, 10485760},
				{maxfiles, 10},
				{filters, [
					{nodes, ['baz@MyDogJesusMac.local']},
					{type, [progress, info]},
					{grep, "log_roller"}
				]}
			]}
		]}		
	]}].

## LICENSE

	Copyright (c) 2009 Jacob Vorreuter <jacob.vorreuter@gmail.com>

	Permission is hereby granted, free of charge, to any person
	obtaining a copy of this software and associated documentation
	files (the "Software"), to deal in the Software without
	restriction, including without limitation the rights to use,
	copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the
	Software is furnished to do so, subject to the following
	conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
	OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
	WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
	FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.
