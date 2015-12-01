[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/wsecli/README)](https://github.com/igrigorik/ga-beacon) [![Build Status](https://travis-ci.org/esl/wsecli.png)](https://travis-ci.org/esl/wsecli)


wsecli
======

A WebSocket client written in Erlang

* [Supported WebSocket version](#versions)
* [Build](#build)
* [Usage](#usage)
* [Tests](#tests)
* [TODO](#todo)
* [License](#license)
* [Contribute](#contribute)

### Features <a name="features"> ###
  * Built using [wsock](https://github.com/madtrick/wsock)
  * Supports both "ws://" and "wss://" schemes.
  * Offers callbacks for events that might occur during the client life: ```on_open```, ```on_close```, ```on_message``` and ```on_error```.

### Supported protocol versions <a name="versions"/> ###
Only the protocol specificied at [RFC6455](http://tools.ietf.org/html/rfc6455) (version 13) is supported. Notice that currently, neither _subprotocols_ nor _extensions_ are supported.

### Build <a name="build">###

Add this repo as a dependency to your rebar.config file

```erlang
{wsecli, ".*", {git, "https://github.com/esl/wsecli", {tag, master}}}
```

and then

```bash
./rebar compile
```

### Usage <a name="usage">###

I will demostrate its usage with the echo service at [www.websocket.org](http://www.websocket.org/echo.html).


1. Start it, using the function ```wsecli:start/4```


   ```erlang
   1>wsecli:start("echo.websocket.org", 80, "/", []).
   ```
   
   or the function ```wsecli:start/2```
   
   ```erlang
   2>wsecli:start("ws://echo.websocket.org/", []).
   ```
   
   The valid options for the client are:
   
   	  * {registered, true | false | atom()}. When *true* the client will be registered using the default name *wsecli*, when *false* it will not be registered and any other atom will be used as the name to register with. By default it will not be registered.
   	  
   	  
2. Add a callback for received messages,

  	```erlang
  	3>wsecli:on_message(fun(text, Message)-> io:format("Echoed message: ~s ~n", [Message]) end).
  	```

3. Send a message that will be echoed,

  	```erlang
  	4> wsecli:send("Hello").
  	ok
  	Echoed message: Hello
  	```

4. And finally to stop it

  	```erlang
  	5>wsecli:stop().
  	```


#### Callbacks

Callbacks for the events: *on_open*, *on_error*, *on_message* and *on_close* can be added. Check the code or the documentation for details.



### Tests <a name="tests">

#### Unit tests

Unit test where done with the library [_espec_](https://github.com/lucaspiller/espec) by [lucaspiller](https://github.com/lucaspiller).

 To run them

  ```bash
  rake spec
  ```
  
  or, in case you don't have rake installed,
  

  ```bash
  ./rebar compile && ERL_LIBS='deps/' ./espec test/spec/
  ```

### TODO <a name="todo">

* Support streaming of data.
* Support _subprotocol_ and _extensions_.

###Author
Farruco Sanjurjo. You can contact me at:

* Twitter [@madtrick](https://twitter.com/madtrick)
* Mail madtrick@gmail.com

### License <a name="installation">

Copyright [2013] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

### Contribute <a name="contribute">

If you find or think that something isn't working properly, just open an issue.

Pull requests and patches (with tests) are welcome.
