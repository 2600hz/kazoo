[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/wsock/README)](https://github.com/igrigorik/ga-beacon)


#WSOCK


* [About](#about)
* [Examples](#examples)
* [Writing clients](#usage_clients)
  * [Upgrading the connection](#upgrading_client)
  * [Sending data](#sending_client)
  * [Receiving data](#receiving_client)
  * [Control messages](#client_control_messages)
* [Writing servers](#usage_servers)
  * [Upgrading the connection](#upgrading_server)
  * [Sending data](#sending_server)
  * [Receiving data](#receiving_server)
  * [Control messages](#server_control_messages)
* [Documentation](#documentation)
* [Tests](#tests)
* [Contributing](#contributing)
* [Author](#author)
* [License](#license)


## About <a name="about"></a>

Wsock are a set of modules that can be used to build Websockets ([RFC 6455](http://tools.ietf.org/html/rfc6455#section-5.3) compliant) clients an servers.

## Examples <a name="examples"></a>

[wsserver](https://github.com/madtrick/wsserver) (a WebSockets server) and [wsecli](https://github.com/madtrick/wsecli) (a WebSockets client) are projects which use wsock.


## Writing clients <a name="usge_clients"></a>
Don't forguet to include the wsock headers file:

  ```erlang
  -include_lib("wsock/include/wsock.hrl").
  ```
### Upgrading the connection <a name="upgrading_client"></a>

Create and send an upgrade request to the server.


1. Build a handshake request:

  	```erlang
  	HandshakeRequest = wsock_handshake:open(Resource, Host, Port)
  	```

2. Encode the handshake to send it to the server:

  	```erlang
 	 BinaryData = wsock_http:encode(HandshakeRequest#handshake.message)
  	```

3. Receive and validate the handshake response:

  	```erlang
  	{ok, HandshakeResponse} = wsock_http:decode(Data, response)
  	wsock_handshake:handle_response(HandshakeResponse, HandshakeRequest)
  	```
  	
  	If the received HTTP message is fragmented ```wsock_http:decode``` will return the atom ```fragmented_http_message```. Check the section [upgrading the connection when writing servers](#upgrading_server) for more info.

### Sending data <a name="client_sending"></a>
Once the connection has been stablished you can send data through it:
  
  ```erlang
  Message = wsock_message:encode(Data, [mask, text]) %text data
  Message = wsock_message:encode(Data, [mask, binary]) %binary data
  ```

### Receiving data <a name="client_receiving"></a>

* If there is no previous fragmented message:

   ```erlang
   ListOfMessages = wsock_message:decode(Data, [])
   ```

* If the previously received message was fragmented pass it as a parameter:

   ```erlang
   ListOfMessages = wsock_message:decode(Data, FragmentedMessage, [])
   ```

Check ```wsock.hrl``` for a description of the message record.

### Control messages <a name="client_control_messages"></a>

* Ping/pong messages:

  ```erlang
  ListOfMessages = wsock_message:encode(Data, [mask, ping])
  ListOfMessages = wsock_message:encode(Data, [mask, pong])
  ```
  
* Close messages (with or without reason):

  ```erlang
  ListOfMessages = wsock_message:encode({StatusCode, Payload}, [mask, close])
  ListOfMessages = wsock_message:encode([]], [close]) % If no payload
  ```


## Writing servers <a name="usage_servers"></a>


Don't forget to include the wsock headers file:

  ```erlang
  -include_lib("wsock/include/wsock.hrl").
  ```

### Upgrading the connection <a name="upgrading_server"></a>

Accept upgrade requests from your clients.

1. Decode the http-handshake request:

  	```erlang
  	{ok, OpenHttpMessage}   = wsock_http:decode(Data, request),
  	{ok, OpenHandshake}     = wsock_handshake:handle_open(OpenHttpMessage)
  	```
  	
  	If the received HTTP message is fragmented ```wsock_http:decode``` will return the atom ```fragmented_http_message```. In this case, buffer the partial HTTP message until more data is received and try again. 
  	
   ```erlang
  	fragmented_http_message = wsock_http:decode(Data, request),
  	
  	%% Buffer Data
  	%% … some time passes and then more data is received
  	%% Concat the new data to the buffered one and pass it to wsock_http:decode
  	
  	{ok, OpenHttpMessage} = wsock_http:decode(<<Buffered/binary, NewData/binary>>, request),
  	…
  	```
  	

2. Get handshake key to generate a handshake response:
  
  	```erlang
  	ClientWSKey             = wsock_http:get_header_value("sec-websocket-key", 	OpenHandshake#handshake.message),
  	{ok, HandshakeResponse} = wsock_handshake:response(ClientWSKey)
  	```
  
3. Encode the http-handshake response:
  
  	```erlang
  	ResponseHttpMessage     = 	wsock_http:encode(HandshakeResponse#handshake.message)
  	```

Now all you have to do is send the handshake response over the wire to upgrade the HTTP connection to a WebSockets one.

### Receiving data <a name="receiving_server"></a>

* If there is no previous fragmented message:

   ```erlang
   ListOfMessages = wsock_message:decode(Data, [masked])
   ```

* If the previously received message was fragmented pass it as a parameter:

   ```erlang
   ListOfMessages = wsock_message:decode(Data, FragmentedMessage, [masked])
   ```
  
  Check wsock.hrl for a description of the message record.

### Sending data <a name="sending_server"></a>
Once the connection has been stablished you can send data through it:

  ```erlang
  ListOfMessages = wsock_message:encode(Data, [text]) % text data, servers don't mask data
  ListOfMessages = wsock_message:encode(Data, [binary]) % binary data
  ```
  
### Control messages <a name="server_control_messages"></a>

* Ping/pong messages:

  ```erlang
  ListOfMessages = wsock_message:encode(Data, [ping])
  ListOfMessages = wsock_message:encode(Data, [pong])
  ```
* Close messages (with or without reason):

  ```erlang
  ListOfMessages = wsock_message:encode({StatusCode, Payload}, [close])
  ListOfMessages = wsock_message:encode([]], [close]) % If no payload
  ```
  
## Documentation <a name="documentation"></a>
Documentation for the modules can be generated. Run:

  ```shell
  rake doc
  ```
  
or, in case you don't have rake installed:

  ```shell
  rebar doc
  ```

## Tests <a name="tests"></a>
Unit test where done with the library [espec](https://github.com/lucaspiller/espec) by lucaspiller. To run them:

  ```
  rake spec
  ```
or, in case you don't have rake installed:

  ```
  rebar compile && ERL_LIBS='deps/' ./espec test/spec/
  ```

## Contribute <a name="contributing"></a>

If you find or think that something isn't working properly, just open an issue.

Pull requests and patches (with tests) are welcome.

## Author <a name="author"></a>

This stuff has been writen by Farruco sanjurjo

  * [@madtrick](https://twitter.com/madtrick) at twitter
  * Email at [madtrick@gmail.com](madtrick@gmail.com)
  
## License <a name="license"></a>
Copyright [2012] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

