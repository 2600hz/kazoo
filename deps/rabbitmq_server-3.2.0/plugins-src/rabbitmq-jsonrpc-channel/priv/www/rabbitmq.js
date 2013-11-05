//  The contents of this file are subject to the Mozilla Public License
//  Version 1.1 (the "License"); you may not use this file except in
//  compliance with the License. You may obtain a copy of the License
//  at http://www.mozilla.org/MPL/
//
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
//  the License for the specific language governing rights and
//  limitations under the License.
//
//  The Original Code is RabbitMQ.
//
//  The Initial Developer of the Original Code is GoPivotal, Inc.
//  Copyright (c) 2007-2013 GoPivotal, Inc.  All rights reserved.
//

function RabbitMQ_ModuleFactory(JsonRpc, Support) {
    function openChannel(readyFn, options) {
	var o = {
	    factoryServiceUrl: "/rpc/rabbitmq",
	    timeout: 30000 // timeout for the *factory*, not the channel
	};
	Support.extend(o, options || {});

	var factoryService = new JsonRpc.Service(o.factoryServiceUrl, onServiceReady, o);
	function onServiceReady() {
	    new Channel(factoryService, readyFn, o);
	}
    }

    function Channel(factory, readyFn, options) {
	this.options = {
	    rpcServiceUrlBase: "/rpc/",
	    username: "guest",
	    password: "guest",
	    virtualHost: null,
	    realm: "/data",
	    debug: false,
	    debugLogger: alert,
	    channelTimeout: 10 /* seconds; zero means "do not specify" */
	};
	Support.extend(this.options, options || {});
        this.consumers = {};
        this.alive = true;
	this.ticket = 0;

	var that = this;

	factory.open(this.options.username,
		     this.options.password,
		     this.options.channelTimeout,
		     this.options.virtualHost)
	.addCallback(channel_created);

	function channel_created(reply) {
	    that.service = new JsonRpc.Service(that.options.rpcServiceUrlBase + reply.service,
					       ready,
					       {debug: that.options.debug,
						debugLogger: that.options.debugLogger,
						timeout: that.options.channelTimeout * 1000});
	}

	function ready(result) {
            that.poll_tophalf();
	    Support.bindEvent(window, 'unload', function () { that.close() });
            Support.bindEvent(window, 'pagehide', function () { that.close() });

	    readyFn(that);
	}
    }

    Support.extend(Channel.prototype,
    {
	_dval: function(v, d) {
	    return (v == null) ? d : v;
	},

	_call: function(method, args) {
	    if (this.alive) {
		return this.service.call(method, args);
	    }
	},

	_cast: function(method, args, content, props) {
	    if (this.alive) {
		this.service.cast(method, args, content, props);
	    }
	},

	_extractArg: function(index) {
	    return function(reply) { return reply.args[index]; };
	},

	exchangeDeclare: function(exchange, type, passive, durable, auto_delete, arguments) {
            return this._call("exchange.declare", [this.ticket,
						   exchange,
						   this._dval(type, "direct"),
						   this._dval(passive, false),
						   this._dval(durable, false),
						   this._dval(auto_delete, false),
						   false, // internal
						   false, // nowait
						   this._dval(arguments, {})]);
	},

	queueDeclare: function(queue, passive, durable, exclusive, auto_delete, arguments) {
            return this._call("queue.declare", [this.ticket,
						this._dval(queue, ""),
						this._dval(passive, false),
						this._dval(durable, false),
						this._dval(exclusive, false),
						this._dval(auto_delete, true),
						false, // nowait
						this._dval(arguments, {})])
	    .addReplyTransformer(this._extractArg(0));
	},

	queueDelete: function(queue, if_unused, if_empty) {
            return this._call("queue.delete", [this.ticket,
					       this._dval(queue, ""),
					       this._dval(if_unused, false),
					       this._dval(if_empty, false),
					       false // nowait
					      ])
	    .addReplyTransformer(this._extractArg(0));
	},

	queueBind: function(queue, exchange, routing_key, arguments) {
            return this._call("queue.bind", [this.ticket,
					     queue,
					     exchange,
					     this._dval(routing_key, ""),
					     false, // nowait
					     this._dval(arguments, {})]);
	},

	basicConsume: function(queue, consumer, options) {
	    o = {
		consumer_tag: "",
		no_local: false,
		no_ack: false,
		exclusive: false
	    };
	    Support.extend(o, options || {});
	    var ch = this;
	    return ch._call("basic.consume", [ch.ticket,
					      queue,
					      o.consumer_tag,
					      o.no_local,
					      o.no_ack,
					      o.exclusive,
					      false, // nowait
					      []
					     ])
	    .addReplyTransformer(ch._extractArg(0))
	    .addCallback(function (tag) {
			     ch.consumers[tag] = consumer;
			     if (consumer.consumeOk) {
				 consumer.consumeOk(tag);
			     }
			 });
	},

	_js_props: function(props) {
	    return { content_type: props[0],
		     content_encoding: props[1],
		     headers: props[2],
		     delivery_mode: props[3],
		     priority: props[4],
		     correlation_id: props[5],
		     reply_to: props[6],
		     expiration: props[7],
		     message_id: props[8],
		     timestamp: props[9],
		     type: props[10],
		     user_id: props[11],
		     app_id: props[12],
		     cluster_id: props[13] };
	},

	_amqp_props: function(props) {
	    return [props.content_type,
		    props.content_encoding,
		    props.headers,
		    props.delivery_mode,
		    props.priority,
		    props.correlation_id,
		    props.reply_to,
		    props.expiration,
		    props.message_id,
		    props.timestamp,
		    props.type,
		    props.user_id,
		    props.app_id,
		    props.cluster_id];
	},

	basicPublish: function(exchange, routing_key, message, props, mandatory, immediate) {
	    this._cast("basic.publish", [this.ticket,
					 exchange,
					 routing_key,
					 this._dval(mandatory, false),
					 this._dval(immediate, false)],
		       message, this._amqp_props(props || {}));
	},

	basicAck: function(delivery_tag, multiple) {
	    this._cast("basic.ack", [delivery_tag,
				     this._dval(multiple, false)]);
	},

	basicCancel: function(consumer_tag) {
	    var ch = this;
	    return ch._call("basic.cancel", [consumer_tag,
					     false // nowait
					    ])
	    .addReplyTransformer(ch._extractArg(0))
	    .addCallback(function (tag) {
			     var consumer = ch.consumers[tag];
			     delete ch.consumers[tag];
			     if (consumer && consumer.cancelOk) {
				 consumer.cancelOk(tag);
			     }
			 });
	},

	poll_tophalf: function() {
	    var ch = this;
            if (ch.alive) {
		ch.service.poll()
		.addCallback(function (result) { ch.handlePollResult(result) })
		.addCallback(function () { ch.poll_tophalf() });
	    }
	},

	close: function() {
	    var ch = this;
            if (ch.alive) {
		ch.alive = false;
		ch.service.close()
		.addCallback(function (result) { ch.handlePollResult(result) });
            }
	},

	handlePollResult: function(result) {
	    var ch = this;
	    if (result === "stop") {
		ch.alive = false;
	    } else {
		Support.each(result, function (e) { ch.handleAsyncMessage(e) });
	    }
	},

	handleAsyncMessage: function (message) {
	    var handler = this["handle_async_" + message.method];
	    if (handler) {
		handler.apply(this, [message.args,
				     message.content,
				     message.props]);
	    } else {
		if (this.options.debug) {
		    this.options.debugLogger({async: message});
		}
	    }
	},

	"handle_async_basic.deliver": function(args, content, props) {
            var consumer = this.consumers[args[0]];
            if (consumer) {
		try {
		    consumer.deliver({content: content,
				      delivery_tag: args[1],
				      redelivered: args[2],
				      exchange: args[3],
				      routing_key: args[4],
				      props: this._js_props(props)});
		} catch (err) {}
            }
	}
    });

    return {
	openChannel: openChannel,
	Channel: Channel
    };
}
