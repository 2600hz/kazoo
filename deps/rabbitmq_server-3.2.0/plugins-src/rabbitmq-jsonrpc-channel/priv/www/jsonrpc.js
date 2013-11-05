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

function JsonRpc_ModuleFactory(Support) {
    var requestId = 1;

    function Transaction(serviceUrl, methodName, params, options) {
        this.options = {
            debug: false,
            debugLogger: alert,
            timeout: 0 /* milliseconds; zero means "do not specify" */
        };
        Support.extend(this.options, options || {});
        this.serviceUrl = serviceUrl;
        this.methodName = methodName;
        this.params = params;
        this.error = null;
        this.reply = null;
        this.replyReady = 0;
        this.callbacks = [];
        this.errorCallbacks = [];
        this.sendRequest();
    }

    Support.extend(Transaction.prototype,
    {
        buildRequest: function() {
            return { version: "1.1",
                     id: requestId++,
                     method: this.methodName,
                     params: this.params };
        },

        sendRequest: function() {
            var that = this;
            this.request = Support.ajaxPost(this.serviceUrl,
                                            this.options.timeout
                                              ? {'X-JSON-RPC-Timeout': this.options.timeout}
                                              : {},
                                            JSON.stringify(this.buildRequest()),
                                            function (ajaxRequest) {
                                                that.receiveReply(ajaxRequest);
                                            });
        },

        debugLog: function(x) {
            if (this.options.debug) {
                this.options.debugLogger(x);
            }
        },

        receiveReply: function(ajaxRequest) {
            var response;
            try {
                response = JSON.parse(ajaxRequest.responseText);
            } catch (e) {
                if (this.options.debug) {
                    this.debugLog("Error parsing JSON:" +
                                  "\nService: " + JSON.stringify(this.serviceUrl) +
                                  "\nResponse: " + ajaxRequest.responseText);
                }
                throw e;
            }
            if (response.error) {
                if (this.options.debug) {
                    this.debugLog("JsonRPC error:" +
                                  "\nService: " + JSON.stringify(this.serviceUrl) +
                                  "\nMethod: " + JSON.stringify(this.methodName) +
                                  "\nParams: " + JSON.stringify(this.params) +
                                  "\nResponse: " + JSON.stringify(response).replace(/\\n/g, "\n"));
                }

                this.error = response.error;
                Support.each(this.errorCallbacks,
                             function (cb) {
                                 try { cb(response.error, true); }
                                 catch (err) {}
                             });
            } else {
                var reply = response.result;
                this.reply = reply;
                this.replyReady = 1;
                Support.each(this.callbacks,
                             function (cb) {
                                 try { cb(reply, false); }
                                 catch (err) {}
                             });
            }
        },

        addReplyTransformer: function(xformer) {
            var that = this;
            var oldAddCallback = that.addCallback;
            that.addCallback = function(cb) {
                return oldAddCallback.apply(that,
                                            [function(reply, is_error) {
                                                 cb(is_error ? reply : xformer(reply), is_error);
                                             }]);
            };
            return that;
        },

        addCallback: function(cb) {
            this.callbacks.push(cb);
            if (this.replyReady) {
                try { cb(this.reply, false); }
                catch (err) {}
            }
            return this;
        },

        addErrorCallback: function(cb) {
            this.errorCallbacks.push(cb);
            if (this.error) {
                try { cb(this.error, true); }
                catch (err) {}
            }
            return this;
        }
    });

    function Service(serviceUrl, onReady, options) {
        this.options = {
            transactionClass: Transaction,
            timeout: 0, /* milliseconds; zero means "do not specify" */
            debug: false,
            debugLogger: alert
        };
        Support.extend(this.options, options || {});
        this.serviceUrl = serviceUrl;
        var svc = this;
        var txn = new (this.options.transactionClass)(serviceUrl,
                                                      "system.describe",
                                                      [],
                                                      {debug: this.options.debug,
                                                       debugLogger: this.options.debugLogger});
        txn.addCallback(receiveServiceDescription);
        function receiveServiceDescription(sd) {
            svc.serviceDescription = sd;
            Support.each(svc.serviceDescription.procs,
                         function (desc) {
                             svc.installGenericProxy(desc);
                         });
            onReady();
        }
    };

    Support.extend(Service.prototype,
    {
        installGenericProxy: function(desc) {
            if (this.options.debug) {
                this.options.debugLogger({installGenericProxy: desc});
            }
            this[desc.name] = function () {
                var actuals = [];
                while (actuals.length < arguments.length) {
                    actuals.push(arguments[actuals.length]);
                }
                while (actuals.length < desc.params.length) {
                    actuals.push(null);
                }
                return new (this.options.transactionClass)(this.serviceUrl,
                                                           desc.name,
                                                           actuals,
                                                           {
                                                               debug: this.options.debug,
                                                               debugLogger:
                                                                 this.options.debugLogger,
                                                               timeout: this.options.timeout
                                                           });
            };
        }
    });

    return {
        Transaction: Transaction,
        Service: Service
    }
}
