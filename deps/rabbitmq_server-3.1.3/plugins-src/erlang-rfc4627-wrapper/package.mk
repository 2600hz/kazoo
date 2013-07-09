APP_NAME:=rfc4627_jsonrpc
DEPS:=mochiweb-wrapper

UPSTREAM_GIT:=http://github.com/rabbitmq/erlang-rfc4627.git
UPSTREAM_REVISION:=7ab174bc5adc3ec4ea73d69849340fa6fe2cd719

ORIGINAL_APP_FILE=$(CLONE_DIR)/ebin/$(APP_NAME).app
DO_NOT_GENERATE_APP_FILE=true

## The path to httpd.hrl has changed in OTP R14A and newer. Detect the
## change, and supply a compile-time macro definition to allow
## rfc4627_jsonrpc_inets.erl to adapt to the new path.
ifeq ($(shell erl -noshell -eval 'io:format([list_to_integer(X) || X <- string:tokens(erlang:system_info(version), ".")] < [5,8]), halt().'),true)
PACKAGE_ERLC_OPTS+=-Dinets_pre_r14a
else
ifeq ($(shell erl -noshell -eval 'io:format([list_to_integer(X) || X <- string:tokens(erlang:system_info(version), ".")] < [5,8,2]), halt().'),true)
PACKAGE_ERLC_OPTS+=-Dinets_pre_r14b01
endif
endif
