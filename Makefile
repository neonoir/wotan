.PHONY: all deps compile clean distclean shell

DEPS_DIR = $(CURDIR)/deps
RABBIT_DL_URI = http://www.rabbitmq.com/releases/rabbitmq-erlang-client/v3.4.3
RABBIT = $(DEPS_DIR)
UUID_GIT_REPO = git@github.com:okeuday/uuid.git
DEPS = $(RABBIT)/rabbit_common-3.4.3.ez \
       $(RABBIT)/amqp_client-3.4.3.ez \

all: deps compile

deps: $(DEPS_DIR) $(DEPS)
	rebar get-deps

$(DEPS_DIR):
	mkdir -p $(DEPS_DIR)

$(RABBIT)%:
	wget $(RABBIT_DL_URI)$* -O $@
	unzip $@ -d $(DEPS_DIR)

app: compile ebin/wotan.app

ebin/wotan.app: src/wotan.app.src
	cp src/wotan.app.src ebin/wotan.app

compile:
	@mkdir -p $(CURDIR)/ebin
	erl -pa deps/*/ebin -make

clean:
	rm -rf *.beam ebin/*.beam erl_crash.dump

distclean: clean
	rm -rf $(DEPS_DIR)

shell:
	erl -pa ebin -pa deps/*/ebin -s reloader start
