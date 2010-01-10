ERL=erl
ERLC=erlc
APP=http

all: compile

compile:
	@$(ERL) -make

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam
	@rm -fv deps/*/ebin/*.beam

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean-docs:
	@echo "removing:"
	@rm -fv doc/edoc-info doc/*.html doc/*.css doc/*.png

run: compile
	@$(ERL) -pa ebin deps/*/ebin -s $(APP)

test: compile
	@$(ERL) -pa ebin -eval "eunit:test({application,$(APP)})" \
	-noshell -s init stop
