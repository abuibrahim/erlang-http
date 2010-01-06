ERL=erl
ERLC=erlc
ERLCFLAGS+=-W +debug_info -I ../../include
ERLS=example1.erl example2.erl example3.erl
BEAMS=$(ERLS:.erl=.beam)

.PHONY: clean
.SUFFIXES: .beam .erl

all: $(BEAMS)

.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<

clean:
	rm -f $(BEAMS)

run: all
	@$(ERL) -pa ../../ebin ../../deps/*/ebin -s http -config esi
