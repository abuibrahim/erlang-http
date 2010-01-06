ERL=erl
ERLC=erlc
ERLCFLAGS+=-W +debug_info
ERLS=persons.erl
MXMLC=mxmlc
MXMLFLAGS=-services "services-config.xml"
MXMLS=persons.mxml
BEAMS=$(ERLS:.erl=.beam)
SWFS=$(MXMLS:.mxml=.swf)

.PHONY: clean
.SUFFIXES: .beam .erl .swf .mxml

all: $(BEAMS) $(SWFS)

.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<

.mxml.swf:
	$(MXMLC) $(MXMLFLAGS) $<

persons.swf: persons.as Person.as

clean:
	rm -f $(BEAMS) $(SWFS)

run: all
	$(ERL) -pa ../../ebin ../../deps/*/ebin -s http -config persons
