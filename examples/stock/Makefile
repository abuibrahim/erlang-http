ERL=erl
ERLC=erlc
ERLCFLAGS+=-W +debug_info -I ../../include -I ../../deps/amf/include
ERLS=stock.erl mochijson2.erl
MXMLC=mxmlc
MXMLFLAGS=-services "services-config.xml"
MXMLS=stock.mxml
BEAMS=$(ERLS:.erl=.beam)
SWFS=$(MXMLS:.mxml=.swf)

.PHONY: clean
.SUFFIXES: .beam .erl .swf .mxml

all: $(BEAMS) $(SWFS)

.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<

.mxml.swf:
	$(MXMLC) $(MXMLFLAGS) $<

stock.swf: stock.as

clean:
	rm -f $(BEAMS) $(SWFS)

run: all
	@$(ERL) -pa ../../ebin ../../deps/*/ebin -s http -config stock
