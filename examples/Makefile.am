dist_erl_DATA = http_esi_example.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_http)/examples
beam_DATA = $(dist_erl_DATA:.erl=.beam)
beamdir = $(ERLANG_INSTALL_LIB_DIR_http)/ebin
dist_html_DATA = index.html examples.html
htmldir = $(ERLANG_INSTALL_LIB_DIR_http)/priv/htdocs
CLEANFILES = $(beam_DATA)
ERLCFLAGS += -W -I../src

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<
