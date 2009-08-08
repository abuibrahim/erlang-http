dist_erl_DATA = \
	http_app.erl \
	http_sup.erl \
	http_lib.erl \
	http_server.erl \
	http_client.erl \
	http_socket_server.erl \
	http_mod_scheme.erl \
	http_mod_options.erl \
	http_mod_trace.erl \
	http_mod_alias.erl \
	http_mod_auth.erl \
	http_mod_amf.erl \
	http_mod_esi.erl \
	http_mod_access.erl \
	http_mod_head.erl \
	http_mod_match.erl \
	http_mod_modified.erl \
	http_mod_range.erl \
	http_mod_include.erl \
	http_mod_dir.erl \
	http_mod_cache.erl \
	http_mod_regular.erl \
	http_mod_gzip.erl \
	http_mod_deflate.erl \
	http_mod_md5.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_http)/src
dist_hrl_DATA = http.hrl
hrldir = $(ERLANG_INSTALL_LIB_DIR_http)/include
beam_DATA = $(dist_erl_DATA:.erl=.beam) http.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_http)/ebin
boot_DATA = http.boot http.script
bootdir = $(libdir)/erlang/bin
html_DATA = $(dist_erl_DATA:.erl=.html) erlang.png edoc-info stylesheet.css \
	index.html overview-summary.html modules-frame.html packages-frame.html
htmldir = $(ERLANG_INSTALL_LIB_DIR_http)/doc
dist_noinst_DATA = overview.edoc.in
dist_priv_DATA = mime_types.config
privdir = $(ERLANG_INSTALL_LIB_DIR_http)/priv
CLEANFILES = $(beam_DATA) $(boot_DATA) $(html_DATA) http.rel overview.edoc
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)
$(boot_DATA): $(beam_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $< 

.rel.boot:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. +no_module_tests $<

.rel.script:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. +no_module_tests $<

$(html_DATA): $(dist_erl_DATA) overview.edoc
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application http '"."' '[{dir,"."}]'

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,http})" -noshell -s init stop
