.PHONY: deps

all: json
	mkdir -p build
	./docs.escript build

deps:
	./rebar get-deps
	cd deps && \
		(test -d smk_api_common || git clone git://git.corp.smarkets.com/smk_api_common.git) && \
	  cd smk_api_common && \
		git checkout renaming && \
		./rebar get-deps

compile: deps
	./rebar compile

json: compile
	piqi convert \
		-f piqi -t json \
		deps/smk_api_common/deps/eto_common/eto.piqi \
		priv/eto.json
	piqi convert -I deps/smk_api_common/deps/eto_common/ \
		-f piqi -t json \
		deps/smk_api_common/seto.piqi \
		priv/seto.json
