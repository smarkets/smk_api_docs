
all: json
	mkdir -p build
	./docs.escript build

deps:
	./rebar get-deps

compile: deps
	./rebar compile

json: compile
	piqi convert \
		-f piqi -t json \
		deps/eto_common/eto.piqi \
		priv/eto.json
	piqi convert -I deps/eto_common/ \
		-f piqi -t json \
		deps/smk_api_common/seto.piqi \
		priv/seto.json
