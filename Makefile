.PHONY: deps

VSN=dev
NAME=smk_api_docs-$(VSN)

all: json
	mkdir -p build
	./docs.escript $(VSN)

deps:
	./rebar get-deps
	cd deps && \
		((test -d smk_api_common && \
	 		cd smk_api_common && \
		  git pull origin master && \
		  ./rebar update-deps) \
		|| \
		 (git clone git://git.corp.smarkets.com/smk_api_common.git && \
			cd smk_api_common && \
			./rebar get-deps) \
			)

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

distclean:
	rm -rf *.tar.gz

dist: all
	cp -rf build "$(NAME)"
	tar -zcf "$(NAME).tar.gz" "$(NAME)"
	rm -rf "$(NAME)"
