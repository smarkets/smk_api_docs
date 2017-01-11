.PHONY: deps

PIQI?=./deps/piqi/priv/bin/piqi
VSN=HEAD
NAME_PREFIX=smk_api_docs
NAME=$(NAME_PREFIX)-$(VSN)

all: json
	mkdir -p build
	./docs.escript $(VSN)

deps:
	./rebar get-deps update-deps

compile: deps
	./rebar compile

json: compile
	$(PIQI) convert \
		-f piqi -t json \
		deps/eto_common/eto.piqi \
		priv/eto.json
	$(PIQI) convert -I deps/eto_common/ \
		-f piqi -t json \
		deps/smk_api_common/seto.piqi \
		priv/seto.json

distclean:
	rm -rf $(NAME_PREFIX)*

clean:
	rm -rf build

dist: clean all
	mkdir "$(NAME)"
	cp -rf build "$(NAME)/docs"
	cp deps/smk_api_common/seto.piqi.proto "$(NAME)/"
	cp deps/smk_api_common/seto.piqi "$(NAME)/"
	cp deps/eto_common/eto.piqi.proto "$(NAME)/"
	cp deps/eto_common/eto.piqi "$(NAME)/"
	cp DIST_README "$(NAME)/README"
	tar -zcf "$(NAME).tar.gz" "$(NAME)"
	rm -rf "$(NAME)"

github:
	git push origin github-master
	git push github github-master:master
	git push github HEAD:refs/tags/v$(VSN)

delvsn:
	git push github :refs/tags/v$(VSN)
