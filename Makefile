.PHONY: deps

VSN=HEAD
NAME_PREFIX=smk_api_docs
NAME=$(NAME_PREFIX)-$(VSN)

all: json proto
	mkdir -p build
	./docs.escript $(VSN)

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

proto: deps
	cd deps/smk_api_common/ && make proto
	cd deps/eto_common/ && make proto

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
	git tag v$(VSN)
	git push origin refs/tags/v$(VSN):refs/tags/v$(VSN)
	git push github refs/tags/v$(VSN):refs/tags/v$(VSN)

delvsn:
	git tag -d v$(VSN)
	git push origin :refs/tags/v$(VSN)
	git push github :refs/tags/v$(VSN)
