#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/jsx/ebin deps/erlydtl/ebin
main([Vsn]) ->
  smk_docs:build(Vsn);

main([Vsn, "site"]) ->
  smk_docs:build_site(Vsn).

