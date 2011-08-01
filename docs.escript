#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin deps/jsx/ebin deps/erlydtl/ebin
main([Dir]) ->
  smk_docs:build(Dir).
