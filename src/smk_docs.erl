-module(smk_docs).

-export([build/1, build_site/1]).

-define(BUILD, "build").

build(Vsn) ->
  write(prep(Vsn), ?BUILD).
build_site(Vsn) ->
  write([{live_site,1}|prep(Vsn)], "site").

write(Ctx, Dir) ->
  {ok, Index} = index_tpl:render(Ctx),
  file:write_file(j(Dir,"index.html"), Index),
  lists:foreach(fun(F) -> file:copy(priv_dir(F), j(Dir,F)) end, [
      "style.css", "logo.gif"
    ]),
  ok.


prep(Vsn) ->
  {ok, EtoBin} = file:read_file(priv_dir("eto.json")),
  {ok, SetoBin} = file:read_file(priv_dir("seto.json")),

  ok = erlydtl:compile(priv_dir("index.html"), index_tpl, [
      {custom_tags_dir, priv_dir()}
    ]),

  {EtoVsn, SetoVsn} = get_piqi_vsns(),

  SetoSpec = atomize(seto, jsx:json_to_term(SetoBin)),
  SetoPiqDef = proplists:get_value(piqdef, SetoSpec),

  EtoSpec = atomize(eto, jsx:json_to_term(EtoBin)),
  EtoPiqDef = proplists:get_value(piqdef, EtoSpec),

  SortedSetoPiqDef = lists:sort(fun sort/2, SetoPiqDef),
  SortedEtoPiqDef = lists:sort(fun sort/2, EtoPiqDef),

  [
    {version, case Vsn of
      "HEAD" -> "HEAD";
      Vsn -> [$v,Vsn]
    end},
    {eto_order, [proplists:get_value(name, proplists:get_value(def, DefDef)) || DefDef <- SortedEtoPiqDef]},
    {seto_order, [proplists:get_value(name, proplists:get_value(def, DefDef)) || DefDef <- SortedSetoPiqDef]},
    {eto_vsn, EtoVsn},
    {seto_vsn, SetoVsn},
    {modules, [
        [{name,"seto"},{def,SortedSetoPiqDef}],
        [{name,"eto"},{def,SortedEtoPiqDef}]
      ]}
  ].

get_piqi_vsns() ->
  {ok, RebarConfig} = file:consult("rebar.config"),
  Deps = proplists:get_value(deps, RebarConfig),
  {
    case lists:keyfind(eto_common, 1, Deps) of
      {eto_common,_,{git,_,{tag,EtoVsn}}} ->
        EtoVsn;
      _ ->
        "HEAD"
    end,
    case lists:keyfind(smk_api_common, 1, Deps) of
      {smk_api_common,_,{git,_,{tag,SetoVsn}}} ->
        SetoVsn;
      _ ->
        "HEAD"
    end
  }.


%extend(Extend, Defs) ->
%  [Name] = proplists:get_value(name,Extend),
%  _Existing =
%  extend(Name, Extend, find(Name, Defs), Defs).
%
%extend(_, _, undefined, Defs) -> Defs;
%extend(Name, Extend, Def, Defs) ->
%  Any = proplists:get_value(piq_any, Extend),
%  replace(Name, lists:foldl(fun extend_any/2, Def, Any),Defs).
%
%extend_any([{named, Named}], [{piqi_type,Type}|_] = Def) ->
%  extend(Type, Named, Def);
%
%extend_any(_, Def) -> Def.
%
%extend(record, [{name,field},{value,Value}], Def) ->
%  [{list,[
%        [{named,[{name,name},{value,[{word,Name}]}]}],
%        [{named,[{name,type},{value,[{word,Type}]}]}]
%      ]}] = Value,
%  DefDef = proplists:get_value(def, Def),
%  Fields = proplists:get_value(field, DefDef),
%  NewFields = Fields ++ [[{name,Name},{type,[{name,Type}]}]],
%  lists:keyreplace(def, 1, Def,
%    {def, lists:keyreplace(field, 1, DefDef,
%      {field, NewFields}
%    )}
%  );
%extend(variant, [{name,option},{value,Value}], Def) ->
%  [{list,[
%        [{named,[{name,type},{value,[{word,Type}]}]}]
%      ]}] = Value,
%  DefDef = proplists:get_value(def, Def),
%  Options = proplists:get_value(option, DefDef),
%  NewOptions = Options ++ [[{type,[{name,Type}]}]],
%  lists:keyreplace(def, 1, Def,
%    {def, lists:keyreplace(option, 1, DefDef,
%      {option, NewOptions}
%    )}
%  );
%
%extend(_, _, Def) -> Def.
%
%find(_, []) -> undefined;
%find(Name, [[_,{def,[{name,Name}|_]}|_]=H|_]) ->
%  H;
%find(Name, [_|T]) ->
%  find(Name, T).
%
%replace(Name, Def, Defs) ->
%  replace(Name, Def, Defs, []).
%
%replace(Name, Def, [[_,{def,[{name,Name}|_]}|_]|T], Acc) ->
%  lists:reverse(Acc) ++ [Def|T];
%replace(Name, Def, [H|T], Acc) ->
%  replace(Name, Def, T, [H|Acc]).
   

priv_dir() ->
  case code:priv_dir(smk_docs) of
    {error, _} ->
      filename:absname(
        filename:join([
            filename:dirname(code:which(?MODULE)),
            "..", "priv"]));
    Priv ->
      Priv
  end.
priv_dir(File) ->
  filename:join(priv_dir(), File).

description(Module, [{<<"name">>,Name}|_]) ->
  File = priv_dir(j(["types",atom_to_binary(Module,utf8),<<Name/binary,".html">>])),
  case file:read_file(File) of
    {ok, Description} -> Description;
    _ -> ""
  end;
description(_, _) -> "".

atomize(Module, [{Type,Def}]) when Type =:= <<"alias">>
                   orelse Type =:= <<"enum">>
                   orelse Type =:= <<"record">>
                   orelse Type =:= <<"variant">> ->
  [
    {piqi_type,atomize(Module, Type)},
    {def, atomize(Module, Def)},
    {description,description(Module, Def)}
  ];
atomize(Module, [{<<"list">>,[{<<"name">>,_},{<<"type">>,_}]=Def}]) ->
  [
    {piqi_type,list},
    {def, atomize(Module, Def)},
    {description,description(Module, Def)}
  ];

atomize(Module, {<<"default">>, [{Type,Value}]}) ->
  {default, [{type,atomize(Module, Type)}, {value,Value}]};

atomize(Module, {<<"type">>,[{<<"name">>,Name}]}) ->
  {type,
    case binary:match(Name, <<"/">>) of
      nomatch ->
        [{name,atomize(Module,Name)},{module,atom_to_binary(Module,utf8)}];
      {Pos,_} ->
        OtherModule = binary:part(Name,0,Pos),
        NamePart = binary:part(Name,Pos+1,size(Name)-Pos-1),
        [{name,NamePart},{module,OtherModule}]
    end};

atomize(Module, L) when is_list(L) ->
  lists:map(fun(X) -> atomize(Module,X) end, L);

atomize(Module, {K,V}) when is_binary(K) ->
  {atomize(Module, K),atomize(Module, V)};

atomize(_, T) when is_binary(T) -> binary_to_atom(T,utf8);
atomize(_, T) -> T.

j(D,F) -> j([D,F]).
j(L) -> filename:join(L).

sort(A, B) ->
  name(A) < name(B).

name(DefDef) ->
  proplists:get_value(name, proplists:get_value(def, DefDef)).
