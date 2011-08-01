-module(smk_docs).

-export([build/1]).

build(Dir) ->
  {ok, EtoBin} = file:read_file(priv_dir("eto.json")),
  {ok, SetoBin} = file:read_file(priv_dir("seto.json")),

  erlydtl:compile(priv_dir("index.html"), index_tpl),

  SetoSpec = atomize(jsx:json_to_term(SetoBin)),
  SetoPiqDef = proplists:get_value(piqdef, SetoSpec),

  EtoSpec = atomize(jsx:json_to_term(EtoBin)),
  EtoPiqDef =
    lists:foldl(fun extend/2,
      proplists:get_value(piqdef, EtoSpec),
      proplists:get_value(extend, SetoSpec)
    ),

  PiqDef = EtoPiqDef ++ SetoPiqDef,

  Ctx = [
    {spec,io_lib:format("~p", [EtoSpec ++ SetoSpec])},
    {piqdef, PiqDef}
  ],
  {ok, Index} = index_tpl:render(Ctx),
  file:write_file(j(Dir,"index.html"), Index),
  file:copy(priv_dir("style.css"), j(Dir,"style.css")),
  ok.

extend(Extend, Defs) ->
  [Name] = proplists:get_value(name,Extend),
  _Existing =
  extend(Name, Extend, find(Name, Defs), Defs).

extend(_, _, undefined, Defs) -> Defs;
extend(Name, Extend, Def, Defs) ->
  Any = proplists:get_value(piq_any, Extend),
  replace(Name, lists:foldl(fun extend_any/2, Def, Any),Defs).

extend_any([{named, Named}], [{piqi_type,Type}|_] = Def) ->
  extend(Type, Named, Def);

extend_any(_, Def) -> Def.

extend(record, [{name,field},{value,Value}], Def) ->
  [{list,[
        [{named,[{name,name},{value,[{word,Name}]}]}],
        [{named,[{name,type},{value,[{word,Type}]}]}]
      ]}] = Value,
  DefDef = proplists:get_value(def, Def),
  Fields = proplists:get_value(field, DefDef),
  NewFields = Fields ++ [[{name,Name},{type,[{name,Type}]}]],
  lists:keyreplace(def, 1, Def,
    {def, lists:keyreplace(field, 1, DefDef,
      {field, NewFields}
    )}
  );
extend(variant, [{name,option},{value,Value}], Def) ->
  [{list,[
        [{named,[{name,type},{value,[{word,Type}]}]}]
      ]}] = Value,
  io:format("Extended Option ~p~n", [Type]),
  DefDef = proplists:get_value(def, Def),
  Options = proplists:get_value(option, DefDef),
  NewOptions = Options ++ [[{type,[{name,Type}]}]],
  lists:keyreplace(def, 1, Def,
    {def, lists:keyreplace(option, 1, DefDef,
      {option, NewOptions}
    )}
  );

extend(_, _, Def) -> Def.

find(_, []) -> undefined;
find(Name, [[_,{def,[{name,Name}|_]}]=H|_]) ->
  H;
find(Name, [_|T]) ->
  find(Name, T).

replace(Name, Def, Defs) ->
  replace(Name, Def, Defs, []).

replace(Name, Def, [[_,{def,[{name,Name}|_]}]|T], Acc) ->
  lists:reverse(Acc) ++ [Def|T];
replace(Name, Def, [H|T], Acc) ->
  replace(Name, Def, T, [H|Acc]).
   

priv_dir(File) ->
  filename:join(
    case code:priv_dir(smk_docs) of
      {error, _} ->
        filename:absname(
          filename:join([
              filename:dirname(code:which(?MODULE)),
              "..", "priv"]));
      Priv ->
        Priv
    end,
    File).

atomize([{<<"record">>,Def}]) ->
  [
    {piqi_type,record},
    {def, atomize(Def)}
  ];
atomize([{<<"variant">>,Def}]) ->
  [
    {piqi_type,variant},
    {def, atomize(Def)}
  ];
atomize([{<<"enum">>,Def}]) ->
  [
    {piqi_type,enum},
    {def, atomize(Def)}
  ];
atomize([{<<"alias">>,Def}]) ->
  [
    {piqi_type,alias},
    {def, atomize(Def)}
  ];
atomize([{<<"list">>,[{<<"name">>,_},{<<"type">>,_}]=Def}]) ->
  [
    {piqi_type,list},
    {def, atomize(Def)}
  ];

atomize({<<"default">>, [{Type,Value}]}) ->
  {default, [{type,atomize(Type)}, {value,Value}]};

atomize(L) when is_list(L) ->
  lists:map(fun atomize/1, L);

atomize({K,V}) when is_binary(K) ->
  {atomize(K),atomize(V)};

atomize(T) when is_binary(T) -> binary_to_atom(T,utf8);
atomize(T) -> T.


j(D,F) -> j([D,F]).
j(L) -> filename:join(L).
