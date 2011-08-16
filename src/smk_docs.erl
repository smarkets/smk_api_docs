-module(smk_docs).

-export([build/1, build_site/1]).

-define(BUILD, "build").
-define(ORDER, [
    payload,
    sequenced,
    seq,
    message,
    'login',
    'login-response',
    'logout',
    'logout-reason',
    'heartbeat',
    'replay',

    'events-request',
    'sport-by-date',
    'sport-by-date-type',
    'events',
    'event-info',
    'event-info-list',
    'event-type',
    'event-category',
    'market-info',
    'market-info-list',
    'contract-info',
    'contract-info-list',
    'contract-type',
    'entity-relationship',
    'entity-relationship-list',
    'entity-relationship-type',

    'market-request',
    'market-subscription',
    'market-unsubscription',
    'market-quotes-request',
    'market-quotes',
    'contract-quotes-list',
    'contract-quotes',
    'contract-quote',
    'quote-list',
    'quote',
    'execution-list',
    'execution',

    'order-create',
    'order-accepted',
    'order-rejected',
    'order-rejected-reason',
    'order-executed',
    'order-cancel',
    'order-cancelled',
    'order-cancelled-reason',
    'order-invalid',
    'order-invalid-reason-list',
    'order-invalid-reason',

    'id-invalid',

    'microseconds',
    'date',
    'time',
    'int-list'
  ]).

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

  SetoSpec = atomize(jsx:json_to_term(SetoBin)),
  SetoPiqDef = proplists:get_value(piqdef, SetoSpec),

  EtoSpec = atomize(jsx:json_to_term(EtoBin)),
  EtoPiqDef =
    lists:foldl(fun extend/2,
      proplists:get_value(piqdef, EtoSpec),
      proplists:get_value(extend, SetoSpec)
    ),

  Order = order(),
  PiqDef = lists:sort(fun(A,B) -> sort(A,B,Order) end, EtoPiqDef ++ SetoPiqDef),

  _Ctx = [
    {version, Vsn},
    {order, [proplists:get_value(name, proplists:get_value(def, DefDef)) || DefDef <- PiqDef]},
    {piqdef, PiqDef}
  ].



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
find(Name, [[_,{def,[{name,Name}|_]}|_]=H|_]) ->
  H;
find(Name, [_|T]) ->
  find(Name, T).

replace(Name, Def, Defs) ->
  replace(Name, Def, Defs, []).

replace(Name, Def, [[_,{def,[{name,Name}|_]}|_]|T], Acc) ->
  lists:reverse(Acc) ++ [Def|T];
replace(Name, Def, [H|T], Acc) ->
  replace(Name, Def, T, [H|Acc]).
   

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

description([{<<"name">>,Name}|_]) ->
  File = priv_dir(j("types",<<Name/binary,".html">>)),
  case file:read_file(File) of
    {ok, Description} -> Description;
    _ -> ""
  end;
description(_) -> "".

atomize([{Type,Def}]) when Type =:= <<"alias">>
                   orelse Type =:= <<"enum">>
                   orelse Type =:= <<"record">>
                   orelse Type =:= <<"variant">> ->
  [
    {piqi_type,atomize(Type)},
    {def, atomize(Def)},
    {description,description(Def)}
  ];
atomize([{<<"list">>,[{<<"name">>,_},{<<"type">>,_}]=Def}]) ->
  [
    {piqi_type,list},
    {def, atomize(Def)},
    {description,description(Def)}
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

order() ->
  {L,_} =
    lists:foldl(
      fun(Name, {Acc,I}) ->
        {[{Name,I}|Acc],I+1}
      end, {[],1}, ?ORDER),
  lists:reverse(L).

sort(A, B, Order) ->
  pos(A, Order) < pos(B, Order).

pos(DefDef, Order) ->
  Name = proplists:get_value(name, proplists:get_value(def, DefDef)),
  proplists:get_value(Name, Order, 99999).
