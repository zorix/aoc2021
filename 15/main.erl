%% -*- erlang -*-

readLines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  re:split(Data, "\n").

convToIntPartA(Data) ->
  M = lists:map(fun(X)->convLineToIntPartA(re:split(X, "")) end, Data),
  L = lists:map(fun(X)-> array:from_list(X) end, M),
  array:from_list(L).

convToIntPartB(Data) ->
  M = lists:map(fun(X)->convLineToIntPartB(re:split(X, "")) end, Data),
  M1 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+1) end, X) end, M),
  M2 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+2) end, X) end, M),
  M3 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+3) end, X) end, M),
  M4 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+4) end, X) end, M),
  L = M ++ M1 ++ M2 ++ M3 ++ M4,
  L1 = lists:map(fun(X)-> array:from_list(X) end, L),
  array:from_list(L1).

wrapInt(I) ->
  (I rem 10) + (I div 10).

convLineToIntPartA(Data) ->
  D = lists:map(fun(X)->strToInt(X) end, Data),
  L = lists:reverse(tl(lists:reverse(D))), %% drop last element (some error)
  L.

convLineToIntPartB(Data) ->
  L = convLineToIntPartA(Data),
  L ++ [wrapInt(X+1) || X <- L] ++ [wrapInt(X+2) || X <- L] ++ [wrapInt(X+3) || X <- L] ++ [wrapInt(X+4) || X <- L].

strToInt(Str) ->
  {V,_}=string:to_integer(Str),
  V.

createWeights(Size) ->
  array:from_list([array:from_list([99999 || _ <- lists:seq(1, Size)]) || _ <- lists:seq(1, Size)]).

setGridVal(Grid, RowIdx, ColIdx, Val) ->
  Row = array:get(RowIdx, Grid),
  NewRow = setCol(Row, ColIdx, Val),
  array:set(RowIdx, NewRow, Grid).

setCol(Row, ColIdx, Val) ->
  array:set(ColIdx, Val, Row).

getGridVal(Grid, RowIdx, ColIdx) ->
  Row = array:get(RowIdx, Grid),
  array:get(ColIdx, Row).

updateNode(Weights, {X, Y, Cost}) ->
  NodeWeight = getGridVal(Weights, Y, X),
  if 
    Cost < NodeWeight ->
      W = setGridVal(Weights, Y, X, Cost),
      {W, [{X, Y, Cost}]};
    true ->
      {Weights, []}
  end.

getCost(Graph, X, Y, Size, Cost) ->
  if 
    (X >= 0) and (X < Size) and (Y >= 0) and (Y < Size) ->
      [{X, Y, Cost + getGridVal(Graph, Y, X)}];
    true ->
      []
  end.

nodesToUpdate(_Graph, _Size, []) ->
  [];

nodesToUpdate(Graph, Size, [P]) ->
  {X, Y, Cost} = P,
  getCost(Graph, X+1, Y, Size, Cost)
  ++ getCost(Graph, X-1, Y, Size, Cost)
  ++ getCost(Graph, X, Y+1, Size, Cost)
  ++ getCost(Graph, X, Y-1, Size, Cost).

visitNode(_Graph, Weights, _Size, []) ->
  Weights;

visitNode(Graph, Weights, Size, [ToCheck|Rem]) ->
  { W, Updated } = updateNode(Weights, ToCheck),
  NewNodesToCheck = lists:keymerge(3, Rem, lists:keysort(3, nodesToUpdate(Graph, Size, Updated))),
  visitNode(Graph, W, Size, NewNodesToCheck).

partA(Lines) ->
  G = convToIntPartA(Lines),
  Size = array:size(G),
  W = createWeights(Size),
  %io:format("Size ~w\n", [Size]),
  T = visitNode(G, W, Size, [{0, 0, 0}]),
  io:format("PartA: ~w\n", [array:get(Size-1, array:get(Size-1, T))]),
  1.

partB(Lines) ->
  G = convToIntPartB(Lines),
  Size = array:size(G),
  W = createWeights(Size),
  %io:format("Size ~w\n", [Size]),
  T = visitNode(G, W, Size, [{0, 0, 0}]),
  io:format("PartB: ~w\n", [array:get(Size-1, array:get(Size-1, T))]),
  1.


main(_) ->
  Lines = readLines("./input.txt"),
  partA(Lines),
  partB(Lines),
  1.
