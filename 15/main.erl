%% -*- erlang -*-

readLines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  re:split(Data, "\n").

convToInt(Data) ->
  M = lists:map(fun(X)->convLineToInt(re:split(X, "")) end, Data),
  M1 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+1) end, X) end, M),
  M2 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+2) end, X) end, M),
  M3 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+3) end, X) end, M),
  M4 = lists:map(fun(X)-> lists:map(fun(Y)-> wrapInt(Y+4) end, X) end, M),
  L = M ++ M1 ++ M2 ++ M3 ++ M4,
  L1 = lists:map(fun(X)-> array:from_list(X) end, L),
  array:from_list(L1).

wrapInt(I) ->
  (I rem 10) + (I div 10).

convLineToInt(Data) ->
  D = lists:map(fun(X)->strToInt(X) end, Data),
  L = lists:reverse(tl(lists:reverse(D))), %% drop last element (some error)
  L ++ [wrapInt(X+1) || X <- L] ++ [wrapInt(X+2) || X <- L] ++ [wrapInt(X+3) || X <- L] ++ [wrapInt(X+4) || X <- L].

strToInt(Str) ->
  {V,_}=string:to_integer(Str),
  V.

createWeights(Size) ->
  array:from_list([array:from_list([99999 || _ <- lists:seq(1, Size)]) || _ <- lists:seq(1, Size)]).


setWeight(RowIdx, ColIdx, Size, Val) ->
  wat:set(RowIdx*Size+ColIdx, Val).

getWeight(RowIdx, ColIdx, Size) ->
  wat:get(RowIdx*Size+ColIdx).

setGridVal(Grid, RowIdx, ColIdx, Val) ->
  Row = array:get(RowIdx, Grid),
  NewRow = setCol(Row, ColIdx, Val),
  array:set(RowIdx, NewRow, Grid).

setCol(Row, ColIdx, Val) ->
  array:set(ColIdx, Val, Row).

getGridVal(Grid, RowIdx, ColIdx) ->
  Row = array:get(RowIdx, Grid),
  array:get(ColIdx, Row).

checkConnection(Graph, Size, NodeWeight, ToX, ToY, Cost) ->
  if
    (ToX >= 0) and (ToX < Size) and (ToY >= 0) and (ToY < Size) ->
      SubNodeWeight = getWeight(ToY, ToX, Size),
      %io:format("~w to ~w new cost ~w ~w\n", [[FromX, FromY], [ToX, ToY], NodeWeight, SubNodeWeight]),
      if 
        NodeWeight+Cost < SubNodeWeight ->
          setWeight(ToY, ToX, Size, NodeWeight+Cost),
          %W = setGridVal(Weights, ToY, ToX, NodeWeight+Cost),
          visitNode(Graph, Size, ToX, ToY),
          0;
        true ->
          0
      end;
    true ->
      0
  end.

getCost(Graph, X, Y, Size) ->
  if 
    (X >= 0) and (X < Size) and (Y >= 0) and (Y < Size) ->
      getGridVal(Graph, Y, X);
    true ->
      10000000
  end.

visitNode(Graph, Size, X, Y) ->
  W = getWeight(Y, X, Size),
  N = [
    {X+1, Y, getCost(Graph, X+1, Y, Size)},
    {X-1, Y, getCost(Graph, X-1, Y, Size)},
    {X, Y+1, getCost(Graph, X, Y+1, Size)},
    {X, Y-1, getCost(Graph, X, Y-1, Size)}
  ],
  N2 = lists:keysort(3, N),
  %io:format("~w\n", [N2]),
  {X1, Y1, C1} = lists:nth(1, N2),
  checkConnection(Graph, Size, W, X1, Y1, C1),
  {X2, Y2, C2} = lists:nth(2, N2),
  checkConnection(Graph, Size, W, X2, Y2, C2),
  {X3, Y3, C3} = lists:nth(3, N2),
  checkConnection(Graph, Size, W, X3, Y3, C3),
  {X4, Y4, C4} = lists:nth(4, N2),
  checkConnection(Graph, Size, W, X4, Y4, C4),
  0.

main(_) ->
  Lines = readLines("./input.txt"),
  G = convToInt(Lines),
  Size = array:size(G),
  io:format("Size ~w\n", [wat:get(Size*Size)]),
  wat:set(0, 0),
  T = visitNode(G, Size, 0, 0),
  io:format("Result: ~w\n", [getWeight(Size-1, Size-1, Size)]),
  %io:format("row2: ~w\n", [array:get(1, T)]),
  %io:format("row3: ~w\n", [array:get(2, T)]),
  %io:format("row100: ~w\n", [lists:nth(100, T)]),
  io:format("grid: ~w\n", [T]),
  1.
