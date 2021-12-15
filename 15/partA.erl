%% -*- erlang -*-

readLines(FileName) ->
  {ok, Data} = file:read_file(FileName),
  re:split(Data, "\n").

convToInt(Data) ->
  array:from_list(lists:map(fun(X)->convLineToInt(re:split(X, "")) end, Data)).

convLineToInt(Data) ->
  D = lists:map(fun(X)->strToInt(X) end, Data),
  array:from_list(lists:reverse(tl(lists:reverse(D)))). %% drop last element (some error)

strToInt(Str) ->
  {V,_}=string:to_integer(Str),
  V.

createWeights(Size) ->
  array:from_list([array:from_list([400 || _ <- lists:seq(1, Size)]) || _ <- lists:seq(1, Size)]).


setGridVal(Grid, RowIdx, ColIdx, Val) ->
  Row = array:get(RowIdx, Grid),
  NewRow = setCol(Row, ColIdx, Val),
  array:set(RowIdx, NewRow, Grid).

setCol(Row, ColIdx, Val) ->
  array:set(ColIdx, Val, Row).

getGridVal(Grid, RowIdx, ColIdx) ->
  Row = array:get(RowIdx, Grid),
  array:get(ColIdx, Row).

checkConnection(Graph, Weights, Size, FromX, FromY, ToX, ToY) ->
  if
    (ToX >= 0) and (ToX < Size) and (ToY >= 0) and (ToY < Size) ->
      NodeWeight = getGridVal(Weights, FromY, FromX),
      SubNodeWeight = getGridVal(Weights, ToY, ToX),
      Cost = getGridVal(Graph, ToY, ToX),
      if 
        NodeWeight+Cost < SubNodeWeight ->
          %%io:format("~w to ~w new cost ~w\n", [[FromX, FromY], [ToX, ToY], NodeWeight+Cost]),
          W = setGridVal(Weights, ToY, ToX, NodeWeight+Cost),
          visitNode(Graph, W, Size, ToX, ToY);
        true ->
          Weights
      end;
    true ->
      Weights
  end.

visitNode(Graph, Weights, Size, X, Y) ->
  W = checkConnection(Graph, Weights, Size, X, Y, X+1, Y),
  W1 = checkConnection(Graph, W, Size, X, Y, X-1, Y),
  W2 = checkConnection(Graph, W1, Size, X, Y, X, Y+1),
  checkConnection(Graph, W2, Size, X, Y, X, Y-1).

main(_) ->
  Lines = readLines("./input.txt"),
  Size = length(Lines),
  G = convToInt(Lines),
  W = createWeights(Size),
  W2 = setGridVal(W, 0, 0, 0),
  T = visitNode(G, W2, Size, 0, 0),
  %%io:format("row1: ~w\n", [array:get(1, T)]),
  %%io:format("row2: ~w\n", [array:get(2, T)]),
  %%io:format("row3: ~w\n", [array:get(3, T)]),
  %%io:format("row100: ~w\n", [lists:nth(100, T)]),
  io:format("grid: ~w\n", [T]),
  1.
