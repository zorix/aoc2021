program Hello;
var
  die_count: Int64 = 1;
  die_rolled: Int64 = 0;

type
  Player = record
    score: Int64;
    pos: Int64;
  end;

// 98 99 100
// 99 100 1
// 100 1 2
// 1 2 3
function iter(): integer;
begin
  if die_count <= 98 then
    iter := (die_count+1)*3
  else if die_count = 99 then
    iter := 99 + 100 + 1
  else if die_count = 100 then
    iter := 100 + 1 + 2;
  //writeln('die c ', die_count, ' ', iter);

  die_count := (die_count + 3);
  if die_count > 100 then
    die_count := die_count - 100;

  die_rolled := die_rolled + 3;
end;

function update(player: Player): Player;
begin
  update.pos := player.pos + iter();
  if (update.pos > 10) then
  begin
    update.pos := update.pos mod 10;
    if update.pos = 0 then
      update.pos := 10;
  end;
  update.score := player.score + update.pos;
end;

var
a : Player;
b : Player;
begin
  a.pos := 3;
  b.pos := 10;
  a.score := 0;
  b.score := 0;

  while 1=1 do
  begin
    a := update(a);
    if a.score >= 1000 then
    begin
        writeln ('PartA ', b.score * die_rolled);
        break;
    end;
    b := update(b);
    if b.score >= 1000 then
    begin
        writeln ('PartA ', a.score * die_rolled);
        break;
    end;
  end;
end.
