long GetFish(long[] fish, int days) {
  foreach (var d in Enumerable.Range(0, days)) {
    var toAdd = fish[0];

    var c = fish.Count();
    for (var i = 1; i < c; ++i) {
      fish[i-1] = fish[i];
    }
    fish[6] += toAdd;
    fish[8] += toAdd;
  }

  return fish.Sum();
}


long[] fish = new long[10];
var data = System.IO.File.ReadAllText("input.txt").Split(",");
foreach (var f in data.Select(x => Int32.Parse(x))) {
  fish[f] += 1;
}


var countA = GetFish((long[])fish.Clone(), 80);
Console.WriteLine("fishA: {0}", countA);

var countB = GetFish((long[])fish.Clone(), 256);
Console.WriteLine("fishB: {0}", countB);
