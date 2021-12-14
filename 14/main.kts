import java.io.File

fun readFileAsLinesUsingUseLines(fileName: String): List<String>
  = File(fileName).useLines { it.toList() }

data class PolyPair(val c1: Char, val c2: Char)
data class CacheKey(val p: PolyPair, val iter: Int)

val cache = HashMap<CacheKey, Map<Char,Long>>()

fun <K> increment(map: MutableMap<K, Long>, key: K, by: Long) {
    when (val count = map[key])
    {
        null -> map[key] = by
        else -> map[key] = count + by
    }
}

fun step(p: PolyPair, mapping: Map<PolyPair,Char>, iter: Int): Map<Char, Long> {
  val cacheValue = cache.get(CacheKey(p, iter))
  if (cacheValue != null) {
    return cacheValue
  }
  if (iter == 0) {
    val counts = HashMap<Char, Long>()
    increment(counts, p.c1, 1)
    increment(counts, p.c2, 1)
    cache.put(CacheKey(p, iter), counts)
    return counts;
  }

  val counts = HashMap<Char, Long>()
  val c = mapping[p]!!
  var map1 = step(PolyPair(p.c1, c), mapping, iter-1)
  var map2 = step(PolyPair(c, p.c2), mapping, iter-1)
  for ((k, v) in map1) {
    increment(counts, k, v)
  }
  for ((k, v) in map2) {
    increment(counts, k, v)
  }
  increment(counts, c, -1)
  cache.put(CacheKey(p, iter), counts)
  return counts;
}

fun calc(polymer: String, mapping: Map<PolyPair,Char>, iter: Int): Long {
  var counts = HashMap<Char,Long>()
  for (idx in 0..(polymer.length-2)) {
    val map = step(PolyPair(polymer[idx], polymer[idx+1]), mapping, iter)
    for ((k, v) in map) {
      increment(counts, k, v)
    }
    if (idx != 0)
      increment(counts, polymer[idx], -1)
  }

  val sorted = counts.toList().sortedWith(compareBy { it.second })
  val most = sorted[sorted.size-1].second
  val least = sorted[0].second
  return most - least
}

val lines = readFileAsLinesUsingUseLines("./input.txt")

val polymer = lines[0]
val mappingStr = lines.slice(2..(lines.size-1))

val map = HashMap<PolyPair,Char>()
for (v in mappingStr) {
  val vals = v.split(" -> ")
  map.put(PolyPair(vals[0][0], vals[0][1]), vals[1][0])
}


println("PartA: ${calc(polymer, map, 10)}")
println("PartB: ${calc(polymer, map, 40)}")
