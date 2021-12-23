import Foundation

var map = Array(repeating: Array(repeating: Array(repeating: false, count: 101), count: 101), count: 101)

let contents = try! String(contentsOfFile: "part_a.txt")
let lines = contents.split(separator:"\n")

lines.forEach { line in
  let vals = line.split(separator:" ")
  let on = vals[0] == "on"
  let x0 = Int(vals[1])!
  let x1 = Int(vals[2])!
  let y0 = Int(vals[3])!
  let y1 = Int(vals[4])!
  let z0 = Int(vals[5])!
  let z1 = Int(vals[6])!

  let xMin = 50 + min(x0, x1)
  let yMin = 50 + min(y0, y1)
  let zMin = 50 + min(z0, z1)
  let xMax = 50 + max(x0, x1)
  let yMax = 50 + max(y0, y1)
  let zMax = 50 + max(z0, z1)

  print("\(xMin) \(xMax) \(yMin) \(yMax) \(zMin) \(zMax)")
  
  for x in xMin...xMax {
    for y in yMin...yMax {
      for z in zMin...zMax {
        map[x][y][z] = on
      }
    }
  }
}

var sum = 0
for x in map {
  for y in x {
    for v in y {
      sum +=  v ? 1 : 0
    }
  }
}

print("PartA \(sum)")
