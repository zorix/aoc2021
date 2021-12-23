import Foundation

var X: [Int] = Array()
var Y: [Int] = Array()
var Z: [Int] = Array()

let contents = try! String(contentsOfFile: "input.txt")
let lines = contents.split(separator:"\n")

lines.forEach { line in
  let vals = line.split(separator:" ")
  let x0 = Int(vals[1])!
  let x1 = (Int(vals[2])!) + 1
  let y0 = Int(vals[3])!
  let y1 = (Int(vals[4])!) + 1
  let z0 = Int(vals[5])!
  let z1 = (Int(vals[6])!) + 1

  X.append(x0)
  X.append(x1)
  Y.append(y0)
  Y.append(y1)
  Z.append(z0)
  Z.append(z1)
}

X.sort()
Y.sort()
Z.sort()

var map = Array(repeating: Array(repeating: Array(repeating: false, count: X.count), count: Y.count), count: Z.count)

lines.forEach { line in
  let vals = line.split(separator:" ")
  let on = vals[0] == "on"
  let x0 = X.firstIndex(of: Int(vals[1])!)!
  let x1 = X.firstIndex(of: (Int(vals[2])!) + 1)!
  let y0 = Y.firstIndex(of: Int(vals[3])!)!
  let y1 = Y.firstIndex(of: (Int(vals[4])!) + 1)!
  let z0 = Z.firstIndex(of: Int(vals[5])!)!
  let z1 = Z.firstIndex(of: (Int(vals[6])!) + 1)!

  for x in x0..<x1 {
    for y in y0..<y1 {
      for z in z0..<z1 {
        map[x][y][z] = on
      }
    }
  }
}

var sum:Int64 = 0
for x in 0..<X.count {
  for y in 0..<Y.count {
    for z in 0..<Z.count {
      if map[x][y][z] {
        sum += Int64(X[x+1] - X[x]) * Int64(Y[y+1] - Y[y]) * Int64(Z[z+1] - Z[z])
      }
    }
  }
}

print("PartB \(sum)")
