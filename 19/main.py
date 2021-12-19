from dataclasses import dataclass
from collections import Counter
from typing import List, Tuple

@dataclass
class Mutator:
  negX: bool = False
  negY: bool = False
  negZ: bool = False

  oX: bool = False
  oY: bool = False
  oZ: bool = False

mutators: List[Mutator] = []
for negX in [False, True]:
  for negY in [False, True]:
    for negZ in [False, True]:
      for oX in [False, True]:
        for oY in [False, True]:
          for oZ in [False, True]:
            mutators.append(Mutator(negX, negY, negZ, oX, oY, oZ))

@dataclass
class Point3D:
  x: int = 0
  y: int = 0
  z: int = 0

  def __hash__(self) -> int:
      return self.x + self.y*100000 + self.z*500000

  def __add__(self, p):
    if type(p) is list:
      return list(map(lambda x: self + x, p))
    return Point3D(self.x + p.x, self.y + p.y, self.z + p.z)

  def __sub__(self, p):
    if type(p) is list:
      return list(map(lambda x: self - x, p))
    return Point3D(self.x - p.x, self.y - p.y, self.z - p.z)

  def __mul__(self, m: List[Mutator]):
    if type(m) is list:
      return list(map(lambda x: self * x, m))
    p = Point3D(self.x, self.y, self.z)
    if m.negX:
      p.x *= -1
    if m.negY:
      p.y *= -1
    if m.negZ:
      p.z *= -1
    if m.oX:
      p.y, p.z = p.z, p.y
    if m.oY:
      p.x, p.z = p.z, p.x
    if m.oZ:
      p.x, p.y = p.y, p.x
    return p

  def dist(self, p):
    return abs(self.x-p.x) + abs(self.y-p.y) + abs(self.z-p.z)


def newPoint(s: str):
  s = list(map(int, s.split(",")))
  return Point3D(*s)


def find_scanner_pos(baseScanner: List[Point3D], targetScanner: List[Point3D]) -> Tuple[List[Point3D],Mutator]:
  for mutation in mutators:
    c = []
    for tp in targetScanner:
      t = tp * mutation
      for bp in baseScanner:
        c.append(bp - t)

    count = Counter(c)
    most = count.most_common(1)
    if len(most) == 0:
      continue
    
    most = most[0]
    #print(most)
    if most[1] < 12:
      continue
    return most[0], mutation

  return None


def get_scanner_and_beacons(scanners_data: List[List[Point3D]]):
  beacons = scanners_data[0]
  remaining_scanners_data = scanners_data[1:]

  scanners = [ Point3D(0,0,0) ]
  while len(remaining_scanners_data) > 0:
    for idx, rem_scan in enumerate(remaining_scanners_data):
      match = find_scanner_pos(beacons, rem_scan)
      if match:
        del remaining_scanners_data[idx]
        scanners.append(match[0])
        print(len(scanners), match[0])
        updated_beacons = list(map(lambda x: x * match[1] + match[0], rem_scan))
        beacons = list(set(beacons + updated_beacons))

  return beacons, scanners

def part_a(beacons: List[Point3D]):
  print("PartA:", len(beacons))


def part_b(scanners: List[Point3D]):
  dists = []
  for s1 in scanners:
    for s2 in scanners:
      dists.append(s1.dist(s2))

  dists = list(set(dists))
  dists.sort()
  print("PartB:", dists[-1])


if __name__ == "__main__":
  scanners_data = open('input.txt').read().split('\n\n')
  scanners_data = list(map(lambda x: list(map(newPoint, x.split('\n')[1:])), scanners_data))

  beacons, scanners = get_scanner_and_beacons(scanners_data)

  part_a(beacons)
  part_b(scanners)
