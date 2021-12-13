import 'dart:io';
import 'dart:core';
import 'dart:collection';
import 'dart:math';

class Point {
  Point(this.x, this.y);
  final int x;
  final int y;

  bool operator ==(Object o) {
    if (o is Point) {
      return this.x == o.x && this.y == o.y;
    }
    return false;
  }
  int get hashCode => this.x * 100 + this.y;

  String toString() {
    return "Point[x=${this.x} y=${this.y}]";
  }
}


class Along {
  Along(this.type, this.v);
  final String type;
  final int v;
}

Set<Point> foldX(Set<Point> ps, int col) {
  Set<Point> newPs = new HashSet<Point>();
  ps.forEach((p) => {
    if (p.x < col) {
      newPs.add(p)
    } else {
      newPs.add(Point(col - (p.x - col), p.y))
    }
  });
  return newPs;
}

Set<Point> foldY(Set<Point> ps, int row) {
  Set<Point> newPs = new HashSet<Point>();
  ps.forEach((p) => {
    if (p.y < row) {
      newPs.add(p)
    } else {
      newPs.add(Point(p.x, row - (p.y - row)))
    }
  });
  return newPs;
}

Set<Point> fold(Set<Point> points, Along a) {
  switch (a.type) {
    case "x":
      points = foldX(points, a.v);
      break;
    case "y":
      points = foldY(points, a.v);
      break;
  }

  return points;
}

void print_dots(Set<Point> points) {
  var max_point = points.reduce((c, x) => Point(max(c.x, x.x), max(c.y, x.y)));

  for (int y = 0; y <= max_point.y; y++) {
    for (int x = 0; x <= max_point.x; x++) {
      if (points.contains(Point(x, y))) {
        stdout.write("#");
      } else {
        stdout.write(".");
      }
    }
    stdout.write("\n");
  }
}

void main() async {
  print("=============");
  var lines = await (new File("./input.txt").readAsLines());

  Set<Point> points = new HashSet<Point>();
  List<Along> alongs = [];
  for (var l in lines) {
    if (l.contains(",")) {
      var xy = l.split(",");
      int x = int.parse(xy[0]);
      int y = int.parse(xy[1]);
      points.add(Point(x, y));
    }
    else if (l.contains("fold along")) {
      var v = l.split(" ");
      var v2 = v[2].split("=");
      alongs.add(Along(v2[0], int.parse(v2[1])));
    }
  }

  var partA = fold(points, alongs[0]);
  print("PartA: ${partA.length}");

  var partB = points;
  for (var a in alongs) {
    partB = fold(partB, a);
  }

  print("PartB:");
  print_dots(partB);
}
