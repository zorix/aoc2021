import scala.io.Source

abstract class SnailfishNode {
  def isNumberNode: Boolean
  var value: Int
  def explodeLeft(v: Int): SnailfishNode
  def explodeRight(v: Int): SnailfishNode

  def reduce: SnailfishNode = {
    var work = true
    var node = this
    while (work) {
      val explodeResult = node.explode(0)
      node = explodeResult(0)
      if explodeResult(1) then {
        work = true
      } else {
        val splitResult = node.split
        node = splitResult(0)
        if splitResult(1) then {
          work = true
        } else {
          work = false
        }
      }
    }
    return node
  }

  def split: (SnailfishNode, Boolean)
  def explode(depth: Int): (SnailfishNode, Boolean, Int, Int)
  def magnitued: Long
}

class SnailfishNumber(initValue: Int) extends SnailfishNode {
  override def toString: String = {
    return value.toString
  }
  def isNumberNode: Boolean = true
  var value: Int = initValue

  def explodeLeft(v: Int) = new SnailfishNumber(value + v)
  def explodeRight(v: Int) = new SnailfishNumber(value + v)
  
  def split = {
    //println("Node value update: " + value)
    if value >= 10 then {
      //println("above 10 => " + value + " " + (value/2.0).floor.toInt + " " + (value/2.0).ceil.toInt)
      val newNode = new SnailfishPair(new SnailfishNumber((value/2.0).floor.toInt), new SnailfishNumber((value/2.0).ceil.toInt))
      return (newNode, true)
    }
    return (this, false)
  }

  def explode(depth: Int) = {
    return (this, false, 0, 0)
  }

  def magnitued: Long = {
    return value
  }
}

class SnailfishPair(l: SnailfishNode, r: SnailfishNode) extends SnailfishNode {
  override def toString: String = {
    return "[" + l.toString + "," + r.toString + "]"
  }

  def isNumberNode: Boolean = false
  var value: Int = -999999

  def explodeLeft(v: Int) = {
    return new SnailfishPair(l.explodeLeft(v), r)
  }

  def explodeRight(v: Int) = {
    return new SnailfishPair(l, r.explodeRight(v))
  }

  def split = {
    {
      val lRes = l.split
      if lRes(1) == true then {
        return (new SnailfishPair(lRes(0), r), true)
      }
    }

    {
      val rRes = r.split
      if rRes(1) == true then {
        return (new SnailfishPair(l, rRes(0)), true)
      }
    }

    return (this, false)
  }

  def explode(depth: Int) = {
    if depth == 4 && l.isNumberNode && r.isNumberNode then {
      return (new SnailfishNumber(0), true, l.value, r.value)
    }

    {
      val lRes = l.explode(depth + 1)
      if lRes(1) == true then {
        return (new SnailfishPair(lRes(0), r.explodeLeft(lRes(3))), true, lRes(2), 0)
      }
    }

    {
      val rRes = r.explode(depth + 1)
      if rRes(1) == true then {
        return (new SnailfishPair(l.explodeRight(rRes(2)), rRes(0)), true, 0, rRes(3))
      }
    }

    return (this, false, 0, 0)
  }

  def magnitued: Long = {
    return l.magnitued * 3 + r.magnitued * 2
  }
}

def parseNode(s: String): SnailfishNode = {
  if s(0) != '[' then {
    throw new Exception("Invalid node")
  }
  var str = s.substring(1)
  var depth = 0
  for ((x,i) <- str.view.zipWithIndex) {
    if x == '[' then {
      depth += 1
    }
    else if x == ']' then {
      depth -= 1
    }
    else if x == ',' && depth == 0 then {
      val leftNodeStr = str.substring(0, i)
      val rightNodeStr = str.substring(i+1, str.length-1)
      //println(s + " => " + leftNodeStr + "<=>" + rightNodeStr)

      val leftNode =  if leftNodeStr(0) == '[' then parseNode(leftNodeStr) else new SnailfishNumber(leftNodeStr.toInt)
      val rightNode = if rightNodeStr(0) == '[' then parseNode(rightNodeStr) else new SnailfishNumber(rightNodeStr.toInt)
      return new SnailfishPair(leftNode, rightNode)
    }
  }
  throw new Exception("Missing closing bracket in node:" + s)
}

def reduceNode(node: SnailfishNode): SnailfishNode = {
  var reducedNode = node.reduce
  return reducedNode
}

val lines = Source.fromFile("input.txt").mkString.split("\n").map(parseNode)

def partA = {
  var sum = lines(0)
  for (line <- lines.tail) {
    sum = (new SnailfishPair(sum, line)).reduce
    //println("iter: " + sum)
  }
  println("PartA: " + sum.magnitued)
}

def partB = {
  var magnituedes = List[Long]()
  for (comb <- lines.toList.combinations(2)) {
    magnituedes = magnituedes :+ (new SnailfishPair(comb(0), comb(1))).reduce.magnitued
  }

  println("PartA: " + magnituedes.max)
}


def main(args: Array[String]): Unit = {
  partA
  partB
}
