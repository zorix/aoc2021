def line
new File("input.txt").withReader('UTF-8') { reader ->
  line = reader.readLine()
}

String hexToBin(String s) {
  return new BigInteger(s, 16).toString(2)
}

long binToInt(String s) {
  return new BigInteger(s, 2).longValue()
}

def findPrefix(String s) {
  def f = binToInt hexToBin(s.substring(0, 1))
  def prefix = ""
  if (f == 0) {
    prefix = "0000" + findPrefix(s.substring(1))
  } else if (f == 1) {
    prefix = "000"
  } else if (f < 4) {
    prefix = "00"
  } else if (f < 8) {
    prefix = "0"
  }

  return prefix
}

def loadHexToBin(String s) {
  def prefix = findPrefix s
  return prefix + hexToBin(s)
}

def readLiteral(String packet) {
  def stringVal = ""
  def read = 0
  while (true) {
    def b = packet.substring(0, 1)
    packet = packet.substring 1
    def bytes = packet.substring(0, 4)
    packet = packet.substring 4
    stringVal = stringVal + bytes
    
    read += 5

    if (b == "0") {
      break
    }
  }

  return [read, binToInt(stringVal)]
}

versionSum = 0
def parsePacket(String packet) {
  def read = 0
  def version = binToInt packet.substring(0, 3)
  packet = packet.substring 3
  read += 3
  //println "version " + version
  versionSum += version

  def typeId = binToInt packet.substring(0, 3)
  packet = packet.substring 3
  read += 3
  //println "typeId " + typeId

  def val = 0
  // literal
  if (typeId == 4) {
    def (r, v) = readLiteral packet
    packet = packet.substring r
    read += r
    //println "literal values: " + v + " " + read
    val = v
  } else {
    // operation
    def l = packet.substring(0, 1)
    packet = packet.substring 1
    read += 1
    //println "l " + l

    def length = 0
    def subPackets = 0
    if (l == "0") {
      length = binToInt packet.substring(0, 15)
      packet = packet.substring 15
      read += 15
    } else {
      subPackets = binToInt packet.substring(0, 11)
      packet = packet.substring 11
      read += 11
    }

    //println "length " + length
    //println "subPackets " + subPackets

    def values = []
    while (length > 0) {
      def (r, v) = parsePacket packet
      packet = packet.substring r
      length -= r
      read += r
      values.add(v)
    }

    while (subPackets > 0) {
      def (r, v) = parsePacket packet
      packet = packet.substring r
      subPackets -= 1
      read += r
      values.add(v)
    }

    //println values
    switch(typeId) {
      case 0:
        val = values.sum()
        break
      case 1:
        val = values.inject {x1, x2 -> x1*x2}
        break
      case 2:
        val = values.min()
        break
      case 3:
        val = values.max()
        break
      case 5:
        val = values[0] > values[1] ? 1 : 0
        break
      case 6:
        val = values[0] < values[1] ? 1 : 0
        break
      case 7:
        val = values[0] == values[1] ? 1 : 0
        break
    }
  }

  return [read, val]
}

versionSum = 0
packet = loadHexToBin(line)
println "PartB: " + parsePacket(packet)
println "PartA: " + versionSum

