import std.algorithm, std.conv, std.functional,
  std.math, std.regex, std.stdio, std.file, std.array;
import std.typecons : tuple, Tuple;


int[int] createMapper(string mapTo)
{
  int[int] mapper;

  for (int i = 0; i < 512; ++i) {
    mapper[i] = mapTo[i] == '#' ? 1 : 0;
  }

  return mapper;
}


int getPixel(int[Tuple!(int, int)] lights, int y, int x) {
  return lights.get(tuple(y, x), 0);
}

int calcKeyForPoint(int[Tuple!(int, int)] lights, int x, int y) {
  int val = 0;
  val |= getPixel(lights, y-1, x-1) << 8;
  val |= getPixel(lights, y-1, x) << 7;
  val |= getPixel(lights, y-1, x+1) << 6;

  val |= getPixel(lights, y, x-1) << 5;
  val |= getPixel(lights, y, x) << 4;
  val |= getPixel(lights, y, x+1) << 3;

  val |= getPixel(lights, y+1, x-1) << 2;
  val |= getPixel(lights, y+1, x) << 1;
  val |= getPixel(lights, y+1, x+1) << 0;

  return val;
}

int[Tuple!(int, int)] enhance(int[Tuple!(int, int)] lights, int[int] mapper, int size, int fill) {

  int newSize = size +1;
  for (int y = -2; y <= newSize; ++y) {
    lights[tuple(y, -2)] = fill;
    lights[tuple(y, -1)] = fill;
    lights[tuple(y, size)] = fill;
    lights[tuple(y, size+1)] = fill;
  }
  for (int x = -2; x <= newSize; ++x) {
    lights[tuple(-2, x)] = fill;
    lights[tuple(-1, x)] = fill;
    lights[tuple(size, x)] = fill;
    lights[tuple(size+1, x)] = fill;
  }

  int[Tuple!(int, int)] newLights;
  for (int y = -1; y < newSize; ++y) {
    for (int x = -1; x < newSize; ++x) {
      //writeln(x, y, calcKeyForPoint(lights, x, y));
      if (mapper[calcKeyForPoint(lights, x, y)] == 1) {
        newLights[tuple(y+1, x+1)] = 1;
      }
    }
  }

  return newLights;
}

void printOutput(int[Tuple!(int, int)] lights, int size) {
  for (int y = 0; y < size; ++y) {
    for (int x = 0; x < size; ++x) {
      if (getPixel(lights, y, x) == 1) {
        write('#');
      } else {
        write('.');
      }
    }
    writeln("");
  }
  writeln(lights.length);
}

void partA(int[Tuple!(int, int)] lights, int[int] mapper, int size) {
  for (int i = 0; i < 2; ++i) {
    //printOutput(data);
    lights = enhance(lights, mapper, size, (i % 2) * mapper.get(0, 0));
    size += 2;
    lights.rehash;
  }

  writeln("PartA: ", lights.length);
}

void partB(int[Tuple!(int, int)] lights, int[int] mapper, int size) {
  for (int i = 0; i < 50; ++i) {
    //printOutput(data);
    lights = enhance(lights, mapper, size, (i % 2) * mapper.get(0, 0));
    size += 2;
    lights.rehash;
  }

  writeln("PartB: ", lights.length);
}

void main()
{
  string r = readText("input.txt");
  auto lines = std.array.split(r, "\n");

  int[int] mapper = createMapper(lines[0]);
  mapper.rehash;
  string[] data = lines[2..lines.length];
  int size = cast(int)data.length;

  int[Tuple!(int, int)] lights;
  for (int y = 0; y < size; ++y) {
    for (int x = 0; x < size; ++x) {
      if (data[y][x] == '#') {
        lights[tuple(y, x)] = 1;
      }
    }
  }

  partA(lights, mapper, size);
  partB(lights, mapper, size);
}
