import java.util.*;
import java.lang.*;
import java.io.*;
import java.nio.file.*;
import java.nio.charset.*;
import java.util.stream.*;


class Program {
  public static void main(String... args) throws IOException {
    byte[] encoded = Files.readAllBytes(Paths.get("./input.txt"));
    var data = new String(encoded, StandardCharsets.UTF_8);
    var lines = data.split("\n");
    var pixelsString = Arrays.stream(lines).map(x -> x.split("")).toArray(String[][]::new);
    var pixels = Arrays.stream(pixelsString).map(x -> Arrays.stream(x).map(Integer::parseInt).toArray(Integer[]::new)).toArray(Integer[][]::new);

    partA(pixels);
    partB(pixels);
  }

  public static void partA(Integer[][] pixels) {
    var heights = new ArrayList<Integer>();
    for (int y = 0; y < pixels.length; ++y) {
      var line = pixels[y];
      for (int x = 0; x < line.length; ++x) {
        var cell = line[x];
        if (isHole(pixels, x, y))
          heights.add(cell+1);
      }
    }

    var sum = heights.stream().reduce(0, (s, x) -> s + x);
    System.out.println("partA: " + sum);
  }

  public static void partB(Integer[][] pixels) {
    var basins = new ArrayList<Integer>();

    for (int y = 0; y < pixels.length; ++y) {
      var line = pixels[y];
      for (int x = 0; x < line.length; ++x) {
        var cell = line[x];
        if (isHole(pixels, x, y)) {
          var markers = new int[pixels.length][];
          for (int yy = 0; yy < markers.length; ++yy) {
            markers[yy] = new int[pixels[0].length];
          }
          markField(pixels, markers, x, y);

          basins.add(Arrays.stream(markers).map(v -> Arrays.stream(v).reduce(0, (s, vv) -> s + vv)).reduce(0, (s, vv) -> s + vv));
        }
      }
    }

    var mul = basins.stream().sorted(Comparator.reverseOrder()).limit(3).reduce(1, (s, x) -> s * x);
    System.out.println("partB: " + mul);
  }

  public static boolean isHole(Integer[][] pixels, int x, int y) {
    var line = pixels[y];
    var cell = line[x];
    if (x > 0) {
      if (line[x-1] <= cell)
        return false;
    }
    if (x < line.length-1) {
      if (line[x+1] <= cell)
        return false;
    }
    if (y > 0) {
      if (pixels[y-1][x] <= cell)
        return false;
    }
    if (y < pixels.length-1) {
      if (pixels[y+1][x] <= cell)
        return false;
    }
    return true;
  }

  public static void markField(Integer[][] pixels, int[][] markers, int x, int y) {
    if (markers[y][x] == 1 || pixels[y][x] == 9) {
      return;
    }

    markers[y][x] = 1;

    var cell = pixels[y][x];
    if (x > 0) {
      if (pixels[y][x-1] > cell)
        markField(pixels, markers, x-1, y);
    }
    if (x < pixels[y].length-1) {
      if (pixels[y][x+1] > cell)
        markField(pixels, markers, x+1, y);
    }
    if (y > 0) {
      if (pixels[y-1][x] > cell)
        markField(pixels, markers, x, y-1);
    }
    if (y < pixels.length-1) {
      if (pixels[y+1][x] > cell)
        markField(pixels, markers, x, y+1);
    }
  }
}
