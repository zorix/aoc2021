#include <stdio.h>
#include <string.h>
#include <stdlib.h>

const int gridSize = 1000;

int gridPartA[1000][1000];
int gridPartB[1000][1000];

int min(int a, int b)
{
  if (a > b)
    return b;
  return a;
}

int max(int a, int b)
{
  if (a > b)
    return a;
  return b;
}

int main()
{
  FILE *f = fopen("./input.txt", "r");

  memset(gridPartA, 0, sizeof(gridPartA));
  memset(gridPartB, 0, sizeof(gridPartB));

  int x1, y1, x2, y2;
  while (fscanf(f, "%i,%i -> %i,%i\n", &x1, &y1, &x2, &y2) == 4)
  {
    int minx = min(x1, x2);
    int maxx = max(x1, x2);
    int miny = min(y1, y2);
    int maxy = max(y1, y2);

    // only horizontal or vertical
    if (minx == maxx || miny == maxy)
    {
      for (int x = minx; x <= maxx; ++x)
      {
        for (int y = miny; y <= maxy; ++y)
        {
          gridPartA[y][x] += 1;
          gridPartB[y][x] += 1;
        }
      }
    }
    else
    {
      if (x1 <= x2 && y1 <= y2)
      {
        for (int x = x1, y = y1; x <= x2 && y <= y2; ++x, ++y)
        {
          gridPartB[y][x] += 1;
        }
      }
      else if (x1 >= x2 && y1 <= y2)
      {
        for (int x = x1, y = y1; x >= x2 && y <= y2; --x, ++y)
        {
          gridPartB[y][x] += 1;
        }
      }
      else if (x1 <= x2 && y1 >= y2)
      {
        for (int x = x1, y = y1; x <= x2 && y >= y2; ++x, --y)
        {
          gridPartB[y][x] += 1;
        }
      }
      else if (x1 >= x2 && y1 >= y2)
      {
        for (int x = x1, y = y1; x >= x2 && y >= y2; --x, --y)
        {
          gridPartB[y][x] += 1;
        }
      }
    }
  }
  fclose(f);

  int countA = 0;
  int countB = 0;
  for (int x = 0; x < gridSize; ++x)
  {
    for (int y = 0; y < gridSize; ++y)
    {
      if (gridPartA[y][x] >= 2)
        ++countA;
      if (gridPartB[y][x] >= 2)
        ++countB;
    }
  }

  printf("part A %i\n", countA);
  printf("part B %i\n", countB);
}
