#include <vector>
#include <queue>
#include <cstdio>
#include <map>
#include <unordered_map>
#include <algorithm>

int wrap(int i)
{
  return (i % 10) + (i / 10);
}

std::vector<std::vector<int>> loadA()
{
  std::vector<std::vector<int>> result;
  FILE *f = fopen("input.txt", "rb");
  char buf[1000];
  while (fgets(buf, 1000, f))
  {
    std::vector<int> l;
    char *c = buf;
    while (*c != '\n' && *c)
    {
      l.emplace_back(*c - '0');
      c++;
    }
    result.emplace_back(l);
  }

  fclose(f);
  return result;
}

std::vector<std::vector<int>> loadB()
{
  std::vector<std::vector<int>> result;
  FILE *f = fopen("input.txt", "rb");
  char buf[1000];
  while (fgets(buf, 1000, f))
  {
    std::vector<int> l;
    char *c = buf;
    while (*c != '\n' && *c)
    {
      l.emplace_back(*c - '0');
      c++;
    }
    result.emplace_back(l);
  }

  fclose(f);

  // Add columns
  std::vector<std::vector<int>> updatedResult;
  for (auto &&l : result)
  {
    std::vector<int> newL;
    std::transform(l.begin(), l.end(), std::back_inserter(newL),
                   [](int c)
                   { return c; });
    std::transform(l.begin(), l.end(), std::back_inserter(newL),
                   [](int c)
                   { return wrap(c + 1); });
    std::transform(l.begin(), l.end(), std::back_inserter(newL),
                   [](int c)
                   { return wrap(c + 2); });
    std::transform(l.begin(), l.end(), std::back_inserter(newL),
                   [](int c)
                   { return wrap(c + 3); });
    std::transform(l.begin(), l.end(), std::back_inserter(newL),
                   [](int c)
                   { return wrap(c + 4); });

    updatedResult.emplace_back(std::move(newL));
  }

  // Add rows
  for (int i = 1; i < 5; ++i)
  {
    for (int l = 0; l < 100; ++l)
    {
      std::vector<int> newL;
      std::transform(updatedResult[l].begin(), updatedResult[l].end(), std::back_inserter(newL),
                     [i](int c)
                     { return wrap(c + i); });
      updatedResult.emplace_back(std::move(newL));
    }
  }

  return updatedResult;
}

using Point = std::pair<int, int>;

int solve(const std::vector<std::vector<int>> &data)
{
  auto size = data.size();

  std::map<Point, int> costs;
  for (int y = 0; y < size; ++y)
    for (int x = 0; x < size; ++x)
      costs.emplace(Point{x, y}, 10000000);

  std::multimap<int, Point> toCheck;
  toCheck.emplace(0, Point{0, 0});

  auto addPointToCheck = [size, &data, &toCheck](int x, int y, int cost)
  {
    if (x >= 0 && x < size && y >= 0 && y < size)
    {
      toCheck.emplace(cost + data[y][x], Point{x, y});
    }
  };

  while (toCheck.size() > 0)
  {
    auto itr = toCheck.begin();
    auto [cost, pos] = *itr;
    toCheck.erase(itr);
    if (cost < costs[pos])
    {
      costs[pos] = cost;
      addPointToCheck(pos.first + 1, pos.second, cost);
      addPointToCheck(pos.first - 1, pos.second, cost);
      addPointToCheck(pos.first, pos.second + 1, cost);
      addPointToCheck(pos.first, pos.second - 1, cost);
    }
  }

  return costs[Point{size - 1, size - 1}];
}

int main()
{
  auto dataA = loadA();
  auto dataB = loadB();

  printf("PartA: %i\n", solve(dataA));
  printf("PartB: %i\n", solve(dataB));
}
