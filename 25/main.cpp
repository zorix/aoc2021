#include <vector>
#include <string>
#include <iostream>
#include <tuple>

enum class Cucumber
{
  EMPTY,
  RIGHT,
  DOWN
};

std::vector<std::vector<Cucumber>> load()
{
  std::vector<std::vector<Cucumber>> result;
  FILE *f = fopen("input.txt", "rb");
  char buf[1000];
  while (fgets(buf, 1000, f))
  {
    std::vector<Cucumber> l;
    l.reserve(100);

    char *c = buf;
    while (*c != '\n' && *c)
    {
      if (*c == '.')
      {
        l.emplace_back(Cucumber::EMPTY);
      }
      if (*c == '>')
      {
        l.emplace_back(Cucumber::RIGHT);
      }
      if (*c == 'v')
      {
        l.emplace_back(Cucumber::DOWN);
      }
      c++;
    }
    result.emplace_back(l);
  }

  fclose(f);
  return result;
}

std::tuple<bool, std::vector<std::vector<Cucumber>>> step(std::vector<std::vector<Cucumber>> const &map)
{
  std::vector<std::vector<Cucumber>> moveRightMap(map.size(), std::vector<Cucumber>(map[0].size(), Cucumber::EMPTY));
  bool moved = false;
  int yLength = map.size();
  int xLength = map[0].size();
  for (int y = 0; y < yLength; ++y)
  {
    for (int x = 0; x < xLength; ++x)
    {
      if (map[y][x] == Cucumber::RIGHT)
      {
        if (map[y][(x + 1) % xLength] == Cucumber::EMPTY)
        {
          moveRightMap[y][(x + 1) % xLength] = Cucumber::RIGHT;
          moved = true;
        }
        else
        {
          moveRightMap[y][x] = Cucumber::RIGHT;
        }
      }
      else if (map[y][x] == Cucumber::DOWN)
      {
        moveRightMap[y][x] = Cucumber::DOWN;
      }
    }
  }

  std::vector<std::vector<Cucumber>> finalMap(map.size(), std::vector<Cucumber>(map[0].size(), Cucumber::EMPTY));

  for (int y = 0; y < yLength; ++y)
  {
    for (int x = 0; x < xLength; ++x)
    {
      if (moveRightMap[y][x] == Cucumber::DOWN)
      {
        if (moveRightMap[(y + 1) % yLength][x] == Cucumber::EMPTY)
        {
          finalMap[(y + 1) % yLength][x] = Cucumber::DOWN;
          moved = true;
        }
        else
        {
          finalMap[y][x] = Cucumber::DOWN;
        }
      }
      else if (moveRightMap[y][x] == Cucumber::RIGHT)
      {
        finalMap[y][x] = Cucumber::RIGHT;
      }
    }
  }

  return std::make_tuple(moved, finalMap);
}

int main()
{
  auto data = load();
  int count = 0;

  while (true)
  {
    ++count;
    auto &&[moved, newData] = step(data);
    if (!moved)
    {
      break;
    }
    data = std::move(newData);
  }
  std::cout << "PartA: " << count << "\n";
}