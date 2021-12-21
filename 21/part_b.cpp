#include <unordered_map>
#include <tuple>
#include <vector>
#include <iostream>

struct Universe {
  int64_t p1_pos = 0;
  int64_t p1_score = 0;
  int64_t p2_pos = 0;
  int64_t p2_score = 0;
  bool p1 = false;

  bool p1Win() {
    return p1_score >= 21;
  }
  bool p2Win() {
    return p2_score >= 21;
  }

  std::vector<Universe> p1Roll() const {
    std::vector<Universe> unies;
    for (int64_t a = 1; a <= 3; ++a) {
      for (int64_t b = 1; b <= 3; ++b) {
        for (int64_t c = 1; c <= 3; ++c) {
          int64_t pos = (p1_pos + a + b + c - 1) % 10 +1;
          int64_t score = p1_score + pos;

          Universe uni = {
            pos,
            score,
            p2_pos,
            p2_score,
            false
          };
          unies.emplace_back(std::move(uni));
        }
      }
    }
    return unies;
  }

  std::vector<Universe> p2Roll() const {
    std::vector<Universe> unies;
    for (int64_t a = 1; a <= 3; ++a) {
      for (int64_t b = 1; b <= 3; ++b) {
        for (int64_t c = 1; c <= 3; ++c) {
          int64_t pos = (p2_pos + a + b + c - 1) % 10 +1;
          int64_t score = p2_score + pos;

          Universe uni = {
            p1_pos,
            p1_score,
            pos,
            score,
            true
          };
          unies.emplace_back(std::move(uni));
        }
      }
    }
    return unies;
  }

  std::vector<Universe> roll() const {
    if (p1)
      return p1Roll();
    return p2Roll();
  }
};

bool operator==(const Universe& lhs, const Universe& rhs) {
  return std::tie(lhs.p1_pos, lhs.p1_score, lhs.p2_pos, lhs.p2_score, lhs.p1) ==
    std::tie(rhs.p1_pos, rhs.p1_score, rhs.p2_pos, rhs.p2_score, rhs.p1);
}


namespace std {

  template <>
  struct hash<Universe>
  {
    std::size_t operator()(const Universe& k) const
    {
      using std::size_t;
      using std::hash;
      using std::string;

      // Compute individual hash values for first,
      // second and third and combine them using XOR
      // and bit shifting:

      return ((((hash<int>()(k.p1_pos)
               ^ (hash<int>()(k.p1_score) << 1) >> 1)
               ^ (hash<int>()(k.p2_pos) << 3) >> 1)
               ^ (hash<int>()(k.p2_score) << 5) >> 1)
               ^ (hash<bool>()(k.p1) << 10));
    }
  };

}

struct Wins {
  int64_t p1Wins = 0;
  int64_t p2Wins = 0;
};

std::unordered_map<Universe, Wins> cache;
Wins partB(const Universe& uni) {
  auto cacheVal = cache.find(uni);
  if (cacheVal != cache.end()) {
    return cacheVal->second;
  }

  Wins wins = {};
  for (auto&& u : uni.roll()) {
    if (u.p1Win()) {
      wins.p1Wins += 1;
    } else if (u.p2Win()) {
      wins.p2Wins += 1;
    } else {
      auto win = partB(u);
      wins.p1Wins += win.p1Wins;
      wins.p2Wins += win.p2Wins;
    }
  }

  cache[uni] = wins;
  return wins;
}

int main() {
  Universe init;
  init.p1_pos = 3;
  init.p2_pos = 10;
  init.p1_score = 0;
  init.p2_score = 0;
  init.p1 = true;

  auto w = partB(init);

  std::cout << std::max(w.p1Wins, w.p2Wins) << "\n";
}
