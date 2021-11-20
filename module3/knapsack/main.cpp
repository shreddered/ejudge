#include <algorithm>
#include <iostream>
#include <numeric>
#include <regex>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

// helper type alias (avoid awkward line wrapping)
using ResultType = std::tuple<std::size_t, std::size_t, std::size_t>;

/**
 * Function for solving knapsack problem (adapted for large capacities)
 *
 * @param weights Array of weights of an items (array).
 * @param values Array of values (or costs) of an items.
 * @param capacity_ Capacity of knapsack.
 * @return tuple (weight of a knapsack, sum of values, bitset), bitset[i] == 1 iff
 *         i-th item is in a knapsack)
 */
ResultType knapsack(const std::vector<std::size_t>& weights,
                    const std::vector<std::size_t>& values,
                    std::size_t capacity_) {
    std::size_t vmax = std::accumulate(values.begin(), values.end(), 0);
    // vector of pairs (weight, bitset)
    // bitset is for backtracking
    std::vector<std::pair<std::size_t, std::size_t>> dp(vmax + 1, {capacity_ + 1, 0});
    dp.front() = std::make_pair(0, 0);
    // The approach itself
    for (std::size_t i = 0; i < values.size(); ++i) {
        for (std::size_t j = vmax; j >= values[i]; --j) {
            if (weights[i] + dp[j - values[i]].first < dp[j].first) {
                dp[j] = dp[j - values[i]];
                dp[j].first += weights[i];
                dp[j].second |= 1 << i;
            }
        }
    }
    for (std::size_t i = vmax; i >= 0; --i) {
        if (dp[i].first <= capacity_)
            return std::make_tuple(dp[i].first, i, dp[i].second);
    }
    // this should never happen (idk what to return here)
    return std::make_tuple(0, 0, 0);
}

int main(int argc, char* argv[]) {
    std::size_t capacity_;
    std::vector<std::size_t> weights, values;

    std::regex expr{R"(^(\d+)((?:\s+)\d+)?$)"};

    std::string line;
    bool flag = false;
    while (std::getline(std::cin, line)) {
        if (line.empty())
            continue;
        std::smatch match;
        if (regex_search(line, match, expr)) {
            if (match.size() > 3) {
                std::cout << "error\n";
                continue;
            }
            if (!match[2].matched) {
                if (flag) {
                    std::cout << "error\n";
                    continue;
                }
                flag = true;
                capacity_ = std::stoull(match[1].str());
            } else {
                if (!flag) {
                    std::cout << "error\n";
                    continue;
                }
                std::size_t weight = std::stoull(match[1].str());
                std::size_t value = std::stoull(match[2].str());
                if ((weight == 0) && (value == 0)) {
                    std::cout << "error\n";
                    continue;
                }
                weights.push_back(weight);
                values.push_back(value);
            }
        } else {
            std::cout << "error\n";
        }
    }

    auto [weight, value, bitvec] = knapsack(weights, values, capacity_);
    std::cout << weight << ' ' << value << '\n';
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (bitvec & 1)
            std::cout << (i + 1) << '\n';
        bitvec >>= 1;
    }
    return 0;
}
