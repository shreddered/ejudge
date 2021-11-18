#include <iostream>
#include <vector>
#include <utility>

enum Step {
    Inc,
    Dec,
    Double
};

std::vector<Step> steps(std::size_t n) {
    std::vector<Step> res;
    auto ctz = [](std::size_t n) {
        std::size_t res = 0;
        while (n && !(n % 2)) {
            ++res;
            n >>= 1;
        }
        return std::make_pair(n, res);
    };
    while (n > 1 && n != 3) {
        if (n % 2) {
            const auto [branch1, ctz1] = ctz(n + 1);
            const auto [branch2, ctz2] = ctz(n - 1);
            if (ctz1 > ctz2) {
                n = branch1;
                res.push_back(Step::Dec);
            } else {
                n = branch2;
                res.push_back(Step::Inc);
            }
            for (std::size_t count = std::max(ctz1, ctz2); count; --count)
                res.push_back(Step::Double);
        } else {
            auto [n1, ctz1] = ctz(n);
            for (; ctz1; --ctz1)
                res.push_back(Step::Double);
            n = n1;
        }
    }
    if (n == 3) {
        res.push_back(Step::Inc);
        res.push_back(Step::Double);
    }
    res.push_back(Step::Inc);
    return res;
}

int main() {
    std::size_t n;
    std::cin >> n;
    auto res = steps(n);
    for (auto it = res.rbegin(); it != res.rend(); ++it) {
        switch (*it) {
            case Step::Inc:
                std::cout << "inc\n";
                break;
            case Step::Dec:
                std::cout << "dec\n";
                break;
            case Step::Double:
                std::cout << "dbl\n";
                break;
            default:
                break;
        }
    }
    return 0;
}
