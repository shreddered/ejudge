#include <deque>
#include <iostream>
#include <utility>

// enum для обозначения шагов алгоритма
enum Step {
    Inc,
    Dec,
    Double
};

// Функция, возвращающая шаги для заданной половины состояния.
std::deque<Step> steps(std::size_t n) {
    std::deque<Step> res;
    // count trailing zeros
    // Результат работы функции - пара, где первый элемент - это само значение,
    // второй элемент - параметр, который сдвинули вправо на полученное число.
    auto ctz = [](std::size_t n) {
        std::size_t res = 0;
        while (n && !(n % 2)) {
            ++res;
            n >>= 1;
        }
        return std::make_pair(n, res);
    };
    // Ключевой цикл алгоритма.
    // Идём с "обратной" стороны - от n к 1.
    // Идея заключается в выборе максимально быстрого варианта как уменьшить число,
    // комбинируя операции +1, -1 и div 2.
    while (n > 1 && n != 3) {
        if (n % 2) {
            // Если двоичная запись числа заканчивается на 1, то смотрим,
            // в каком случае больше нулей с конца (т.о. число уменьшится
            // быстрее).
            const auto [branch1, ctz1] = ctz(n + 1);
            const auto [branch2, ctz2] = ctz(n - 1);
            if (ctz1 > ctz2) {
                n = branch1;
                res.push_front(Step::Dec);
            } else {
                n = branch2;
                res.push_front(Step::Inc);
            }
            // После выбора команды inc/dec убираем нули с конца с помощью команды dbl.
            for (std::size_t count = std::max(ctz1, ctz2); count; --count)
                res.push_front(Step::Double);
        } else {
            // Иначе просто убираем все нули с конца
            // с помощью команды dbl.
            auto [n1, ctz1] = ctz(n);
            for (; ctz1; --ctz1)
                res.push_front(Step::Double);
            n = n1;
        }
    }
    // Исключением является число 3. Для него лучшая последовательность действий
    // следующая: inc, dbl, inc.
    if (n == 3) {
        res.push_front(Step::Inc);
        res.push_front(Step::Double);
    }
    // Сначала всегда забираем у богача миллион.
    res.push_front(Step::Inc);
    return res;
}

int main() {
    std::size_t n;
    std::cin >> n;
    auto res = steps(n);
    for (auto it = res.begin(); it != res.end(); ++it) {
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
