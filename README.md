## Решение всех задач ejudge

Решение всех задач (дополняется). Большинство задач написано на C/C++ (исключением является задача про автозамену, очень уж удобно её писать на Haskell). __Актуально на осенний семестр 2021-2022 учебного года.__

* Модуль 1
    * [Сумма (С)](module1/sum/main.c)
        В строке может быть не только число (и если вдруг будет, то не факт, что одно).
    * [Стэк (C++)](module1/stack/main.cpp)
        Бывают неправильные команды и всяческие отступления от формата ввода.
    * [Очередь (C++)](module1/queue/main.cpp)
        То же самое, что и в задаче про стэк.
    * [Граф (C++)](module1/graph/main.cpp)
        Тут без приколов.
* Модуль 2
    * [Косое дерево (C++)](module2/splay_tree/main.cpp)
        Если элемент не найден, поднимается вверх последний элемент в пути, по которому искали. И прочие подобные приколы.
    * [Min-куча (C++)](module2/min_heap/main.cpp)
        Приколов не обнаружено.
    * [Автозамена (Haskell)](module2/autocorrection/Main.hs)
        Могут быть русские буквы, регистр которых надо понижать. В Haskell функция `Data.Char.toLower` действует с опорой на локаль, так что никаких проблем не возникнет, чего не скажешь о C++. [^1]
* Модуль 3 - work in progress

## Как собирать?

Ручками! Вооружиться любимым компилятором и вперёд.

[^1]: Если вы собрались писать автозамену на C++, то ~~я вам сочувствую~~ вам придётся иметь дело с русскими буквами. Для этого придётся сделать что-то наподобие такого:
```cpp
#include <algorithm>
#include <functional>
#include <locale>
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    // This is an example code which converts line from stdin to lower case according to user locale
    std::ios_base::sync_with_stdio(false); // required for imbue() to work as intended
    std::locale loc{""}; // use user locale
    std::wcin.imbue(loc);
    std::wcout.imbue(loc);
    std::wstring s;
    std::getline(std::wcin, s);
    // Convert tolower case in-place
    std::transform(s.begin(), s.end(), s.begin(),
                   std::bind(std::tolower<wchar_t>, // use this template from <locale>
                   std::placeholders::_1,
                   std::cref(loc)));
    std::wcout << s << std::endl;
}
```
