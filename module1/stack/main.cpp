#include <algorithm>
#include <iostream>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

class UnderflowException : public std::out_of_range {
public:
    UnderflowException() : std::out_of_range("underflow") {}
}; // class UnderflowException

class OverflowException : public std::out_of_range {
public:
    OverflowException() : std::out_of_range("overflow") {}
}; // class OverflowException

template <typename T>
class Stack {
public:
    Stack() : _ptr{nullptr}, _size{0}, _capacity{0} {}
    void setSize(std::size_t n) {
        _capacity = n;
        _ptr.reset(new T[n]);
    }
    template<typename... U>
    void emplace(U&&... values) {
        if (_size == _capacity)
            throw OverflowException();
        _ptr[_size++] = T(std::forward<U>(values)...);
    }
    inline void push(const T& value) {
        emplace(value);
    }
    inline bool empty() const noexcept {
        return _size == 0;
    }
    T pop() {
        if (empty())
            throw UnderflowException();
        return _ptr[--_size];
    }
    template<typename U>
    friend std::ostream& operator<<(std::ostream& out, const Stack<U>& s);
private:
    std::unique_ptr<T[]> _ptr;
    std::size_t _size, _capacity;
}; // class Stack

template<typename T>
std::ostream& operator<<(std::ostream& out, const Stack<T>& s) {
    if (s.empty())
        return out << "empty";
    for (std::size_t i = 0; i < s._size - 1; ++i)
        out << s._ptr[i] << ' ';
    return out << s._ptr[s._size - 1];
}

int main() {
    const std::string commands[] = {
        "set_size",
        "push",
        "pop",
        "print"
    };
    bool isSized = false;
    std::string line;
    Stack<std::string> s;
    while(std::getline(std::cin, line) && !isSized) {
        if (line.empty())
            continue;
        auto it = std::find_if(line.begin(), line.end(), isspace);
        if (std::string(line.begin(), it) != commands[0]) {
            std::cout << "error" << std::endl;
            continue;
        }
        it = std::find_if_not(it, line.end(), isspace);
        auto str = std::string(it, line.end());
        try {
            s.setSize(std::stoull(str));
            isSized = true;
        } catch(const std::invalid_argument&) {
            std::cout << "error" << std::endl;
        }
    }
    if (!isSized)
        return 0;
    do {
        if (line.empty())
            continue;
        auto it = std::find_if(line.begin(), line.end(), isspace);
        std::string command(line.begin(), it);
        if (command == commands[1]) {
            auto end = std::find_if(it + 1, line.end(), isspace);
            if (end != line.end()) {
                std::cout << "error" << std::endl;
                continue;
            }
            try {
                s.emplace(it + 1, line.end());
            } catch(const OverflowException& e) {
                std::cout << e.what() << std::endl;
            }
        } else if(command == commands[2]) {
            if (it == line.end()) {
                try {
                    std::cout << s.pop() << std::endl;
                } catch(const UnderflowException& e) {
                    std::cout << e.what() << std::endl;
                }
            }
            else {
                std::cout << "error" << std::endl;
            }
        } else if(command == commands[3]) {
            if (it == line.end()) {
                std::cout << s << std::endl;
            } else {
                std::cout << "error" << std::endl;
            }
        } else {
            std::cout << "error" << std::endl;
        }
    } while (std::getline(std::cin, line));
    return 0;
}
