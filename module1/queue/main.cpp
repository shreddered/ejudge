#include <algorithm>
#include <fstream>
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
class Queue {
public:
    Queue() : _ptr{nullptr}, _size{0}, _first{-1}, _last{-1} {}
    void setSize(std::size_t n) {
        _size = n;
        _ptr.reset(new T[n]);
    }
    template<typename... U>
    void emplace(U&&... values) {
        if ((_last + 1) % _size == _first)
            throw OverflowException();
        if (_first == -1)
            _first = 0;
        _ptr[_last = (_last + 1) % _size] = T{std::forward<U>(values)...};
    }
    inline void push(const T& value) {
        emplace(value);
    }
    inline bool empty() const noexcept {
        return _first == -1;
    }
    T pop() {
        if (empty())
            throw UnderflowException();
        auto __first = _first;
        if (_first == _last)
            _first = _last = -1;
        else
            _first = (_first + 1) % _size;
        return _ptr[__first]; 
    }
    template<typename U>
    friend std::ostream& operator<<(std::ostream& out, const Queue<U>& s);
private:
    std::unique_ptr<T[]> _ptr;
    int _size, _first, _last;
}; // class Queue

template<typename T>
std::ostream& operator<<(std::ostream& out, const Queue<T>& s) {
    if (s.empty())
        return out << "empty";
    std::size_t i;
    for (i = s._first; i != s._last; i = (i + 1) % s._size)
        out << s._ptr[i] << ' ';
    return out << s._ptr[s._last];
}

int main(int argc, char* argv[]) {
    const std::string commands[] = {
        "set_size",
        "push",
        "pop",
        "print"
    };
    bool isSized = false;
    std::string line;
    Queue<std::string> s;
    std::ifstream in{argv[1]};
    std::ofstream out{argv[2]};
    while(std::getline(in, line) && !isSized) {
        if (line.empty())
            continue;
        auto it = std::find_if(line.begin(), line.end(), isspace);
        if (std::string(line.begin(), it) != commands[0]) {
            out << "error" << std::endl;
            continue;
        }
        it = std::find_if_not(it, line.end(), isspace);
        auto str = std::string(it, line.end());
        try {
            s.setSize(std::stoull(str));
            isSized = true;
        } catch(const std::invalid_argument&) {
            out << "error" << std::endl;
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
                out << "error" << std::endl;
                continue;
            }
            try {
                s.emplace(it + 1, line.end());
            } catch(const OverflowException& e) {
                out << e.what() << std::endl;
            }
        } else if(command == commands[2]) {
            if (it == line.end()) {
                try {
                    out << s.pop() << std::endl;
                } catch(const UnderflowException& e) {
                    out << e.what() << std::endl;
                }
            }
            else {
                out << "error" << std::endl;
            }
        } else if(command == commands[3]) {
            if (it == line.end()) {
                out << s << std::endl;
            } else {
                out << "error" << std::endl;
            }
        } else {
            out << "error" << std::endl;
        }
    } while (std::getline(in, line));
    return 0;
}
