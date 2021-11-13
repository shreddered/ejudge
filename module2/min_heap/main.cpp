#include <cstdint>
#include <algorithm>
#include <functional>
#include <iostream>
#include <ostream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector>

template <
    class Key,
    class T
> class MinHeap final {
public:
    MinHeap() noexcept = default;
    ~MinHeap() noexcept = default;
    T& at(const Key& key) {
        auto it = m_lookupTable.find(key);
        if (it == m_lookupTable.end())
            throw std::out_of_range{"No such element"};
        return m_array[it->second].second;
    }
    inline bool empty() const noexcept {
        return m_array.empty();
    }
    std::pair<Key, T> extract() {
        if (empty())
            throw std::logic_error{"Cannot extract from empty heap"};
        auto res = m_array.front();
        m_lookupTable[m_array.back().first] = 0;
        m_lookupTable.erase(res.first);
        removeImpl(0);
        return res;
    }
    std::pair<std::size_t, T> find(const Key& key) const {
        auto it = m_lookupTable.find(key);
        if (it == m_lookupTable.end())
            throw std::out_of_range{"No such element"};
        return std::make_pair(it->second, m_array[it->second].second);
    }
    void insert(const Key& key, const T& value) {
        if (m_lookupTable.find(key) != m_lookupTable.end())
            throw std::logic_error{"Element is already in the heap"};
        m_array.emplace_back(key, value);
        m_lookupTable.emplace(key, m_array.size() - 1);
        heapifyUp(m_array.size() - 1);
    }
    std::tuple<Key, std::size_t, T> min() const {
        if (empty())
            throw std::logic_error{"No minimum in empty heap"};
        return std::make_tuple(m_array.front().first, 0, m_array.front().second);
    }
    std::tuple<Key, std::size_t, T> max() {
        if (empty())
            throw std::logic_error{"No maximum in empty heap"};
        auto it = std::max_element(m_array.begin(), m_array.end());
        return std::make_tuple(it->first, std::distance(m_array.begin(), it),
                               it->second);
    }
    void remove(const Key& key) {
        auto it = m_lookupTable.find(key);
        if (it == m_lookupTable.end())
            throw std::out_of_range{"No such element"};
        auto index = it->second;
        m_lookupTable[m_array.back().first] = index;
        m_lookupTable.erase(it);
        removeImpl(index);
    }
    template<class _Key, class _Value>
    friend std::ostream& operator<<(std::ostream& os, const MinHeap<_Key, _Value>& heap);

private:
    using ValueType = std::pair<Key, T>;
    std::vector<ValueType> m_array;
    std::unordered_map<Key, std::size_t> m_lookupTable;

    void heapifyUp(std::size_t curr) {
        if (!curr)
            return;
        auto parent = (curr - 1) >> 1;
        while (curr && m_array[curr].first < m_array[parent].first) {
            std::swap(m_lookupTable.at(m_array[curr].first),
                      m_lookupTable.at(m_array[parent].first));
            std::swap(m_array[curr], m_array[parent]);
            curr = parent;
            parent = (parent - 1) >> 1;
        }
    }
    void heapifyDown(std::size_t curr) {
        auto left = (curr << 1) + 1, right = left + 1;
        while (left < m_array.size()) {
            if (right < m_array.size()) {
                if (m_array[curr].first < m_array[left].first
                    && m_array[curr].first < m_array[right].first) {
                    break;
                } else if (m_array[left].first < m_array[right].first) {
                    std::swap(m_lookupTable.at(m_array[curr].first),
                              m_lookupTable.at(m_array[left].first));
                    std::swap(m_array[left], m_array[curr]);
                    curr = left;
                } else {
                    std::swap(m_lookupTable.at(m_array[curr].first),
                              m_lookupTable.at(m_array[right].first));
                    std::swap(m_array[right], m_array[curr]);
                    curr = right;
                }
            } else {
                if (m_array[curr].first < m_array[left].first) {
                    break;
                } else {
                    std::swap(m_lookupTable.at(m_array[curr].first),
                              m_lookupTable.at(m_array[left].first));
                    std::swap(m_array[left], m_array[curr]);
                    curr = left;
                }
            }
            left = (curr << 1) + 1;
            right = left + 1;
        }
    }
    void removeImpl(std::size_t index) {
        std::swap(m_array[index], m_array.back());
        m_array.pop_back();
        if (index != m_array.size() - 1) {
            auto parent = (index - 1) >> 1;
            if (index == 0 || m_array[parent].first < m_array[index].first)
                heapifyDown(index);
            else
                heapifyUp(index);
        }
    }
}; // class Heap

template<class _Key, class _Value>
std::ostream& operator<<(std::ostream& os, const MinHeap<_Key, _Value>& heap) {
    if (heap.empty())
        return os << '_';
    const auto& arr = heap.m_array;
    auto repeatUnderscore = [](std::size_t times) {
        std::string res{};
        if (times) {
            res.reserve(times << 1);
            for (; times; --times) {
                res.append(" _", 2);
            }
        }
        return res;
    };
    os << '[' << arr.front().first << ' ' << arr.front().second << ']';
    std::size_t index = 1, levelSize = 2;
    while (index < arr.size()) {
        os << '\n';
        if (index + levelSize < arr.size()) {
            for (std::size_t offset = 0; offset < levelSize; ++offset) {
                if (offset)
                    os << ' ';
                const auto& elem = arr.at(index + offset);
                const auto parentIndex = (index + offset - 1) >> 1;
                std::cout << '[' << elem.first << ' ' << elem.second << ' '
                          << arr.at(parentIndex).first << ']';
            }
        } else {
            auto diff = arr.size() - index;
            for (std::size_t offset = 0; offset < diff; ++offset) {
                if (offset)
                    os << ' ';
                const auto& elem = arr.at(index + offset);
                const auto parentIndex = (index + offset - 1) >> 1;
                std::cout << '[' << elem.first << ' ' << elem.second << ' '
                          << arr.at(parentIndex).first << ']';
            }
            std::cout << repeatUnderscore(levelSize - diff);
        }
        index += levelSize;
        levelSize <<= 1;
    }
    return os;
}

int main() {
    MinHeap<int64_t, std::string> heap;
    std::string command;
    while (std::cin >> command) {
        if (command == "add") {
            try {
                int64_t key;
                std::string value;
                std::cin >> key >> value;
                heap.insert(key, value);
            }  catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "set") {
            try {
                int64_t key;
                std::string value;
                std::cin >> key >> value;
                heap.at(key) = value;
            }  catch (std::out_of_range) {
                std::cout << "error\n";
            }
        } else if (command == "delete") {
            try {
                int64_t key;
                std::cin >> key;
                heap.remove(key);
            }  catch (std::out_of_range) {
                std::cout << "error\n";
            }
        } else if (command == "search") {
            try {
                int64_t key;
                std::cin >> key;
                const auto& [index, value] = heap.find(key);
                std::cout << "1 " << index << ' ' << value << '\n';
            }  catch (std::out_of_range) {
                std::cout << "0\n";
            }
        } else if (command == "min") {
            try {
                const auto& [key, index, value] = heap.min();
                std::cout << key << ' ' << index << ' ' << value << '\n';
            }  catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "max") {
            try {
                const auto& [key, index, value] = heap.max();
                std::cout << key << ' ' << index << ' ' << value << '\n';
            }  catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "extract") {
            try {
                const auto& [key, value] = heap.extract();
                std::cout << key << ' ' << value << '\n';
            } catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "print") {
            std::cout << heap << '\n';
        } else {
            std::cout << "error\n";
        }
    }
    return 0;
}
