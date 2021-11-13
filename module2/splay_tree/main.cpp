#include <cstdint>
#include <iostream>
#include <ostream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

template<
    class Key,
    class Value
> class SplayTree {
public:
    SplayTree() noexcept : m_root{nullptr} {}
    virtual ~SplayTree() noexcept {
        if (!empty())
            clear(m_root);
    }
    Value& at(const Key& key) {
        auto node = search(key);
        if (node) {
            m_root = splay(node);
            return node->value;
        }
        throw std::out_of_range{"No element with such key"};
    }
    inline bool empty() const noexcept {
        return m_root == nullptr;
    }
    void insert(const Key& key, const Value& value) {
        if (empty()) {
            m_root = new Node{key, value};
            return;
        }
        Node* prev = nullptr;
        for (auto curr = m_root; curr; ) {
            prev = curr;
            if (key < curr->key) {
                curr = curr->left;
            } else if (key > curr->key) {
                curr = curr->right;
            } else {
                m_root = splay(curr);
                throw std::logic_error{"Cannot insert element that is already presented in tree"};
            }
        }
        if (key < prev->key) {
            prev->left = new Node{key, value, prev};
            m_root = splay(prev->left);
        }
        else {
            prev->right = new Node{key, value, prev};
            m_root = splay(prev->right);
        }
    }
    std::pair<Key, Value> max() {
        if (empty())
            throw std::logic_error{"Cannot find max element of an empty tree"};
        auto curr = m_root;
        for (; curr->right; curr = curr->right) {}
        m_root = splay(curr);
        return std::make_pair(curr->key, curr->value);
    }
    std::pair<Key, Value> min() {
        if (empty())
            throw std::logic_error{"Cannot find min element of an empty tree"};
        auto curr = m_root;
        for (; curr->left; curr = curr->left) {}
        m_root = splay(curr);
        return std::make_pair(curr->key, curr->value);
    }
    void remove(const Key& key) {
        auto node = search(key);
        if (node) {
            m_root = splay(node);
            if (node->left)
                node->left->parent = nullptr;
            if (node->right)
                node->right->parent = nullptr;
            m_root = merge(node->left, node->right);
            delete node;
        } else {
            throw std::logic_error{"No element to remove"};
        }
    }
    template<class _Key, class _Value>
    friend std::ostream& operator<<(std::ostream& os, const SplayTree<_Key, _Value>& tree);

private:
    struct Node {
        Node* left, * right, * parent;
        Key key;
        Value value;
        explicit Node(const Key& _key,
                      const Value& _value,
                      Node* _parent = nullptr) noexcept
            : left{nullptr}, right{nullptr}, parent{_parent},
              key{_key}, value{_value} {}
    }; // struct Node
    Node* m_root;

    Node* search(const Key& key) {
        Node* curr = m_root, * prev = nullptr;
        while(curr && key != curr->key) {
            prev = curr;
            if (key > curr->key)
                curr = curr->right;
            else if (key < curr->key)
                curr = curr->left;
        }
        if (!curr && prev)
            m_root = splay(prev);
        return curr;
    }

    static Node* merge(Node* t1, Node* t2) {
        if (!t1)
            return t2;
        if (!t2)
            return t1;
        auto curr = t1;
        for(; curr->right; curr = curr->right) {}
        t1 = splay(curr);
        t1->right = t2;
        if (t2)
            t2->parent = t1;
        return t1;
    }

    // for deallocating memory
    static void clear(Node* node) {
        if (node->left)
            clear(node->left);
        if (node->right)
            clear(node->right);
        delete node;
    }

    // splay-related functions
    static Node* splay(Node* node) {
        while (node->parent) {
            auto parent = node->parent,
                 grandparent = parent->parent;
            if (!grandparent)
                zig(node);
            else if ((grandparent->left == parent && parent->left == node)
                     || (grandparent->right == parent && parent->right == node))
                zigZig(node);
            else
                zigZag(node);
        }
        return node;
    }
    static void zig(Node* node) {
        auto parent = node->parent;
        if (auto grandparent = parent->parent; grandparent) {
            if (grandparent->left == parent)
                grandparent->left = node;
            else
                grandparent->right = node;
        }
        if (parent->left == node) {
            auto rightSubTree = node->right;
            // right sub tree of current node -> left sub tree of parent
            parent->left = rightSubTree;
            if (rightSubTree)
                rightSubTree->parent = parent;
            // parent becomes right child
            node->right = parent;
            node->parent = parent->parent;
            parent->parent = node;
        } else {
            // mirroring
            auto leftSubTree = node->left;
            parent->right = leftSubTree;
            if (leftSubTree)
                leftSubTree->parent = parent;
            node->left = parent;
            node->parent = parent->parent;
            parent->parent = node;
        }
    }
    static void zigZig(Node* node) {
        zig(node->parent);
        zig(node);
    }
    static void zigZag(Node* node) {
        zig(node);
        zig(node);
    }
}; // class SplayTree

template<class Key, class Value>
std::ostream& operator<<(std::ostream& os, const SplayTree<Key, Value>& tree) {
    if (tree.empty()) {
        return os << '_';
    }
    // helper types
    using NodePtr = typename SplayTree<Key, Value>::Node*;
    struct Element {
        NodePtr node;
        std::size_t underscoreCount;
        Element(NodePtr _node) noexcept
            : node{_node}, underscoreCount{(node) ? 0u : 1u} {}
    }; // struct Element

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

    std::vector<Element> currentLevel, nextLevel;
    currentLevel.emplace_back(tree.m_root);
    do {
        for (auto it = currentLevel.begin(); it != currentLevel.end(); ++it) {
            if (it != currentLevel.begin()) {
                os << ' ';
            }
            if (NodePtr node = it->node; node) {
                os << '[' << node->key << ' ' << node->value;
                if (NodePtr parent = node->parent; parent) {
                    os << ' ' << parent->key;
                }
                os << ']';

                // tricky insertion
                if (node->left) {
                    nextLevel.emplace_back(node->left);
                } else if (nextLevel.empty() || nextLevel.back().node) {
                    nextLevel.emplace_back(nullptr);
                } else {
                    ++nextLevel.back().underscoreCount;
                }
                
                if (node->right) {
                    nextLevel.emplace_back(node->right);
                } else if (nextLevel.empty() || nextLevel.back().node) {
                    nextLevel.emplace_back(nullptr);
                } else {
                    ++nextLevel.back().underscoreCount;
                }
            } else {
                os << '_' << repeatUnderscore(it->underscoreCount - 1);
                if (nextLevel.empty() || nextLevel.back().node) {
                    nextLevel.emplace_back(nullptr);
                    nextLevel.back().underscoreCount = it->underscoreCount << 1;
                } else {
                    nextLevel.back().underscoreCount += it->underscoreCount << 1;
                }
            }
        }
        if (nextLevel.size() > 1)
            os << '\n';
        std::swap(nextLevel, currentLevel);
        nextLevel.clear();
    } while (currentLevel.size() > 1);
    return os;
}

int main() {
    SplayTree<int64_t, std::string> tree;
    std::string command;
    while(std::cin >> command) {
        if (command == "add") {
            try {
                int64_t key;
                std::string value;
                std::cin >> key >> value;
                tree.insert(key, value);
            }  catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "set") {
            try {
                int64_t key;
                std::string value;
                std::cin >> key >> value;
                tree.at(key) = value;
            }  catch (std::out_of_range) {
                std::cout << "error\n";
            }
        } else if (command == "print") {
            std::cout << tree << '\n';
        } else if (command == "delete") {
            try {
                int64_t key;
                std::cin >> key;
                tree.remove(key);
            } catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "search") {
            try {
                int64_t key;
                std::cin >> key;
                auto&& elem = tree.at(key);
                std::cout << "1 " << elem << std::endl;
            }  catch (std::out_of_range) {
                std::cout << '0' << std::endl;
            }
        } else if (command == "min") {
            try {
                auto p = tree.min();
                std::cout << p.first << ' ' << p.second << std::endl;
            }  catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else if (command == "max") {
            try {
                auto p = tree.max();
                std::cout << p.first << ' ' << p.second << std::endl;
            }  catch (std::logic_error) {
                std::cout << "error\n";
            }
        } else {
            std::cout << "error\n";
        }
    }
    return 0;
}
