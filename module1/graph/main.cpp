#include <functional>
#include <iostream>
#include <iterator>
#include <list>
#include <queue>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

template<typename T>
using Graph = std::unordered_map<T, std::list<T>>;

template<typename T, typename Iter>
void bfs(const Graph<T>& g, const T& startVertex, Iter begin) {
    std::unordered_set<T> visited;
    std::queue<T> q;
    q.push(startVertex);
    visited.insert(startVertex);
    while(!q.empty()) { 
        const auto v = std::move(q.front());
        q.pop();
        *begin++ = v;
        auto it = g.find(v);
        if (it == g.end())
            continue;
        for (const auto& elem : it->second) {
            if (visited.count(elem) == 0) {
                visited.insert(elem);
                q.push(elem);
            }
        }
    }
}

template<typename T, typename Iter>
void dfs(const Graph<T>& g, const T& startVertex, Iter begin) {
    std::unordered_set<T> visited;
    std::stack<T> s;
    s.push(startVertex);
    while(!s.empty()) {
        const auto v = std::move(s.top());
        s.pop();
        if (visited.count(v) == 0) { 
            *begin++ = v;
            visited.insert(v);
        }
        auto it = g.find(v);
        if (it == g.end())
            continue;
        for(const auto& elem : it->second) {
            if (visited.count(elem) == 0) {
                s.push(elem);
            }
        }
    }
}

int main() {
    Graph<std::string> g;
    char graphType, traversalType;
    std::string startVertex;
    std::cin >> graphType >> startVertex >> traversalType; 
    std::string v1, v2;
    while(std::cin >> v1 >> v2) {
        switch(graphType) {
            case 'u':
                g[v2].push_back(v1);
            case 'd':
                g[v1].push_back(v2);
            default:
                break;
        }
    }
    std::function<bool(const std::string&, const std::string&)> cmp;
    switch(traversalType) {
        case 'b':
            cmp = std::less<std::string>{};
            break;
        case 'd':
            cmp = std::greater<std::string>{}; 
            break;
        default:
            break;
    }
    for (auto& elem : g) {
        elem.second.sort(cmp);
    }
    switch(traversalType) {
        case 'b':
            bfs(g, startVertex, std::ostream_iterator<std::string>{std::cout, "\n"});
            break;
        case 'd':
            dfs(g, startVertex, std::ostream_iterator<std::string>{std::cout, "\n"});
        default:
            break;
    }
    return 0;
}
