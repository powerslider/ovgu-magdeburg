#include "node.hpp"

// TODO: streaming operator implementation

std::ostream& operator << (std::ostream &lhs, const Node &rhs) {
    lhs << rhs.toString();
    return lhs;
}
