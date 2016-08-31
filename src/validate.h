#include <vector>
#include <Rcpp.h>

bool ValidateChildren(const std::vector< std::vector<unsigned int> > &children);
bool ValidateParent(const std::vector<int> &parent);

bool validate_children(std::vector< std::vector<int> > children);
bool validate_parent(std::vector<int> parent);
