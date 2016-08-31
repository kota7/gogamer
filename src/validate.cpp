#include <vector>
#include <stack>
#include <Rcpp.h>
#include "validate.h"


bool ValidateChildren(const std::vector< std::vector<unsigned int> > &children)
{
  // checks if children pointer implies valid tree
  // Assumes:
  //   indices are zero-based
  //
  // Check if:
  //  (a) all IDs are within the range
  //  (b) there is only one path to reach each node from the root (id = 0),
  //  (c) all nodes are reacheable
  //
  // strategy:
  //   depth-first search
  //
  // Args:
  //   children: vector of integer vectors
  //
  // Returns:
  //   bool

  unsigned int n = children.size();
  if (n == 0) {
    Rcpp::Rcout << "there is no tree\n";
    return false;
  }
  std::vector<bool> visited(n);
  unsigned int visit_count = 0;
  for (unsigned int i = 0; i < n; i++) visited[i] = false;

  std::stack<unsigned int> nodeList;
  nodeList.push(0);
  unsigned int currentNode;
  while (!nodeList.empty())
  {
    currentNode = nodeList.top();
    nodeList.pop();
    if (currentNode >= n) {
      Rcpp::Rcout << "nodeid " << currentNode << " is out of bounds\n";
      return false;
    }
    if (visited[currentNode]) {
      Rcpp::Rcout << "multiple path to reach " << currentNode << "\n";
      return false;
    }

    visited[currentNode] = true;
    visit_count++;
    for (unsigned int j = 0; j < children[currentNode].size(); j++)
      nodeList.push(children[currentNode][j]);
  }
  if (visit_count != n) {
    Rcpp::Rcout << "only " << visit_count << " out of " << n <<
      " nodes are reacheable\n";
    return false;
  }

  // no problem found
  return true;
}



bool ValidateParent(const std::vector<int> &parent)
{
  // This function detects problems in parent vector
  // Assumes:
  //   indices are zero-based
  //
  // check if:
  //   (a) all indices are within the range
  //   (b) there is no cycle
  //
  // strategy:
  //   check the validity of corresponding children pointer
  //
  // Args:
  //   parent: integer vector
  //
  // returns:
  //   bool
  int n = parent.size();
  if (n == 0) {
    Rcpp::Rcout << "there is no tree\n";
    return false;
  }
  if (parent[0] >= 0) {
    Rcpp::Rcout << "first node must be the root\n";
    return false;
  }

  std::vector< std::vector<unsigned int> > children(n);
  for (unsigned int i = 0; i < parent.size(); i++)
  {
    if (parent[i] >= n) {
      Rcpp::Rcout << "nodeid " << parent[i] << " is out of bounds\n";
      return false;
    }
    if (parent[i] >= 0) children[parent[i]].push_back(i);
  }

  return ValidateChildren(children);
}


//[[Rcpp::export]]
bool validate_children(std::vector< std::vector<int> > children)
{
  // children may potentially contain negative integers, which would
  // be found invalid.
  // ValidateChildren only accepts unsigned int
  std::vector<std::vector<unsigned int> > newChildren(children.size());
  // convert to zero-based
  for (unsigned int i = 0; i < children.size(); i++)
  {
    newChildren[i].resize(children[i].size());
    for (unsigned int j = 0; j < children[i].size(); j++)
    {
      if (children[i][j] < 1) {
        Rcpp::Rcout << "index must be larger than zero\n";
        return false;
      }
      newChildren[i][j] = children[i][j] - 1;
    }
  }

  return ValidateChildren(newChildren);
}


//[[Rcpp::export]]
bool validate_parent(std::vector<int> parent)
{
  // convert to zero-based
  for (unsigned int i = 0; i < parent.size(); i++)
    parent[i]--;

  return ValidateParent(parent);
}


/***R
gogamer:::validate_children(list(integer(0)))
gogamer:::validate_children(list(2:3, integer(0), 4:5, integer(0), integer(0)))
gogamer:::validate_children(list(2:3, integer(0), 4:5, integer(0)))
gogamer:::validate_children(list(2:3, integer(0), 1, integer(0)))
gogamer:::validate_children(list(2:3, 0, 4:5, integer(0)))
gogamer:::validate_children(list(2:3, integer(0), integer(0), integer(0)))
gogamer:::validate_children(list())

gogamer:::validate_parent(c(0, 1, 1, 3, 3))
gogamer:::validate_parent(0)
gogamer:::validate_parent(c(2, 0, 1, 3, 3))
gogamer:::validate_parent(c(0, 1, 1, 0, 3))
gogamer:::validate_parent(c(0, 1, 1, 3, -1))
gogamer:::validate_parent(c(0, 1, 1, 3, 6))
gogamer:::validate_parent(integer(0))
*/

