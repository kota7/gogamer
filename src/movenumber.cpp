#include <vector>
#include "Rcpp.h"
#include "validate.h"


std::vector<int> get_movenumber(
    std::vector<bool> hasmove,
    std::vector< std::vector<unsigned int> > children,
    bool onebased);
void GetMoveNumberRecursive(
    int currentNode, int parentNumber, std::vector<int> &out,
    const std::vector<bool> &hasmove,
    const std::vector< std::vector<unsigned int> > &children);


//[[Rcpp::export]]
std::vector<int> get_movenumber(
    std::vector<bool> hasmove,
    std::vector< std::vector<unsigned int> > children,
    bool onebased = true)
{
  // Obtain move number for each node
  //
  // Assumes:
  //   Each node has at most one move
  //
  // Args:
  //   hasmove : bool vector. indicates if each node has a move or not
  //   children: vector of int vector. indicates the set of children nodes
  //
  // Returns:
  //   int vector of same size as hasmove.

  int n = hasmove.size();

  // make children indices zeo-based, if needed
  if (onebased) {
    for (unsigned int i = 0; i < children.size(); i++)
      for (unsigned int j = 0; j < children[i].size(); j++)
        children[i][j]--;
  }

  bool check = ValidateChildren(children);
  if (!check) Rcpp::stop("Invalid 'children' input");


  std::vector<int> out(n);
  GetMoveNumberRecursive(0, 0, out, hasmove, children);

  return out;
}


void GetMoveNumberRecursive(
    int currentNode, int parentNumber, std::vector<int> &out,
    const std::vector<bool> &hasmove,
    const std::vector< std::vector<unsigned int> > &children)
{
  if (hasmove[currentNode]) parentNumber++;
  out[currentNode] = parentNumber;

  for (unsigned int i = 0; i < children[currentNode].size(); i++)
    GetMoveNumberRecursive(
      children[currentNode][i], parentNumber, out, hasmove, children);
}
