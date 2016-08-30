#include <vector>
#include <Rcpp.h>
#include "validate.h"



Rcpp::List tree_compressor(std::vector< std::vector<unsigned int> > children);
void AssignNewIndex(unsigned int currentNode, int &currentIndex,
                    const std::vector< std::vector<unsigned int> > &children,
                    std::vector<int> &newIndex);



//[[Rcpp::export]]
Rcpp::List tree_compressor(std::vector< std::vector<unsigned int> > children,
                           bool onebased = true)
{
  // Returns information to compress tree
  //
  // Args:
  //   children: a list of integer vectors of children pointers
  //   onebased: bool indicating if the children is given by one-base or zero-base
  //             default is true
  //
  // Returns:
  //   List containing
  //     indices : a list of integer vectors that specifies the index of new data
  //     parent  : an integer vector of new parent
  //     children: a list of integer vectors of new children

  std::vector< std::vector<int> > newIndices;
  std::vector<int> newParent;
  std::vector< std::vector<int> > newChildren;
  std::vector<int> newLeaf;

  // convert to zeo-based indices
  if (onebased) {
    for (unsigned int i = 0; i < children.size(); i++)
      for (unsigned int j = 0; j < children[i].size(); j++)
        children[i][j]--;
  }

  bool check = ValidateChildren(children);
  if (!check) Rcpp::stop("Invalid 'children' input");


  // make a list that maps current id to new id
  // note the difference from the newIndices, which
  // map the new id to current id
  // assume the first entry is the top node
  std::vector<int> newIndex(children.size());
  newIndex[0] = 0;
  int maxID = 0;
  AssignNewIndex(0, maxID, children, newIndex);


  // compile newIndices newParent, newChildren
  int newN = maxID + 1;
  newIndices.resize(newN);
  newParent.resize(newN);
  newChildren.resize(newN);
  unsigned int index;
  unsigned int childID;
  for (unsigned int i = 0; i < newIndex.size(); i++)
  {
    index = newIndex[i];
    newIndices[index].push_back(i + 1);
    for (unsigned int j = 0; j < children[i].size(); j++)
    {
      childID = newIndex[children[i][j]]; // new child ID
      if (index != childID) {
        // index may equal to childID, since the two may have been combined
        // if they differ, then they are parernt and child
        newChildren[index].push_back(childID + 1);
        newParent[childID] = index + 1;
      }
    }
  }
  // first entry is the root
  newParent[0] = 0;

  // compile newleaf
  for (unsigned int i = 0; i < newN; i++)
    if (newChildren[i].size() == 0) newLeaf.push_back(i + 1);

  // compile newIndex
  for (unsigned int i = 0; i < newIndex.size(); i++) newIndex[i]++;

  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("indices") = newIndices,
    Rcpp::Named("indexmap") = newIndex,
    Rcpp::Named("parent") = newParent,
    Rcpp::Named("children") = newChildren,
    Rcpp::Named("leaf") = newLeaf
  );
  return out;
}


void AssignNewIndex(unsigned int currentNode, int &currentIndex,
                    const std::vector< std::vector<unsigned int> > &children,
                    std::vector<int> &newIndex)
{
  // Assign new index recursively
  //
  // if a node has a unique child, then give it the same index
  // if multile children, then give them different indices for each

  unsigned int nextNode;
  if (children[currentNode].size() == 1) {
    nextNode = children[currentNode][0];
    newIndex[nextNode] = newIndex[currentNode];
    AssignNewIndex(nextNode, currentIndex, children, newIndex);
  } else if (children[currentNode].size() > 1) {
    for (unsigned int j = 0; j < children[currentNode].size(); j++)
    {
      currentIndex++;
      nextNode = children[currentNode][j];
      newIndex[nextNode] = currentIndex;
      AssignNewIndex(nextNode, currentIndex, children, newIndex);
    }
  }
}




/*** R
children <- list(2, c(3,6), 4, 5, 10, c(7,9), 8, integer(0), integer(0), integer(0))
gogamer:::tree_compressor(children)
*/
