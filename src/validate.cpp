#include <vector>
#include <Rcpp.h>
#include "validate.h"

// TODO: add validateParent
// TODO: add R export

bool ValidateChildren(const std::vector< std::vector<unsigned int> > &children)
{
  // This function checks if children is valide or not
  // Assumes:
  //   indices are zero-based
  //
  // Check if:
  //  (a) pointed IDs are within the scope
  //  (b) no index has two or more parents
  //
  // Returns:
  //   bool

  unsigned int n = children.size();
  std::vector<bool> parentIdentified(n);
  for (unsigned int i = 0; i < n; i++) parentIdentified[i] = false;

  unsigned int childId;
  for (unsigned int i = 0; i < children.size(); i++)
  {
    for (unsigned int j = 0; j < children[i].size(); j++)
    {
      childId = children[i][j];
      if (childId >= n) {
        // id is out of bounds
        Rcpp::Rcout << "ID " << childId << " is out of bounds";
        return false;
      }
      if (parentIdentified[childId]) {
        // this children appears twice
        Rcpp::Rcout << "ID " << childId << " appears more than twice";
        return false;
      }
      parentIdentified[childId] = true;
    }
  }

  // no problem found
  return true;
}

