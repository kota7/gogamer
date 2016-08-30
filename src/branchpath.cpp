#include <vector>
#include <algorithm>
#include <Rcpp.h>

std::vector<int> get_branchpath(
    std::vector<int> parent, int goal, bool onebased);
void GetBranchPathRecursive(
    int currentPosition, std::vector<int> &out, const std::vector<int> &parent);


//[[Rcpp::export]]
std::vector<int> get_branchpath(
    std::vector<int> parent, int goal, bool onebased = true)
{
  // find the path that goes gown to the goal
  //
  // Args:
  //   Parent  : integer vector that point the parent index
  //   goal    : integer of goal index
  //   onebased: bool, indicating if indices are zero based or onebased
  //
  // Returns:
  //   integer vector
  if (onebased) {
    for (unsigned int i = 0; i < parent.size(); i++) parent[i]--;
    goal--;
  }

  std::vector<int> out;
  GetBranchPathRecursive(goal, out, parent);

  std::reverse(out.begin(), out.end());
  for (unsigned int i = 0; i < out.size(); i++) out[i]++;

  return out;
}


void GetBranchPathRecursive(
    int currentPosition, std::vector<int> &out, const std::vector<int> &parent)
{
  if (currentPosition < 0) return;
  out.push_back(currentPosition);
  GetBranchPathRecursive(parent[currentPosition], out, parent);
}

/*** R
gogamer:::get_branchpath(c(0,1,1,3,3), 2)
gogamer:::get_branchpath(c(0,1,1,3,3), 5)
gogamer:::get_branchpath(c(0,1,1,3,3), 4)
*/
