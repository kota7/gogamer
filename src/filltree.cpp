#include <Rcpp.h>


//[[Rcpp::export]]
std::vector<int> children_to_parentC(std::vector< std::vector<int> > children)
{
  std::vector<int> out(children.size());
  if (out.size() == 0) return out;

  for (unsigned int i = 0; i < children.size(); i++)
  {
    for (unsigned int j = 0; j < children[i].size(); j++)
    {
      if (children[i][j] > (int)out.size() || children[i][j] < 1)
        Rcpp::stop("index out of bounds, (@children_to_parent)");
      out[children[i][j]-1] = i + 1; // '-1' and '+1' to make it one-based
    }
  }
  out[0] = 0;
  return out;
}



//[[Rcpp::export]]
std::vector< std::vector<int> > parent_to_childrenC(std::vector<int> parent)
{
  std::vector< std::vector<int> > out(parent.size());
  if (out.size() == 0) return out;

  for (unsigned int i = 0; i < parent.size(); i++)
  {
     if (parent[i] < 0 || parent[i] > (int)out.size())
       Rcpp::stop("index out of bounds, (@parent_to_children)");
     if (parent[i] >= 1) // index zero, which indicates the root node
       out[parent[i]-1].push_back(i + 1);
  }
  return out;
}
