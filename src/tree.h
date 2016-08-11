#include <vector>
#include <Rcpp.h>

template <class T>
class Tree
{
  // generic tree class
  // data is a vector of nodes
  // parent and children store the index of parent and children nodes
  // isleaf indicates if each node is a leaf, that is, has no child.
  // Hence data, parent, children and isleaf are of the same size
  // There are as many pathes as the number of leaves in the tree.
  // One can recover a path by back traversing from a leave
  unsigned int nodecount;
  std::vector<T> data;
  std::vector<int> parent;
  std::vector< std::vector<unsigned int> > children;
  std::vector<bool> isleaf;

public:
  Tree() { nodecount = 0; }

  // AddNode ... add a new node x, whose parent is p
  //             p = -1 if x is the first node
  void AddNode(T x, int p);

  // access private members
  // TODO?: assert i < nodecount
  int GetParent(unsigned int i) const { return parent[i]; }
  T Get(unsigned int i) const { return data[i]; }
  std::vector<unsigned int> GetChildren(unsigned int i) const
    { return children[i]; }
  bool IsLeaf(unsigned int i) const { return isleaf[i]; }
  unsigned int size() const { return nodecount; }

  // make a List object
  // Since this is a friend function, it can access to private members
  // This function is defined by type since some data type may not be
  // convertible to R object
  friend Rcpp::List TreeToList(Tree<std::string> t);
};




template <class T>
void Tree<T>::AddNode(T x, int p)
{
  // TODO: assert p < nodecount

  // x is a new node, and its parent index is p
  // since it is new, it is a leaf for now
  data.push_back(x);
  parent.push_back(p);
  isleaf.push_back(true);
  nodecount++;
  // x's children is an empty set
  children.resize(nodecount);


  // if not p is non-negative (this id is the root)
  // mark x is p's child
  if (p >= 0) {
    children[p].push_back(nodecount-1);
    // p is not a leaf any more
    isleaf[p] = false;
  }
}


// parse a string tree into a List object
// indices are converted to one-based
// leaf is computed as a vector of leaf indices
Rcpp::List TreeToList(Tree<std::string> t)
{
  std::vector<int> leafid;
  std::vector<int> parent(t.size());
  std::vector< std::vector<int> > children(t.size());
  for (unsigned int i = 0; i < t.size(); i++)
  {
    if (t.isleaf[i]) leafid.push_back(i + 1);

    parent[i] = t.parent[i] + 1;
    children[i].resize(t.children[i].size());
    for (unsigned int j = 0; j < t.children[i].size(); j++)
      children[i][j] = t.children[i][j] + 1;
  }


  Rcpp::List out = Rcpp::List::create(
    Rcpp::Named("data")     = t.data,
    Rcpp::Named("parent")   = parent,
    Rcpp::Named("children") = children,
    Rcpp::Named("leaf")     = leafid
  );
  return out;
}

