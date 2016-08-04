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
  // TODO: assert i < nodecount
  int GetParent(unsigned int i) const { return parent[i]; }
  T Get(unsigned int i) const { return data[i]; }
  std::vector<unsigned int> GetChildren(unsigned int i) const
    { return children[i]; }
  bool IsLeaf(unsigned int i) const { return isleaf[i]; }
  unsigned int size() const { return nodecount; }
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




