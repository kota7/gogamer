#include <vector>

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
  std::vector<unsigned int> parent;
  std::vector< std::vector<unsigned int> > children;
  std::vector<bool> isleaf;

public:
  Tree() { nodecount = 0; }

  // AddNode ... add a new node x, whose parent is p
  //             p = -1 if x is the first node
  void AddNode(T x, unsigned int p);


};




template <class T>
void Tree<T>::AddNode(T x, unsigned int p)
{
  // TODO: assert p < nodecount

  // x is a new node, and its parent index is p
  data.push_back(x);
  parent.push_back(p);
  nodecount++;

  // don't forget to mark x is p's child
  children[p].push_back(nodecount-1);
  // x's children is an empty set
  children.resize(nodecount);
  // hence it is a leaf for now
  isleaf[nodecount-1] = true;
  // but p is not any more
  isleaf[p] = false;
}




