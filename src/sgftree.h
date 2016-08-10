#include <Rcpp.h>
#include <vector>
#include <string>
#include "tree.h"


struct Move
{
  // this structure stores game plays and setups
  unsigned int color;
  unsigned int x;
  unsigned int y;
  bool ismove;
  Move() {}
  Move(unsigned int cc, unsigned int xx, unsigned int yy, bool bb)
  {
    color = cc;
    x = xx;
    y = yy;
    ismove = bb;
  }
};


struct GoNode
{
  // stores events occuring at a node of go game
  std::string comment;
  std::vector<Move> moves;
};


// parse an entire SGF into a tree of game nodes
// sgf2tree is the main interface to parse SGF text into a node tree
// ParseBranch is a recursive function to be called from sgf2tree
// ParseSgf is parse text into a node object form SGF of a single node
Tree<GoNode> sgf2tree(const std::string &sgf);
void ParseSgfBranch(const std::string &sgf, unsigned int &index,
                 int parentid, Tree<GoNode> &out);
GoNode ParseSgfNode(const std::string &sgf);




Tree<std::string> get_sgftree(const std::string &sgf);
