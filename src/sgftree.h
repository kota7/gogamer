#ifndef SGFTREEHEADERDEF
#define SGFTREEHEADERDEF


#include <Rcpp.h>
#include <vector>
#include <string>
#include "tree.h"




// parse an SGF text into a tree of game nodes
// ParseBranch is a recursive function to be called by MakeSgfNodeTree
Tree<std::string> MakeSgfNodeTree(const std::string &sgf);
void ParseSgfBranch(const std::string &sgf, unsigned int &index,
                 int parentid, Tree<std::string> &out);

// parse an SGF text into a tree of branches
Tree<std::string> MakeSgfBranchTree(const std::string &sgf);




// interacting with R
Rcpp::List make_nodetree(std::string sgf, bool bynode = true);


#endif
